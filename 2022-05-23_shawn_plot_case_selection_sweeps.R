# Description: Plot case selection sweeps to visualize the number of eligible patients
#   by minimum sequencing date.
# Author: Haley Hunter-Zinck
# Date: 2022-05-23

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(RColorBrewer)
library(yaml)
library(reshape2)
library(ggplot2)
library(dplyr)
library(optparse)
library(synapser)

# constants
url <- "https://raw.githubusercontent.com/Sage-Bionetworks/genie-bpc-pipeline/gh-66-phase2-case-selection/scripts/case_selection/config.yaml"
file_config <- "config.yaml"
phase <- "2"

waitifnot <- function(cond, msg) {
  if (!cond) {
    
    for (str in msg) {
      message(str)
    }
    message("Press control-C to exit and try again.")
    
    while(T) {}
  }
}

# user input ----------------------------

option_list <- list( 
  make_option(c("-i", "--synid_file_input"), type = "character",
              help="Synapse ID of input file"),
  make_option(c("-o", "--synid_folder_output"), type = "character",
              help="Synapse ID of output folder"),
  make_option(c("-v", "--verbose"), action="store_true", default = FALSE, 
              help="Output script messages to the user.")
)
opt <- parse_args(OptionParser(option_list=option_list))
waitifnot(!is.null(opt$synid_file_input) && !is.null(opt$synid_folder_output),
          msg = "Rscript template.R -h")

synid_file_input <- opt$synid_file_input
synid_folder_output <- opt$synid_folder_output
verbose <- opt$verbose

# functions ----------------------------

#' Get the name of a Synapse entity. 
#' 
#' @param synapse_id Synapse ID string
#' @return String representing entity name
#' @example get_synapse_entity_name("syn12345")
get_synapse_entity_name <- function(synapse_id) {
  return(synGet(synapse_id, downloadFile = F)$properties$name)
}

#' Download and load data stored in csv or other delimited format on Synapse
#' into an R data frame.
#' 
#' @param synapse_id Synapse ID
#' @version Version of the Synapse entity to download.  NA will load current
#' version
#' @param set Delimiter for file
#' @param na.strings Vector of strings to be read in as NA values
#' @param header TRUE if the file contains a header row; FALSE otherwise.
#' @param check_names TRUE if column names should be modified for compatibility 
#' with R upon reading; FALSE otherwise.
#' @param comment.char character designating comment lines to ignore
#' @return data frame
get_synapse_entity_data_in_csv <- function(synapse_id, 
                                           version = NA,
                                           sep = ",", 
                                           na.strings = c("NA"), 
                                           header = T,
                                           check_names = F,
                                           comment.char = "#",
                                           colClasses = "character",
                                           row.names = NULL) {
  
  if (is.na(version)) {
    entity <- synGet(synapse_id)
  } else {
    entity <- synGet(synapse_id, version = version)
  }
  
  data <- read.csv(entity$path, stringsAsFactors = F, 
                   na.strings = na.strings, sep = sep, check.names = check_names,
                   header = header, comment.char = comment.char, colClasses = colClasses,
                   row.names = row.names)
  return(data)
}

#' Store a file on Synapse with options to define provenance.
#' 
#' @param path Path to the file on the local machine.
#' @param parent_id Synapse ID of the folder or project to which to load the file.
#' @param file_name Name of the Synapse entity once loaded
#' @param prov_name Provenance short description title
#' @param prov_desc Provenance long description
#' @param prov_used Vector of Synapse IDs of data used to create the current
#' file to be loaded.
#' @param prov_exec String representing URL to script used to create the file.
#' @return Synapse ID of entity representing file
save_to_synapse <- function(path, 
                            parent_id, 
                            file_name = NA, 
                            prov_name = NA, 
                            prov_desc = NA, 
                            prov_used = NA, 
                            prov_exec = NA) {
  
  if (is.na(file_name)) {
    file_name = path
  } 
  file <- File(path = path, parentId = parent_id, name = file_name)
  
  if (!is.na(prov_name) || !is.na(prov_desc) || !is.na(prov_used) || !is.na(prov_exec)) {
    act <- Activity(name = prov_name,
                    description = prov_desc,
                    used = prov_used,
                    executed = prov_exec)
    file <- synStore(file, activity = act)
  } else {
    file <- synStore(file)
  }
  
  return(file$properties$id)
}

get_production_target <- function(config, phase, cohort, site) {
  return(config$phase[[phase]]$cohort[[cohort]]$site[[site]]$production)
}

# synapse login --------------------

status <- synLogin()

# read ----------------------------

df_raw <- get_synapse_entity_data_in_csv(synid_file_input, 
                                           row.names = "")
df_sweep <- sapply(df_sweep, as.integer)
rownames(df_sweep) <- rownames(df_raw)

filename <- get_synapse_entity_name(synid_file_input)
cohort <- toupper(gsub(pattern = ".csv", replacement = "", fixed = T, 
                       x = gsub(pattern = "case_selection_sweep_2_", replacement = "", x = filename)))

status <- download.file(url, destfile = file_config, method = "wget")
config <- read_yaml(file_config)

# main ----------------------------

pal <- "Dark2"
ylimits <- c(0, max(df_sweep))
colors <- setNames(brewer.pal(n = ncol(df_sweep), name = pal), colnames(df_sweep))

# plot raw counts
for (j in 1:ncol(df_sweep)) {
  
  site <- colnames(df_sweep)[j]
  n_prod <- get_production_target(config, phase, cohort, site)
  
  if (j == 1) {
    plot(df_sweep[,j], 
         xlab = "Minimum sequencing date", 
         ylab = "Number of eligible cases",
         col = colors[site], type = "l", 
         xaxt = "n",
         ylim = ylimits,
         main = cohort)
    axis(side = 1, at = c(1:nrow(df_sweep)), labels = rownames(df_sweep))
  } else {
    points(df_sweep[,j], col = colors[site], type = "l")
  }
  abline(h = n_prod, col = colors[site], lty = 2)
}
legend(x = "topright", legend = colnames(df_sweep), col = colors, lwd = 3)

# plot percentage to target counts
for (j in 1:ncol(df_sweep)) {
  
  site <- colnames(df_sweep)[j]
  n_prod <- get_production_target(config, phase, cohort, site)
  frac_target <- df_sweep[,j] / n_prod
  frac_target[which(frac_target > 1)] <- 1
  
  if (j == 1) {
    plot(frac_target, 
         xlab = "Minimum sequencing date", 
         ylab = "Fraction of target eligible cases",
         col = colors[site], type = "l", 
         xaxt = "n",
         ylim = c(0,1),
         main = cohort)
    axis(side = 1, at = c(1:nrow(df_sweep)), labels = rownames(df_sweep))
  } else {
    points(frac_target, col = colors[site], type = "l")
  }
}
legend(x = "bottomleft", legend = colnames(df_sweep), col = colors, lwd = 3)

df_plot <- reshape2::melt(df_sweep) 
colnames(df_plot) <- c("min_seq_date", "site", "n_eligible")
ggplot(data=df_plot, aes(x=min_seq_date, y=n_eligible, group=site, color = site)) +
  geom_line() +
  ylab("Number of eligible cases") +
  xlab("Minimum sequencing date") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks = grep(pattern = "Jan", x = df_plot$min_seq_date, value = T)) +
  ggtitle(cohort)


# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
