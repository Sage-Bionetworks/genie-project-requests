library(genieBPC)
library(lobstr)
library(magrittr)
library(dplyr)
library(sunburstR)

# need to use set_synapse_credentials(), which only supports username and password unfortunately.
# Never set credentials in a tracked script, use the command line.

nsclc <- pull_data_synapse("NSCLC", version = "v2.0-public")

str(data.frame(a = 1:3))
# Can also use things like class(), names(), etc.
lobstr::tree(data.frame(a = 1:3))

lobstr::tree(nsclc, max_depth = 2)


nsclc_stg_iv <- create_analytic_cohort(
    data_synapse = nsclc$NSCLC_v2.0,
    stage = c("Stage IV")
)
lobstr::tree(nsclc_stg_iv, max_depth = 1)
# Similar structure, but different names and classes.

sunplot <- drug_regimen_sunburst(
    data_synapse = nsclc$NSCLC_v2.0,
    data_cohort = nsclc_stg_iv,
    max_n_regimens = 3
)
# We can see there's two things returned:
lobstr::tree(sunplot, max_depth = 2)

treat_hist_df <- sunplot$treatment_history
# The treatment history dataframe is actually very simple.  It's just all the regimen orders (concatenated by dashes) and the number of people who did that sequence:
glimpse(treat_hist_df)
print(treat_hist_df, rows = 500)

# The rest of the work to get a sunburst plot is done by other code:
sunburstR::sunburst(
    data = treat_hist_df
)

# So if you can get to the treatment history table on your own (not hard), you can lose the reliance on {genieBPC} for sunburst plots.








# also works:
test <- pull_data_synapse("PANC", version = "v1.2-consortium")