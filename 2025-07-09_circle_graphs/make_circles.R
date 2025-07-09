library(tidyverse)
library(ggforce)
library(ggrepel)

coh_dat <- readr::read_rds('cohort_data.rds')

# Specify some positions for these:
pos_addon <- tribble(
  ~name,
  ~x_pos,
  ~y_pos,

  "GENIE",
  0,
  0,

  "BPC public",
  0.75,
  0,

  "BPC consortium",
  0.75,
  0,

  "Sponsored projects",
  0.1,
  -0.6,

  "Special projects",
  -0.1,
  -0.6,
)

coh_dat <- left_join(
  coh_dat,
  pos_addon,
  by = "name",
  relationship = "one-to-one"
)


coh_dat <- coh_dat %>%
  mutate(
    n_prop = n / max(n),
    n_rad = sqrt(n_prop)
  )

coh_dat <- coh_dat |>
  mutate(name = fct_inorder(name))


gg <- ggplot(coh_dat) +
  ggforce::geom_circle(
    aes(fill = color, r = n_rad, x0 = x_pos, y0 = y_pos)
  ) +
  geom_text_repel(
    aes(label = lab, x = x_pos, y = y_pos),
    color = 'black',
    vjust = 0,
    hjust = 1,
    size = 3,
    show.legend = FALSE
  ) +
  scale_fill_identity() +
  coord_fixed() +
  theme_void() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
