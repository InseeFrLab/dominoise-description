# Chargé au début de chaque chapitre : bibliothèques, options, données.
suppressPackageStartupMessages({
  library(dominoise)
  library(dplyr)
  library(ggplot2)
  library(purrr)
})

theme_set(theme_minimal(base_size = 12))
options(digits = 4, pillar.sigfig = 4)

# Les trois tableaux de démonstration, construits par data-raw/build_data.R
tables <- readRDS(here::here("data", "tables_demo.rds"))

# Version du package sur laquelle le document est construit
version_dominoise <- as.character(utils::packageVersion("dominoise"))
