library(dplyr)
library(readr)
library(purrr) # for discard()

fac <- read_csv("datasets/locationfacilitydata_filtered.csv", show_col_types = FALSE)

all_facilities <- fac %>%
  pull(FACILITIES) %>%
  strsplit("\\|") %>%
  unlist() %>%
  trimws() %>%
  purrr::discard(~ .x == "" | is.na(.x)) %>%
  unique() %>%
  sort()

print(all_facilities)
