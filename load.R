library(tidyverse)
library(lubridate)
library(janitor)

library(helpers)

responses <- tibble(path = fs::dir_ls("data/source/open.canada.ca/2e4a1eb3-82ba-4a14-a5a3-f40bc11660bf/", regexp = "\\.csv$")) %>%
  pull(path) %>%
  map_dfr(
    read_csv,
    .id = "subset",
    col_types = cols(
      LEVEL1ID = col_character(),
      LEVEL2ID = col_character(),
      LEVEL3ID = col_character(),
      LEVEL4ID = col_character(),
      LEVEL5ID = col_character(),
      SURVEYR = col_double(),
      BYCOND = col_character(),
      DESCRIP_E = col_character(),
      DESCRIP_F = col_character(),
      DEMCODE = col_double(),
      QUESTION = col_character(),
      TITLE_E = col_character(),
      TITLE_F = col_character(),
      ANSWER1 = col_double(),
      ANSWER2 = col_double(),
      ANSWER3 = col_double(),
      ANSWER4 = col_double(),
      ANSWER5 = col_double(),
      ANSWER6 = col_double(),
      ANSWER7 = col_double(),
      POSITIVE = col_double(),
      NEUTRAL = col_double(),
      NEGATIVE = col_double(),
      AGREE = col_double(),
      SCORE5 = col_double(),
      SCORE100 = col_double(),
      ANSCOUNT = col_double(),
      DEPT_E = col_character(),
      DEPT_F = col_character(),
      INDICATORID = col_double(),
      INDICATORENG = col_character(),
      INDICATORFRA = col_character(),
      SUBINDICATORID = col_double(),
      SUBINDICATORENG = col_character(),
      SUBINDICATORFRA = col_character()
    ),
    locale = readr::locale(encoding = "ISO-8859-1")
  ) %>%
  separate(BYCOND, into = c("BYCOND_CATEGORY", "BYCOND_VALUE"), sep = fixed(" = "), remove = FALSE, convert = TRUE)

## TODO: replace 9999 with NA

response_count_2019 <- 182306
response_rate_2019 <- 0.623
population_2019 <- response_count_2019 / response_rate_2019

pop_by_department <- read_csv("data/source/open.canada.ca/a68c6586-8ab0-4e76-b54e-cbdbd6183c2a/ssa-pop-eng.csv") %>%
  clean_names()

library(readxl)
library(naniar)
organizational_units <- read_excel(
    "data/source/open.canada.ca/2e4a1eb3-82ba-4a14-a5a3-f40bc11660bf/2019_PSES_Supporting_Documentation_Document_de_reference_du_SAFF_2019.xlsx",
    sheet = "LEVEL2ID_LEVEL5ID"
  ) %>%
  replace_with_na_at(
    c("LEVEL1ID", "LEVEL2ID", "LEVEL3ID", "LEVEL4ID", "LEVEL5ID"),
    ~ .x == "000"
  )









