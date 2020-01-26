library(tidyverse)
library(lubridate)

library(helpers)

responses <- tibble(path = fs::dir_ls("data/source/", regexp = "\\.csv$")) %>%
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
    )
  ) %>%
  separate(BYCOND, into = c("BYCOND_CATEGORY", "BYCOND_VALUE"), sep = fixed(" = "), remove = FALSE, convert = TRUE)

response_count_2019 <- 182306
response_rate_2019 <- 0.623
population_2019 <- response_count_2019 / response_rate_2019
