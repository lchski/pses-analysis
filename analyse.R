
highest_answered_questions_2019 <- responses %>%
  filter(SURVEYR == 2019) %>%
  mutate(
    response_count_prop = ANSCOUNT / response_count_2019
  ) %>%
  arrange(-response_count_prop) %>%
  slice(1:10) %>%
  select(QUESTION) %>%
  distinct()

## see all responses to Q11 for 2019, by subset (file) and BYCOND_CATEGORY
responses %>%
  filter(SURVEYR == 2019) %>%
  filter(TITLE_E == "Question 11. Overall, I feel valued at work.") %>%
  group_by(subset, BYCOND_CATEGORY) %>%
  summarize(responses = sum(ANSCOUNT, na.rm = TRUE)) %>%
  View("2019q11")

### with extra detail of BYCOND_VALUE
responses %>%
  filter(SURVEYR == 2019) %>%
  filter(TITLE_E == "Question 11. Overall, I feel valued at work.") %>%
  group_by(subset, BYCOND_CATEGORY, BYCOND_VALUE, DESCRIP_E) %>%
  summarize(responses = sum(ANSCOUNT, na.rm = TRUE)) %>%
  View("2019q11_detailed")



## ECs by department (note one DEPT_E is "Public Service") (by responses to Q11)
responses %>%
  filter(SURVEYR == 2019) %>%
  filter(TITLE_E == "Question 11. Overall, I feel valued at work.") %>%
  filter(BYCOND_CATEGORY == "Q94A" & DESCRIP_E == "EC") %>% remove_extra_columns() %>% summarize(sum(ANSCOUNT, na.rm = TRUE))

## Number of responses to Q11 by occupational group
responses %>%
  filter(SURVEYR == 2019) %>%
  filter(TITLE_E == "Question 11. Overall, I feel valued at work.") %>%
  filter(DEPT_E == "Public Service") %>%
  filter(BYCOND_CATEGORY == "Q94A") %>%
  select(DESCRIP_E, ANSCOUNT) %>%
  mutate(prop = ANSCOUNT / sum(ANSCOUNT, na.rm = TRUE))

count_by_occupational_group_for_top_10 <- responses %>%
  filter(SURVEYR == 2019) %>%
  filter(QUESTION %in% highest_answered_questions_2019$QUESTION) %>%
  filter(DEPT_E == "Public Service") %>%
  filter(BYCOND_CATEGORY == "Q94A") %>%
  group_by(QUESTION) %>%
  select(DESCRIP_E, ANSCOUNT) %>%
  mutate(prop = ANSCOUNT / sum(ANSCOUNT, na.rm = TRUE))

count_by_occupational_group_for_top_10 %>%
  ungroup() %>%
  group_by(DESCRIP_E) %>%
  skim(ANSCOUNT)

count_by_occupational_group_for_top_10 %>%
  ungroup() %>%
  group_by(DESCRIP_E) %>%
  skim(ANSCOUNT) %>%
  yank("numeric") %>%
  as_tibble() %>%
  arrange(sd) %>%
  View()

count_by_occupational_group_for_top_10 %>%
  ungroup() %>%
  group_by(DESCRIP_E) %>%
  summarize(max_responses = max(ANSCOUNT)) %>%
  mutate(prop = max_responses / sum(max_responses, na.rm = TRUE)) %>%
  mutate(prop_rounded = round(prop * 100, 2)) %>%
  mutate(est_pop = round(population_2019 * prop)) %>%
  select(-prop) %>%
  arrange(-est_pop) %>%
  write_csv("data/out/2019-estimated-occupational-group-size.csv")

count_by_occupational_group_for_top_10 %>%
  ungroup() %>%
  group_by(DESCRIP_E) %>%
  summarize(total = mean(ANSCOUNT, na.rm = TRUE))

responses %>%
  filter(SURVEYR == 2019) %>%
  mutate(
    response_count_prop = ANSCOUNT / response_count_2019,
    residual = abs(ANSCOUNT - response_count_2019)
  ) %>%
  View()



