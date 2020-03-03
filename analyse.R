
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



## CS planning to retire in next two years
responses %>%
  filter(SURVEYR == 2019) %>%
  filter(QUESTION == "Q55") %>%
  filter(BYCOND_CATEGORY == "Q94A" & DESCRIP_E == "CS") %>%
  select(DEPT_E, ANSWER1, ANSCOUNT)




count_by_occupational_group_for_top_10_tbs <- responses %>%
  filter(SURVEYR == 2019) %>%
  filter(QUESTION %in% highest_answered_questions_2019$QUESTION) %>%
  filter(DEPT_E == "Treasury Board of Canada Secretariat") %>%
  filter(BYCOND_CATEGORY == "OCCLEVEL") %>%
  group_by(QUESTION) %>%
  select(DESCRIP_E, ANSCOUNT) %>%
  mutate(prop = ANSCOUNT / sum(ANSCOUNT, na.rm = TRUE))

## get max number of responses by occupational group at TBS
responses_by_occ_group_tbs <- count_by_occupational_group_for_top_10_tbs %>%
  ungroup() %>%
  group_by(DESCRIP_E) %>%
  summarize(responses_max = max(ANSCOUNT, na.rm = TRUE)) %>%
  filter(responses_max > 0) %>%
  rename(occ_group = DESCRIP_E)

responses_tbs <- responses_by_occ_group_tbs %>%
  summarize(sum = sum(responses_max)) %>%
  pull(sum)

population_tbs <- pop_by_department %>%
  filter(year == 2019) %>%
  filter(departments_and_agencies == "Treasury Board of Canada Secretariat") %>%
  pull(employees)

response_rate_tbs <- responses_tbs / population_tbs

estimated_costs_by_occ_group_tbs <- responses_by_occ_group_tbs %>%
  mutate(employee_count_scaled = round(responses_max / response_rate_tbs)) %>%
  mutate(occ_group = case_when(
    str_detect(occ_group, "AS|CR|FI|IS") ~ str_remove(occ_group, "0"),
    TRUE ~ occ_group
  )) %>%
  left_join(
    rates_of_pay %>%
      select(label:max),
    by = c("occ_group" = "label")
  ) %>%
  rename(
    occ_group_salary_min = min,
    occ_group_salary_max = max
  ) %>%
  mutate(
    est_cost_salary = employee_count_scaled * ((occ_group_salary_min + occ_group_salary_max) / 2),
    est_cost_benefits = est_cost_salary * 0.4,
    est_cost_overhead = employee_count_scaled * 15000,
    est_cost_total = est_cost_salary + est_cost_benefits + est_cost_overhead
  )

estimated_costs_by_occ_group_tbs %>%
  summarize_at(
    vars(est_cost_salary:est_cost_total),
    sum
  ) %>%
  pivot_longer(est_cost_salary:est_cost_total, names_to = "estimated_cost", values_to = "yearly_value") %>%
  mutate(estimated_cost = str_remove(estimated_cost, "est_cost_")) %>%
  mutate(
    monthly_value = yearly_value / 12,
    weekly_value = yearly_value / 52,
    daily_value = yearly_value / 365
  )
  



