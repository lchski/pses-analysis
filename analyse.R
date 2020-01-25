
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
