
responses %>%
  filter(SURVEYR == 2019) %>%
  filter(subset == 1) %>%
  group_by(DEPT_E) %>%
  summarize(est_employees = max(ANSCOUNT, na.rm = TRUE)) %>%
  left_join(
    pop_by_department %>%
      filter(year == 2019) %>%
      select(departments_and_agencies, employees),
    by = c("DEPT_E" = "departments_and_agencies")
  ) %>%
  mutate(
    est_response_rate = est_employees / employees
  )

## find repeat depts/differently named depts :upside_down_smile:
responses %>%
  filter(SURVEYR == 2019) %>%
  filter(subset == 1) %>%
  distinct(DEPT_E) %>%
  mutate(source = "pses") %>%
  rbind(
    pop_by_department %>%
      filter(year == 2019) %>%
      distinct(departments_and_agencies) %>%
      rename(DEPT_E = departments_and_agencies) %>%
      mutate(source = "pbd")
  ) %>%
  View()




responses %>%
  filter(SURVEYR == 2019) %>%
  filter(subset == 3)


