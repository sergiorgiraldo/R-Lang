library("dplyr")
library("janitor")
library("readxl")

roster_raw <- read_excel(file.path(getwd(), "dirty_data.xlsx")) 

glimpse(roster_raw)

roster <- roster_raw %>%
  clean_names() %>%
  remove_empty("rows") %>%
  remove_empty("cols") %>%
  mutate(hire_date = excel_numeric_to_date(hire_date)) %>%
  select(-certification_1)

glimpse(roster)

roster %>% 
    get_dupes(first_name, last_name)

roster %>%
    tabyl(subject)

roster %>%
  filter(hire_date > as.Date("1950-01-01")) %>%
  tabyl(employee_status, full_time)

roster %>%
  filter(hire_date > as.Date("1950-01-01")) %>%
  tabyl(full_time, employee_status)  

roster %>%
  tabyl(employee_status, sort = TRUE) %>%
  adorn_totals("row")  