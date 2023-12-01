#-------------------------------------------------------------------
# Project: Non-Cognitive Skills
# Script: Non-Cognitive Skills and Academic Achievement
# Author: Garen Avanesian
# Date: 21 October 2023
#-------------------------------------------------------------------

# Produce table of sample characteristics

# sample characteristics
age <- table(model_data$City, model_data$Cohort) %>%
  as.data.frame() %>%
  spread(Var2, Freq) %>%
  rename(City = Var1)

sex <- table(model_data$City, model_data$Sex) %>%
  as.data.frame() %>%
  spread(Var2, Freq) %>%
  rename(City = Var1)

schools <- model_data %>%
  group_by(City) %>%
  summarise(N_Schools = n_distinct(SchID)) %>%
  drop_na()

sample_descr <-
  age %>%
  full_join(sex) %>%
  full_join(schools) %>%
  mutate(Total_Obs = `1. Younger` + `2. Older`)

# Adding a row with totals for each column
total_row <- sample_descr %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE))

total_row$City <- "Total"

# Append the total row to your original data frame
sample_descr <- rbind(sample_descr, total_row)

# Assess equity of high and low performance

equity <-
  model_data %>%
  select(City, Grades_Top25, Grades_Bottom25, SES_Group, WT2019) %>%
  drop_na() %>%
  as_survey_design(weights = WT2019) %>%
  group_by(City, SES_Group) %>%
  as_survey(weight = WT2019) %>%
  summarise(
    HP = survey_mean(Grades_Top25),
    LP = survey_mean(Grades_Bottom25)
  ) %>%
  # arrange(City, Grades, SES_Group) %>%
  select(-contains("_se")) %>%
  mutate_if(is.numeric, ~ round(., 3)) %>%
  filter(!str_detect(SES_Group, "Middle")) %>%
  pivot_wider(
    names_from = SES_Group,
    values_from = c(HP, LP)
  ) %>%
  mutate(
    WPR_LP = `LP_Bottom 40%` / `LP_Top 10%`,
    WPR_HP = `HP_Bottom 40%` / `HP_Top 10%`
  ) %>%
  as_tibble() %>%
  select(
    City, `LP_Bottom 40%`, `LP_Top 10%`, WPR_LP,
    `HP_Bottom 40%`, `HP_Top 10%`, WPR_HP
  )
