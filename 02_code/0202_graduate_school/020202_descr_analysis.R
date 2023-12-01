#-------------------------------------------------------------------
# Project: Non-Cognitive Skills
# Script: Non-Cognitive Skills and Plans to Pursue Graduate Degree
# Author: Garen Avanesian
# Date: 21 October 2023
#-------------------------------------------------------------------


##### SAMPLE #######
students_bfi_m = model_data_hei  %>% select(starts_with("BFI"))

descriptive_bfi = 
  describe(students_bfi_m) %>% 
  rownames_to_column() %>%
  as_tibble() %>%
  select(rowname,  min, median, mean, max, sd, se, skew, kurtosis,  n) %>%
  mutate_if(is.numeric, ~round(.,1))


######### Profiling  ###########

hei = 
  model_data_hei %>%
  filter(Expect_HEI==1) 

# Compute weighted proportions
# Compute weighted proportions without reducing sample size
profile <- hei %>%
  summarise(
    Male = sum(WT2019[Sex == "Male"], na.rm = TRUE) / sum(WT2019, na.rm = TRUE),
    Female = sum(WT2019[Sex == "Female"], na.rm = TRUE) / sum(WT2019, na.rm = TRUE),
    Q1 = sum(WT2019[SES_Quintile == "1"], na.rm = TRUE) / sum(WT2019, na.rm = TRUE),
    Q2 = sum(WT2019[SES_Quintile == "2"], na.rm = TRUE) / sum(WT2019, na.rm = TRUE),
    Q3 = sum(WT2019[SES_Quintile == "3"], na.rm = TRUE) / sum(WT2019, na.rm = TRUE),
    Q4 = sum(WT2019[SES_Quintile == "4"], na.rm = TRUE) / sum(WT2019, na.rm = TRUE),
    Q5 = sum(WT2019[SES_Quintile == "5"], na.rm = TRUE) / sum(WT2019, na.rm = TRUE),
    # `Top 10` = sum(WT2019[SES_Group == "Top 10%"], na.rm = TRUE) / sum(WT2019, na.rm = TRUE),
    # `Middle 50` = sum(WT2019[SES_Quintile == "Middle 50%"], na.rm = TRUE) / sum(WT2019, na.rm = TRUE),
    # `Bottom 40` = sum(WT2019[SES_Group == "Bottom 40%"], na.rm = TRUE) / sum(WT2019, na.rm = TRUE),
    `Grades: Top 25%` = mean(Grades_Top25, na.rm = TRUE),
    `Grades: Bottom 25%` = mean(Grades_Bottom25, na.rm = TRUE), 
    `Grades: Average` = 1 - (`Grades: Top 25%`+`Grades: Bottom 25%`)) %>%
  select(Male, Female, Q1, Q2, Q3, Q4, Q5,
         #`Top 10`, `Middle 50`, `Bottom 40`,
         `Grades: Top 25%`, `Grades: Average`, `Grades: Bottom 25%`)

# Reshape to long format
profile_long <- profile %>%
  pivot_longer(cols = everything(), 
               names_to = "Category", 
               values_to = "Proportion") %>%
  mutate(Group = case_when(str_detect(Category, "ale") ~ "Sex",
                           str_detect(Category, "Q") ~ "SES",
                           str_detect(Category, "Grade") ~ "Academic Performance"))  %>%
  mutate(Group = factor(Group, levels = c("Sex", "SES", "Academic Performance")),
         Category = factor(Category, levels = c("Male", "Female", 
                                                "Q1", "Q2", "Q3", "Q4", "Q5",
                                                #"Top 10", "Middle 50", "Bottom 40",
                                                "Grades: Bottom 25%", "Grades: Average", "Grades: Top 25%")))

ggplot(profile_long, aes(x = Category, y = Proportion)) +
  geom_bar(stat = "identity", fill = "#89CFF0") +
  geom_text(aes(label = scales::percent(Proportion, accuracy = 0.1)),
            vjust = -0.5, color = "black", size = 3.5) +
  facet_wrap(~ Group, scales = "free_x") +
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = "#D1E4F3", color = "black", size = 1.5),
    strip.text = element_text(size = 12, face = "bold")
  ) +
  labs(y = "Proportion", x = NULL)


##### who plans to pursue HE?
# Reshaping the data and dropping NAs from Expect_HEI
long_data <- model_data_hei %>%
  filter(!is.na(Expect_HEI)) %>%
  pivot_longer(cols = starts_with("BFI"), 
               names_to = "Trait", 
               values_to = "Value")

# Modify the Trait values
long_data$Trait <- sub("^BFI_", "", long_data$Trait)  # Removing the "BFI_" prefix
long_data$Trait <- gsub("_", " ", long_data$Trait)    # Replacing underscores with spaces


# Making the boxplot
ggplot(long_data, aes(x = as.factor(Expect_HEI), y = Value, fill = as.factor(Expect_HEI))) +
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(~ Trait, ncol = length(unique(long_data$Trait))) +
  scale_fill_manual(values = c("0" = "#E57373", "1" = "#64B5F6")) +  # Adjusted color values
  scale_x_discrete(name = "Plans to pursue tertiary degree", labels = c("0" = "No", "1" = "Yes")) +
  theme_minimal() +
  theme(legend.position = "none",  # Remove legend
        strip.text = element_text(size = 11, face = "bold"),  # Adjust facet label size and make bold
        strip.background = element_rect(fill = "grey90", color = "grey20", size = 1)) +  # Add a background box to facet labels
  labs(y = "Trait Value")