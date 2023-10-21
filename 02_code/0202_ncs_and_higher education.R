#-------------------------------------------------------------------
# Project: Non-Cognitive Skills
# Script: Non-Cognitive Skills and Plans to Pursue Graduate Degree
# Author: Garen Avanesian
# Date: 21 October 2023
#-------------------------------------------------------------------


source(file.path(rcodes, "0200_packages.R"))

int_std <- read_dta(file.path(ssesData, "INT_01_ST_(2021.04.14)_Public.dta"))


student_moscow_tertiary =
  int_std %>%
  filter(SiteID == "07") %>%
  #we need to generate SES quintiles based on the whole sample, including the 
  #young ones, and then drop the rest
  mutate(SES_Qartile = factor(ntile(SES, 4)),
         SES_Quintile = factor(ntile(SES, 5)),
         SES_Percentile = percent_rank(SES),
         SES_Group = case_when(SES_Percentile <=0.4 ~ "Bottom 40%",
                               SES_Percentile >0.4 & SES_Percentile <=0.9 ~ "Middle 50%",
                               SES_Percentile >0.9 ~ "Top 10%")) %>%
  #we need to exclude the younger cohort
  filter(CohortID==2) %>%
  mutate(Sex = ifelse(Gender_Std==1, "Female", "Male"),
         Life_Satisfaction = ifelse(STQM01901>10, NA, STQM01901),
         Growth_Mindset_Intelligence = ifelse(STQM04403 %in% c(1,2),1,0),
         Growth_Mindset_Emotional = ifelse(STQM04402 %in% c(1,2),1,0),
         Growth_Mindset_Social = ifelse(STQM04401 %in% c(1,2),1,0),
         Reading_Scaled = scale(Sgrade_Read_Lang, scale = T, center = T)*0.39,
         Math_Scaled = scale(Sgrade_Math, scale = T, center = T)*0.35,
         Arts_Scaled = scale(Sgrade_Arts, scale = T, center = T)*0.25) %>%
  mutate_at(vars(contains("WLE")), ~(scale(.))) %>%
  rowwise() %>%
  mutate(BFI_Open_Mindedness = mean(c(CUR_WLE_ADJ, TOL_WLE_ADJ,CRE_WLE_ADJ)),
         BFI_Task_Performance = mean(c(RES_WLE_ADJ, SEL_WLE_ADJ, PER_WLE_ADJ)),
         BFI_Engaging_with_Others = mean(c(SOC_WLE_ADJ, ASS_WLE_ADJ, ENE_WLE_ADJ)),
         BFI_Collaboration = mean(c(EMP_WLE_ADJ, TRU_WLE_ADJ, COO_WLE_ADJ)),
         BFI_Emotional_Regulation = mean(c(STR_WLE_ADJ, OPT_WLE_ADJ, EMO_WLE_ADJ)),
         Grades_Avg = mean(c(Sgrade_Read_Lang, Sgrade_Math, Sgrade_Arts), na.rm = T),
         Grades_Avg_Std = sum(c(Reading_Scaled, Math_Scaled, Arts_Scaled), na.rm = T),
         Growth_Mindset = mean(c(STQM04403, STQM04402, STQM04401))) %>%
  ungroup() %>%
  rename(Grade = STQM00101,
         Self_Efficacy = EFF_WLE_ADJ,
         Achievement_Motivation = MOT_WLE_ADJ) %>%
  mutate(Cohort = ifelse(CohortID==1, "1. Younger", "2. Older"),
         Growth_Mindset = scale(5-Growth_Mindset),
         Expect_HEI = ifelse(STQM02301==3,1,0),
         Grades_Avg1 = scale(Grades_Avg, center = T, scale = T)) %>%
  mutate_at(vars(contains("BFI_")), ~(scale(.))) %>%
  mutate(Reading_Quartiles = factor(ntile(Sgrade_Read_Lang, 4)),
         Math_Quartiles = factor(ntile(Sgrade_Math, 4)),
         Arts_Quartiles = factor(ntile(Sgrade_Arts, 4)),
         Avg_Grades_Quartiles = factor(ntile(Grades_Avg_Std, 4))) %>%
  mutate(Read_Top25 = ifelse(Reading_Quartiles=="4", 1, 0),
         Math_Top25 = ifelse(Math_Quartiles=="4", 1, 0),
         Grades_Top25 = ifelse(Avg_Grades_Quartiles=="4",1, 0),
         Read_Bottom25 = ifelse(Reading_Quartiles=="1", 1, 0),
         Math_Bottom25 = ifelse(Math_Quartiles=="1", 1, 0),
         Grades_Bottom25 = ifelse(Avg_Grades_Quartiles == "1", 1, 0)) %>%
  #we need to drop nas from the outcome variable
  drop_na(Expect_HEI)

#summary(student_moscow_tertiary$Expect_HEI)

##### SAMPLE #######
students_bfi_m = student_moscow_tertiary  %>% select(starts_with("BFI"))

descriptive_bfi = 
  describe(students_bfi_m) %>% 
  rownames_to_column() %>%
  as_tibble() %>%
  select(rowname,  min, median, mean, max, sd, se, skew, kurtosis,  n) %>%
  mutate_if(is.numeric, ~round(.,1))

##### data for modeling
model_data_hei =
  student_moscow_tertiary  %>%
  select(starts_with("BFI"), SchID, SiteID,  Sex, SES_Group,  IMMBACK, SES_Quintile,
         Grades_Top25, Grades_Bottom25, Grades_Avg1, Expect_HEI,
         #Read_Top25, Math_Top25, Read_Bottom25, Math_Bottom25, 
         WT2019) %>%
  mutate(SES_Group = relevel(factor(SES_Group), ref = "Top 10%"),
         City = "Moscow")

#sample characteristics
sex_hei = table(model_data_hei$City, model_data_hei$Sex) %>% as.data.frame() %>% spread(Var2, Freq) %>% rename(City = Var1)
schools_hei = model_data_hei %>% group_by(City) %>% summarise(N_Schools = n_distinct(SchID)) %>% drop_na()
ses_hei = table(model_data_hei$City, model_data_hei$SES_Group) %>% as.data.frame() %>% spread(Var2, Freq) %>% rename(City = Var1)


sample_descr_m = 
  sex_hei %>%
  full_join(ses_hei) %>%
  full_join(schools_m) %>%
  mutate(Total_Obs = Female + Male) 


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


#### MLM ####


####calculate a baseline model
mlm_hei_base = lmer(Expect_HEI ~ Sex + SES_Quintile + Grades_Top25 +
                          BFI_Open_Mindedness + BFI_Task_Performance + BFI_Engaging_with_Others +
                          BFI_Collaboration + BFI_Emotional_Regulation +
                          (1|SchID),  REML = F,control = lmerControl(optimizer ="Nelder_Mead"),
                        data = student_moscow_tertiary)

summary(mlm_hei_base)

# Extract information from the model summary
model_summary <- summary(mlm_hei_base)
fixed_effects <- model_summary$coefficients[,"Estimate"]
standard_errors <- model_summary$coefficients[,"Std. Error"]
z_values <- model_summary$coefficients[,"t value"]
p_values <- model_summary$coefficients[,"Pr(>|t|)"]

# Create a dataset
df_effects <- data.frame(
  term = names(fixed_effects),
  estimate = fixed_effects,
  std_error = standard_errors,
  z_value = z_values,
  p_value = p_values
)

# Specify significance level, e.g., 0.05
alpha_level <- 0.1
df_effects$significant <- ifelse(df_effects$p_value < alpha_level, "p<0.1", "p>=0.1")

# Renaming the terms for a better appearance on the plot


df_effects <- df_effects %>%
  mutate(
    term_label = case_when(
      term == "(Intercept)" ~ "Intercept",
      term == "SexMale" ~ "Sex: Male",
      term == "SES_Quintile2" ~ "SES Quintile 2",
      term == "SES_Quintile3" ~ "SES Quintile 3",
      term == "SES_Quintile4" ~ "SES Quintile 4",
      term == "SES_Quintile5" ~ "SES Quintile 5",
      term == "Grades_Top25" ~ "Grades: Top 25%",
      term == "BFI_Open_Mindedness" ~ "Open Mindedness",
      term == "BFI_Task_Performance" ~ "Task Performance",
      term == "BFI_Engaging_with_Others" ~ "Engaging with Others",
      term == "BFI_Collaboration" ~ "Collaboration",
      term == "BFI_Emotional_Regulation" ~ "Emotional Regulation",
      TRUE ~ as.character(term)  # Keep the original term if none of the conditions match
    ),
    order = 1:n()
  )




# Plotting
ggplot(df_effects, aes(x = fct_reorder(term_label, -order), y = estimate)) +
  geom_point(aes(color = significant), position = position_dodge(0.2), size = 3) +
  geom_errorbar(aes(ymin = estimate - std_error, ymax = estimate + std_error), width = 0.2, position = position_dodge(0.2)) +
  coord_flip() +
  geom_vline(xintercept = 0, linetype = "dotted", color = "gray") +  # Add a dotted vertical line at 0
  #scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 1)) +
  #scale_size_manual(values = c("TRUE" = 4, "FALSE" = 3)) +  # Bigger point sizes
  scale_color_manual(name = "Statistical Significance",
                     values = c("p<0.1" = "#E57373", "p>=0.1" = scales::alpha("#E57373", 0.5))) +  # Adjusted colors
  theme_minimal() +
  theme(legend.position = "bottom",  # Move legend to bottom
        legend.title = element_text(face = "bold"),  # Bold legend title
        axis.text.y = element_text(size = 12)) +  # Bigger y-axis labels
  labs(y = "", x = "") 


####calculate the effect on NCS by SES
mlm_hei_ses = lmer(Expect_HEI ~ Sex  + Grades_Top25 +
                     BFI_Open_Mindedness + BFI_Task_Performance + BFI_Engaging_with_Others +
                     BFI_Collaboration + BFI_Emotional_Regulation +
                     (1|SchID)  + 
                     (1 + BFI_Open_Mindedness + BFI_Task_Performance + BFI_Engaging_with_Others +
                        BFI_Collaboration + BFI_Emotional_Regulation | SES_Quintile) , 
                   REML = F,control = lmerControl(optimizer ="Nelder_Mead"),
                   data = student_moscow_tertiary)


mlm_hei_by_ses =
  coef(mlm_hei_ses)$SES_Quintile %>%
  as_tibble() %>%
  select(starts_with("BFI")) %>%
  rename(`Open Mindedness` = BFI_Open_Mindedness,
         `Task Performance` = BFI_Task_Performance,
         `Engaging with Others` = BFI_Engaging_with_Others,
         Collaboration = BFI_Collaboration,
         `Emotional Regulation` = BFI_Emotional_Regulation) %>%
  mutate(SES_Quintile = c("1. Poorest", "2. Poor", "3. Middle", "4. Rich", "5. Richest")) %>%
  gather(Skill, Estimate, -SES_Quintile) %>%
  mutate(Order_Value = c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4,5,5,5,5,5)) %>%
  mutate(Skill = fct_reorder(Skill, Order_Value)) %>%
  ggplot(., aes(Estimate, SES_Quintile))+
  geom_point(size = 7, color = "lightblue")+
  geom_text(aes(label = round(Estimate, 2)), size = 2, color = "black")+
  #geom_segment( aes(x=0, xend=Estimate, y=SES_Quintile, yend=SES_Quintile), color="skyblue")+
  geom_vline(xintercept=0, linetype="solid",  color = "black")+
  theme_bw()+
  scale_y_discrete(limits=rev)+
  facet_wrap(Skill~.,)+# c(2,3))+
  ylab("")+
  xlab("Probability of Pursuing Graduate Degree")

