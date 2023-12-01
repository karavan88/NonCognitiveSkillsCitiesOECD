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
  mutate(SES_Quartile = factor(ntile(SES, 4)),
         SES_Quintile = factor(ntile(SES, 5)),
         SES_Percentile = percent_rank(SES),
         SES_Group = case_when(SES_Percentile <=0.4 ~ "Bottom 40%",
                               SES_Percentile >0.4 & SES_Percentile <=0.9 ~ "Middle 50%",
                               SES_Percentile >0.9 ~ "Top 10%")) %>%
  #we need to exclude the younger cohort
  filter(CohortID==2) %>%
  mutate(Sex = ifelse(Gender_Std==1, "Female", "Male"),
         Reading_Scaled = scale(Sgrade_Read_Lang, scale = T, center = T)*0.39,
         Math_Scaled = scale(Sgrade_Math, scale = T, center = T)*0.35,
         Arts_Scaled = scale(Sgrade_Arts, scale = T, center = T)*0.25) %>%
  mutate_at(vars(contains("WLE")), ~(scale(.))) %>%
  rowwise() %>%
  mutate(BFI_Open_Mindedness = mean(c(CUR_WLE_ADJ, TOL_WLE_ADJ,CRE_WLE_ADJ)),
         BFI_Task_Performance = mean(c(RES_WLE_ADJ, SEL_WLE_ADJ, PER_WLE_ADJ)),
         BFI_Engaging_with_Others = mean(c(SOC_WLE_ADJ, ASS_WLE_ADJ, ENE_WLE_ADJ)),
         BFI_Collaboration = mean(c(EMP_WLE_ADJ, TRU_WLE_ADJ, COO_WLE_ADJ)),
         BFI_Emotional_Regulation = mean(c(STR_WLE_ADJ, OPT_WLE_ADJ, EMO_WLE_ADJ))) %>%
  ungroup() %>%
  mutate(Expect_HEI = ifelse(STQM02301==3,1,0),
         Reading_Quartiles = factor(ntile(Sgrade_Read_Lang, 4)),
         Math_Quartiles = factor(ntile(Sgrade_Math, 4)),
         Arts_Quartiles = factor(ntile(Sgrade_Arts, 4))) %>%
  mutate(Read_Top25 = ifelse(Reading_Quartiles == "4", 1, 0),
         Math_Top25 = ifelse(Math_Quartiles == "4", 1, 0),
         Arts_Top25 = ifelse(Arts_Quartiles == "4", 1, 0),
         Grades_Top25 = ifelse(Read_Top25 == "1" & Math_Top25 == "1", 1, 0),
         Read_Bottom25 = ifelse(Reading_Quartiles=="1", 1, 0),
         Math_Bottom25 = ifelse(Math_Quartiles=="1", 1, 0),
         Arts_Bottom25 = ifelse(Arts_Quartiles == "1", 1, 0),
         Grades_Bottom25 = ifelse(Read_Bottom25 == "1" & Math_Bottom25 == "1", 1, 0)) %>%
  #we need to drop nas from the outcome variable
  drop_na(Expect_HEI)


##### data for modeling
model_data_hei =
  student_moscow_tertiary  %>%
  select(starts_with("BFI"), SchID, SiteID,  Sex, SES_Group,  SES_Quartile, SES_Quintile,
         Grades_Top25, Grades_Bottom25, Expect_HEI,
         WT2019) %>%
  mutate(SES_Quintile = relevel(factor(SES_Quintile), ref = "5"),
         City = "Moscow") %>%
  drop_na()