#-------------------------------------------------------------------
# Project: Non-Cognitive Skills
# Script: Non-Cognitive Skills and Academic Achievement
# Author: Garen Avanesian
# Date: 21 October 2023
#-------------------------------------------------------------------

# Load the data
int_std <- read_dta(file.path(ssesData, "INT_01_ST_(2021.04.14)_Public.dta"))

# Prepare the data for the abalysis
students =
  int_std %>%
  #exclude Ottowa and Sintra due to lack of the needed variables 
  filter(!SiteID %in% c("01", "10")) %>%
  mutate(Sex = ifelse(Gender_Std==1, "Female", "Male"),
         Life_Satisfaction = ifelse(STQM01901>10, NA, STQM01901)) %>%
  mutate_at(vars(contains("WLE")), ~(scale(.))) %>%
  rowwise() %>%
  mutate(BFI_Open_Mindedness = mean(c(CUR_WLE_ADJ, TOL_WLE_ADJ,CRE_WLE_ADJ)),
         BFI_Task_Performance = mean(c(RES_WLE_ADJ, SEL_WLE_ADJ, PER_WLE_ADJ)),
         BFI_Engaging_with_Others = mean(c(SOC_WLE_ADJ, ASS_WLE_ADJ, ENE_WLE_ADJ)),
         BFI_Collaboration = mean(c(EMP_WLE_ADJ, TRU_WLE_ADJ, COO_WLE_ADJ)),
         BFI_Emotional_Regulation = mean(c(STR_WLE_ADJ, OPT_WLE_ADJ, EMO_WLE_ADJ))) %>%
  ungroup() %>%
  mutate(Cohort = ifelse(CohortID==1, "1. Younger", "2. Older")) %>%
  mutate_at(vars(contains("BFI_")), ~(scale(.))) %>%
  mutate(City = case_when(SiteID == "02" ~ "Houston",
                          SiteID == "03" ~ "Bogota",
                          SiteID == "04" ~ "Manizales",
                          SiteID == "06" ~ "Helsinki",
                          SiteID == "07" ~ "Moscow",
                          SiteID == "08" ~ "Istanbul",
                          SiteID == "09" ~ "Daegu",
                          SiteID == "11" ~ "Suzhou"), 
         N = 1) %>%
  group_by(City) %>%
  mutate(SES_Percentile = percent_rank(SES),
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
         Grades_Bottom25 = ifelse(Read_Bottom25 == "1" & Math_Bottom25 == "1", 1, 0),
         SES_Group = case_when(SES_Percentile <=0.4 ~ "Bottom 40%",
                               SES_Percentile >0.4 & SES_Percentile <=0.9 ~ "Middle 50%",
                               SES_Percentile >0.9 ~ "Top 10%")) %>%
  ungroup() 

# Produce the final data based on the select variables and dropping all NAs
model_data =
  students %>%
  select(SchID, SiteID, Cohort, 
         starts_with("BFI"),  Sex, SES_Group, 
         City, IMMBACK,
         Read_Top25, Math_Top25, 
         Read_Bottom25, Math_Bottom25, 
         Grades_Top25, Grades_Bottom25, WT2019) %>%
  mutate(SES_Group = relevel(factor(SES_Group), ref = "Top 10%")) %>%
  drop_na()
