library(tidyverse)
library(haven)
library(lme4)
library(lmerTest)
#library(lmtest)
library(sjPlot)
library(srvyr)
library(psych)
library(corrplot)
library(RColorBrewer)
library(ggeffects)


##### goood links #####

# https://easystats.github.io/performance/reference/icc.html
# https://www.rensvandeschoot.com/tutorials/lme4/
# https://m-clark.github.io/mixed-models-with-R/random_intercepts.html#the-mixed-model
# https://psyteachr.github.io/ug3-stats/introducing-linear-mixed-effects-models.html
# https://quantdev.ssri.psu.edu/tutorials/r-bootcamp-introduction-multilevel-model-and-interactions
# https://www.alexanderdemos.org/Mixed3.html

####

int_std <- read_dta("INT_01_ST_(2021.04.14)_Public.dta")

students=
  int_std %>%
  filter(!SiteID %in% c("01", "10")) %>%
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
  mutate(City = case_when(#SiteID == "01" ~ "Ottawa",
                          SiteID == "02" ~ "Houston",
                          SiteID == "03" ~ "Bogota",
                          SiteID == "04" ~ "Manizales",
                          SiteID == "06" ~ "Helsinki",
                          SiteID == "07" ~ "Moscow",
                          SiteID == "08" ~ "Istanbul",
                          SiteID == "09" ~ "Daegu",
                          #SiteID == "10" ~ "Sintra",
                          SiteID == "11" ~ "Suzhou"),
         N = 1) %>%
  group_by(City) %>%
  mutate(SES_Qartile = factor(ntile(SES, 4)),
         SES_Percentile = percent_rank(SES),
         Reading_Quartiles = factor(ntile(Sgrade_Read_Lang, 4)),
         Math_Quartiles = factor(ntile(Sgrade_Math, 4)),
         Arts_Quartiles = factor(ntile(Sgrade_Arts, 4)),
         Avg_Grades_Quartiles = factor(ntile(Grades_Avg_Std, 4))) %>%
  mutate(Read_Top25 = ifelse(Reading_Quartiles=="4", 1, 0),
         Math_Top25 = ifelse(Math_Quartiles=="4", 1, 0),
         Grades_Top25 = ifelse(Avg_Grades_Quartiles=="4",1, 0),
         Read_Bottom25 = ifelse(Reading_Quartiles=="1", 1, 0),
         Math_Bottom25 = ifelse(Math_Quartiles=="1", 1, 0),
         Grades_Bottom25 = ifelse(Avg_Grades_Quartiles == "1", 1, 0),
         SES_Group = case_when(SES_Percentile <=0.4 ~ "Bottom 40%",
                                SES_Percentile >0.4 & SES_Percentile <=0.9 ~ "Middle 50%",
                                SES_Percentile >0.9 ~ "Top 10%")) %>%
  ungroup() #%>%
  #mutate(Poorest40 = ifelse(SES_Quintile %in% c(1,2), "Bottom 40%", "Top 60%"))


##### section on PCA which is out of sequence and is used to modify the code above ######
ggplot(students, aes(Grades_Avg_Std, Grades_Avg1))+
  geom_point(aes(color = City), alpha = 0.2)

#### we could produce PCA based indices
grades = 
  students %>%
  select(Sgrade_Math, Sgrade_Read_Lang, Sgrade_Arts) %>%
  mutate_all(~scale(., center = T, scale = T)) %>%
  drop_na()

dim(grades)
summary(grades)

pca <- prcomp(grades)

summary(pca$x[,1])

# Extract the loadings for the first principal component
loadings <- pca$rotation[,1]

# Calculate the variance explained by each variable within the first principal component
variance_explained <- (loadings^2)/sum(loadings^2)

#############. ---- end of pca section #########

#### Sample characteristics
#n schools per city
#proportion boys and girls
#proportion 10 and 15 year olds

#students$Sgrade_Read_Lang = factor(round(students$Sgrade_Read_Lang))

#ach = filter(students, Sgrade_Read_Lang==50)
#summary(factor(students$Reading_Quintiles[students$City=="Bogota"]))

table(students$City, students$N)

students_bfi = student %>% select(starts_with("BFI"), City, Cohort)

sample_detailed = 
  describeBy(students_bfi, group = c("City", "Cohort"), mat = T, digits = 2) %>% 
  rownames_to_column() %>%
  as_tibble() %>%
  mutate(item = as.numeric(item)) %>%
  filter(item <=100) %>%
  select(rowname, group1, group2, min, median, mean, max, sd, se, skew, kurtosis,  n)



View(sample_detailed)

#### correlations
bfi = 
  students %>%
  select(contains("BFI")) %>%
  drop_na()


pairs.panels(bfi,
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)



model_data =
  students %>%
  select(starts_with("BFI"), SchID, SiteID, Cohort, CohortID, Sex, SES_Group, City, IMMBACK,
         Grades_Top25, Grades_Bottom25, Grades_Avg, Grades_Avg1, Grades_Avg_Std,
         Read_Top25,Math_Top25, Read_Bottom25, Math_Bottom25, WT2019) %>%
  mutate(SES_Group = relevel(factor(SES_Group), ref = "Top 10%"))


ggplot(model_data, aes(Grades_Avg1))+
  geom_density()+
  facet_wrap(City~.)

#sample characteristics
age = table(model_data_reading$City, model_data_reading$Cohort) %>% as.data.frame() %>% spread(Var2, Freq) %>% rename(City = Var1)
sex = table(model_data_reading$City, model_data_reading$Sex) %>% as.data.frame() %>% spread(Var2, Freq) %>% rename(City = Var1)
schools = model_data_reading %>% group_by(City) %>% summarise(N_Schools = n_distinct(SchID)) %>% drop_na()

sample_descr = 
  age %>%
  full_join(sex) %>%
  full_join(schools) %>%
  mutate(Total_Obs = `1. Younger` + `2. Older`) 


#### charts

#NCS by city and age
bfi_box =
  model_data %>%
  select(starts_with("BFI"), City) %>%
  drop_na(City) %>%
  rename(`Open Mindedness` = BFI_Open_Mindedness,
         `Task Performance` = BFI_Task_Performance,
         `Engaging with Others` = BFI_Engaging_with_Others,
         Collaboration = BFI_Collaboration,
         `Emotional Regulation` = BFI_Emotional_Regulation) %>%
  gather(Skill, Value, -City) %>%
  ggplot(aes(Skill, Value))+
  geom_boxplot()+
  facet_wrap(City~., nrow = 2)+
  xlab("")+
  ylab("Skill (Standardized)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "bottom", # c(0.85,-0.15),
        legend.background = element_rect( size=0.5, linetype="solid",  colour ="black"))



#economic equity of high performance

equity =
  model_data %>%
  select(City, Grades_Top25, SES_Group, WT2019) %>%
  drop_na() %>%
  as_survey_design(weights  = WT2019) %>%
  group_by(City, SES_Group) %>%
  as_survey(weight = WT2019) %>%
  summarise(HP = survey_mean(Grades_Top25)) %>%
  #arrange(City, Grades, SES_Group) %>%
  select(-contains("_se")) %>%
  mutate(HP = round(HP*100,1)) %>%
  spread(SES_Group, HP) %>%
  mutate(WPI = round(`Bottom 40%`/`Top 10%`,2)) %>%
  #mutate_if(is.numeric, round, 3) %>%
  select(City, `Bottom 40%`, `Middle 50%`, `Top 10%`, `WPI`)

write.csv(equity, "equity.csv", row.names = F)

View(equity)

wealth_parity = 
  equity %>%
  spread(SES_Group, HP) %>%
  mutate(WPI = `Bottom 40%`/`Top 10%`) %>%
  arrange(WPI)

ggplot(equity, aes(SES_Group, HP, fill = SES_Group))+
  geom_col(color = "black")+
  facet_wrap(City~., nrow = 2)+
  geom_text(aes(label = round(HP,3)*100), vjust = 1.35)+
  scale_y_continuous(labels = scales::percent_format())+
  xlab("")+
  ylab("Share of High Performing\nStudents in Reading/Language")+
  theme_bw()+
  theme(#axis.text.x = element_text(angle = 45),
        legend.title= element_blank(),
        legend.position = "bottom", # c(0.85,-0.15),
        legend.background = element_rect( size=0.5, linetype="solid",  colour ="black"))


################# PROFILING #################

### High achievers ####

profiling_ha = 
  students %>%
  filter(Read_Top25==1 |Math_Top25==1 ) %>%
  select(starts_with("BFI"), City, SES_Group, Cohort, Sex, WT2019)

sex_ha = profiling_ha %>% as_survey_design(weights = WT2019) %>% drop_na(Sex) %>% group_by(City, Sex) %>% summarise(survey_prop()) %>% select(-contains("_se"))
age_ha = profiling_ha %>% as_survey_design(weights = WT2019) %>% drop_na(Cohort) %>% group_by(City, Cohort) %>% summarise(survey_prop()) %>% select(-contains("_se"))
ses_ha = profiling_ha %>% as_survey_design(weights = WT2019) %>% drop_na(SES_Group) %>% group_by(City, SES_Group) %>% summarise(survey_prop()) %>% select(-contains("_se"))


profiling_la = 
  students %>%
  filter(Read_Bottom25==1 |Math_Bottom25==1 ) %>%
  select(starts_with("BFI"), City, SES_Group, Cohort, Sex, WT2019)

sex_la = profiling_la %>% as_survey_design(weights = WT2019) %>% drop_na(Sex) %>% group_by(City, Sex) %>% summarise(survey_prop()) %>% select(-contains("_se"))
age_la = profiling_la %>% as_survey_design(weights = WT2019) %>% drop_na(Cohort) %>% group_by(City, Cohort) %>% summarise(survey_prop()) %>% select(-contains("_se"))
ses_la = profiling_la %>% as_survey_design(weights = WT2019) %>% drop_na(SES_Group) %>% group_by(City, SES_Group) %>% summarise(survey_prop()) %>% select(-contains("_se"))



#### MLM


##### low achievement #####
mlm_la_base = lmer(Grades_Bottom25 ~ Sex + SES_Group  + factor(IMMBACK) +
                         (1|City/SchID)  + (1| Cohort) + (1 + SES_Group|City) ,
                       REML = F, control = lmerControl(optimizer ="Nelder_Mead"),
                       data = model_data)

mlm_la_ses = lmer(Grades_Bottom25 ~ Sex + SES_Group + CohortID + factor(IMMBACK) +
                BFI_Open_Mindedness + BFI_Task_Performance + BFI_Engaging_with_Others +
                BFI_Collaboration + BFI_Emotional_Regulation + 
                (1|City/SchID)  + (1| CohortID) + (1 + SES_Group|City) +
                (1 + BFI_Open_Mindedness + BFI_Task_Performance + 
                   BFI_Engaging_with_Others +
                   BFI_Collaboration + BFI_Emotional_Regulation |City) ,
              REML = F, control = lmerControl(optimizer ="Nelder_Mead"),
              data = model_data)


mlm_la_master = lmer(Grades_Bottom25 ~ Sex +  factor(IMMBACK) +
                         BFI_Open_Mindedness + BFI_Task_Performance + BFI_Engaging_with_Others +
                         BFI_Collaboration + BFI_Emotional_Regulation + 
                         (1|City/SchID)  + (1| Cohort) + 
                         (1 + BFI_Open_Mindedness + BFI_Task_Performance + 
                            BFI_Engaging_with_Others +
                            BFI_Collaboration + BFI_Emotional_Regulation || City:SES_Group) ,
                       REML = F, control = lmerControl(optimizer ="Nelder_Mead"),
                       data = model_data)

##### compare two models

# Extract the regression coefficients of SES_Group random slopes by City from each model
Model1_RE = coef(mlm_la_base)$City["SES_GroupBottom 40%"] %>% as.data.frame() %>%
  rownames_to_column() %>% mutate(Model = "Model without NCS")

Model2_RE = coef(mlm_la)$City["SES_GroupBottom 40%"] %>% as.data.frame() %>% 
  rownames_to_column() %>% mutate(Model = "Model with NCS")

Model1_FE = unique(coef(mlm_la_base)$CohortID["SES_GroupBottom 40%"]) %>% as.data.frame() %>% 
  mutate(rowname = "Average Fixed Effect", Model = "Model without NCS")

Model2_FE = unique(coef(mlm_la)$CohortID["SES_GroupBottom 40%"]) %>% as.data.frame() %>% 
  mutate(rowname = "Average Fixed Effect", Model = "Model with NCS")

coef_df = 
  Model1_FE %>%
  bind_rows(Model1_RE, Model2_FE, Model2_RE)

rownames(coef_df) <- NULL

# Plot the coefficients as a geom_point plot
ggplot(coef_df, aes(x = `SES_GroupBottom 40%`, y = fct_reorder(rowname, desc(rowname) ))) +
  geom_point(aes(color = Model), size = 7.5) +
  geom_text(aes(label = round(`SES_GroupBottom 40%`,3)*100), size = 3)+
  scale_color_manual(values = c( "lightblue", "#FF9999"))+
  scale_x_continuous(limits = c(0, 0.15), labels = scales::percent_format())+
  labs(x = "Effect of Bottom 40% by SES on Probability of\n Low Academic Performance", y = "City") +
  theme_bw()+
  theme(legend.position = "bottom",
        legend.background = element_rect(size=0.5, linetype="solid", 
          colour ="black"))


###### Model Effects
ggpredict(mlm_la, terms = c("BFI_Task_Performance","SES_Group", "City"),
          type="re") %>% 
  plot()+
  scale_color_manual(values = c("red", "green", "blue"), 
                     breaks = c("Bottom 40%", "Middle 50%", "Top 10%"))+
  theme_bw()+
  theme(legend.position = c(0.85,0.13),
        legend.background = element_rect(#fill="gray",
          size=0.5, linetype="solid", 
          colour ="black")) +
  ylim(0,1)+
  ylab("Probability of Low Performance in\n Reading and Language")+
  xlab("Task Performance")

ggpredict(mlm_la, terms = c("BFI_Open_Mindedness","SES_Group", "City"),
          type="re") %>% 
  plot()+
  scale_color_manual(values = c("red", "green", "blue"), 
                     breaks = c("Bottom 40%", "Middle 50%", "Top 10%"))+
  theme_bw()+
  theme(legend.position = c(0.85,0.13),
        legend.background = element_rect(#fill="gray",
          size=0.5, linetype="solid", 
          colour ="black")) +
  ylim(0,1)+
  ylab("Probability of Low Performance in\n Reading and Language")+
  xlab("Open Mindedness")

ggpredict(mlm_la, terms = c("BFI_Engaging_with_Others","SES_Group", "City"),
          type="re") %>% 
  plot()+
  scale_color_manual(values = c("red", "green", "blue"), 
                     breaks = c("Bottom 40%", "Middle 50%", "Top 10%"))+
  theme_bw()+
  theme(legend.position = c(0.85,0.13),
        legend.background = element_rect(#fill="gray",
          size=0.5, linetype="solid", 
          colour ="black")) +
  ylim(0,1)+
  ylab("Probability of Low Performance in\n Reading and Language")+
  xlab("Engaging with Others")

ggpredict(mlm_la, terms = c("BFI_Emotional_Regulation","SES_Group", "City"),
          type="re") %>% 
  plot()+
  scale_color_manual(values = c("red", "green", "blue"), 
                     breaks = c("Bottom 40%", "Middle 50%", "Top 10%"))+
  theme_bw()+
  theme(legend.position = c(0.85,0.13),
        legend.background = element_rect(#fill="gray",
          size=0.5, linetype="solid", 
          colour ="black")) +
  ylim(0,1)+
  ylab("Probability of Low Performance in\n Reading and Language")+
  xlab("Emotional Regulation")

ggpredict(mlm_la, terms = c("BFI_Collaboration","SES_Group", "City"),
          type="re") %>% 
  plot()+
  scale_color_manual(values = c("red", "green", "blue"), 
                     breaks = c("Bottom 40%", "Middle 50%", "Top 10%"))+
  theme_bw()+
  theme(legend.position = c(0.85,0.13),
        legend.background = element_rect(#fill="gray",
          size=0.5, linetype="solid", 
          colour ="black")) +
  ylim(0,1)+
  ylab("Probability of Low Performance in\n Reading and Language")+
  xlab("Collaboration")


##### high achievement #####

mlm_ha_null = lmer(Grades_Top25 ~ 1 + (1|City/SchID)  + (1| Cohort) + (1 |SES_Group),
                   data = model_data)

summary(mlm_ha_null)



mlm_ha_base = lmer(Grades_Top25 ~ Sex + SES_Group  + factor(IMMBACK) +
                     (1|City/SchID)  + (1| Cohort) + (1 + SES_Group|City) ,
                   REML = F, control = lmerControl(optimizer ="Nelder_Mead"),
                   data = model_data)

mlm_ha_ses = lmer(Grades_Top25 ~ Sex + SES_Group + CohortID + factor(IMMBACK) +
                    BFI_Open_Mindedness + BFI_Task_Performance + BFI_Engaging_with_Others +
                    BFI_Collaboration + BFI_Emotional_Regulation + 
                    (1|City/SchID)  + (1| CohortID) + (1 + SES_Group|City) +
                    (1 + BFI_Open_Mindedness + BFI_Task_Performance + 
                       BFI_Engaging_with_Others +
                       BFI_Collaboration + BFI_Emotional_Regulation |City) ,
                  REML = F, control = lmerControl(optimizer ="Nelder_Mead"),
                  data = model_data)


##### compare two models

# Extract the regression coefficients of SES_Group random slopes by City from each model
Model1_RE_HA = coef(mlm_ha_base)$City["SES_GroupBottom 40%"] %>% as.data.frame() %>%
  rownames_to_column() %>% mutate(Model = "Model without NCS")

Model2_RE_HA = coef(mlm_ha_ses)$City["SES_GroupBottom 40%"] %>% as.data.frame() %>% 
  rownames_to_column() %>% mutate(Model = "Model with NCS")

Model1_FE_HA = unique(coef(mlm_ha_base)$Cohort["SES_GroupBottom 40%"]) %>% as.data.frame() %>% 
  mutate(rowname = "Average Fixed Effect", Model = "Model without NCS")

Model2_FE_HA = unique(coef(mlm_ha_ses)$Cohort["SES_GroupBottom 40%"]) %>% as.data.frame() %>% 
  mutate(rowname = "Average Fixed Effect", Model = "Model with NCS")

coef_df_ha = 
  Model1_FE_HA %>%
  bind_rows(Model1_RE_HA, Model2_FE_HA, Model2_RE_HA)

rownames(coef_df_ha) <- NULL

# Plot the coefficients as a geom_point plot
ggplot(coef_df_ha, aes(x = `SES_GroupBottom 40%`, y = fct_reorder(rowname, desc(rowname) ))) +
  geom_point(aes(color = Model), size = 7.5) +
  geom_text(aes(label = round(`SES_GroupBottom 40%`,3)*100), size = 3)+
  scale_color_manual(values = c( "lightblue", "#FF9999"))+
  scale_x_continuous(labels = scales::percent_format())+
  labs(x = "Effect of Bottom 40% by SES on Probability of\n High Academic Performance", y = "City") +
  theme_bw()+
  theme(legend.position = "bottom",
        legend.background = element_rect(size=0.5, linetype="solid", 
                                         colour ="black"))

##### HA master model

mlm_ha_master = lmer(Grades_Top25 ~ Sex +  factor(IMMBACK) +
                       BFI_Open_Mindedness + BFI_Task_Performance + BFI_Engaging_with_Others +
                       BFI_Collaboration + BFI_Emotional_Regulation + 
                       (1|City/SchID)  + (1| Cohort) + 
                       (1 + BFI_Open_Mindedness + BFI_Task_Performance + 
                          BFI_Engaging_with_Others +
                          BFI_Collaboration + BFI_Emotional_Regulation || City:SES_Group) ,
                     REML = F, control = lmerControl(optimizer ="Nelder_Mead"),
                     data = model_data)


###### Model Effects
ggpredict(mlm_ha_master, terms = c("BFI_Task_Performance","SES_Group", "City"),
          type="re") %>% 
  plot()+
  scale_color_manual(values = c("red", "green", "blue"), 
                     breaks = c("Bottom 40%", "Middle 50%", "Top 10%"))+
  theme_bw()+
  theme(legend.position = c(0.85,0.13),
        legend.background = element_rect(#fill="gray",
          size=0.5, linetype="solid", 
          colour ="black")) +
  ylim(0,1)+
  ylab("Probability of High Performance in\n Reading and hanguage")+
  xlab("Task Performance")

ggpredict(mlm_ha_master, terms = c("BFI_Open_Mindedness","SES_Group", "City"),
          type="re") %>% 
  plot()+
  scale_color_manual(values = c("red", "green", "blue"), 
                     breaks = c("Bottom 40%", "Middle 50%", "Top 10%"))+
  theme_bw()+
  theme(legend.position = c(0.85,0.13),
        legend.background = element_rect(#fill="gray",
          size=0.5, linetype="solid", 
          colour ="black")) +
  ylim(0,1)+
  ylab("Probability of High Performance in\n Reading and hanguage")+
  xlab("Open Mindedness")

ggpredict(mlm_ha_master, terms = c("BFI_Engaging_with_Others","SES_Group", "City"),
          type="re") %>% 
  plot()+
  scale_color_manual(values = c("red", "green", "blue"), 
                     breaks = c("Bottom 40%", "Middle 50%", "Top 10%"))+
  theme_bw()+
  theme(legend.position = c(0.85,0.13),
        legend.background = element_rect(#fill="gray",
          size=0.5, linetype="solid", 
          colour ="black")) +
  ylim(0,1)+
  ylab("Probability of High Performance in\n Reading and hanguage")+
  xlab("Engaging with Others")

ggpredict(mlm_ha_master, terms = c("BFI_Emotional_Regulation","SES_Group", "City"),
          type="re") %>% 
  plot()+
  scale_color_manual(values = c("red", "green", "blue"), 
                     breaks = c("Bottom 40%", "Middle 50%", "Top 10%"))+
  theme_bw()+
  theme(legend.position = c(0.85,0.13),
        legend.background = element_rect(#fill="gray",
          size=0.5, linetype="solid", 
          colour ="black")) +
  ylim(0,1)+
  ylab("Probability of High Performance in\n Reading and hanguage")+
  xlab("Emotional Regulation")

ggpredict(mlm_ha_master, terms = c("BFI_Collaboration","SES_Group", "City"),
          type="re") %>% 
  plot()+
  scale_color_manual(values = c("red", "green", "blue"), 
                     breaks = c("Bottom 40%", "Middle 50%", "Top 10%"))+
  theme_bw()+
  theme(legend.position = c(0.85,0.13),
        legend.background = element_rect(#fill="gray",
          size=0.5, linetype="solid", 
          colour ="black")) +
  ylim(0,1)+
  ylab("Probability of High Performance in\n Reading and hanguage")+
  xlab("Collaboration")


##### average grade #####
mlm_avg_base = lmer(Grades_Avg1 ~ Sex + SES_Group + CohortID + factor(IMMBACK) +
                      (1|City/SchID)  + (1| CohortID) + (1 + SES_Group|City) ,
                    REML = F, control = lmerControl(optimizer ="Nelder_Mead"),
                    data = model_data)

mlm_avg = lmer(Grades_Avg1 ~ Sex + SES_Group + CohortID + factor(IMMBACK) +
                 BFI_Open_Mindedness + BFI_Task_Performance + BFI_Engaging_with_Others +
                 BFI_Collaboration + BFI_Emotional_Regulation + 
                 (1|City/SchID)  + (1| CohortID) + (1 + SES_Group|City) +
                 (1 + BFI_Open_Mindedness + BFI_Task_Performance + 
                    BFI_Engaging_with_Others +
                    BFI_Collaboration + BFI_Emotional_Regulation || City:SES_Group) ,
               REML = F, control = lmerControl(optimizer ="Nelder_Mead"),
               data = model_data)

##### compare two models

# Extract the regression coefficients of SES_Group random slopes by City from each model
Model1_RE_avg = coef(mlm_avg_base)$City["SES_GroupBottom 40%"] %>% as.data.frame() %>%
  rownames_to_column() %>% mutate(Model = "Model without NCS")

Model2_RE_avg = coef(mlm_avg)$City["SES_GroupBottom 40%"] %>% as.data.frame() %>% 
  rownames_to_column() %>% mutate(Model = "Model with NCS")

Model1_FE_avg = unique(coef(mlm_avg_base)$CohortID["SES_GroupBottom 40%"]) %>% as.data.frame() %>% 
  mutate(rowname = "Average Fixed Effect", Model = "Model without NCS")

Model2_FE_avg = unique(coef(mlm_avg)$CohortID["SES_GroupBottom 40%"]) %>% as.data.frame() %>% 
  mutate(rowname = "Average Fixed Effect", Model = "Model with NCS")

coef_df_avg = 
  Model1_FE_avg %>%
  bind_rows(Model1_RE_avg, Model2_FE_avg, Model2_RE_avg)

rownames(coef_df_avg) <- NULL

# Plot the coefficients as a geom_point plot
ggplot(coef_df_avg, aes(x = `SES_GroupBottom 40%`, y = fct_reorder(rowname, desc(rowname) ))) +
  geom_point(aes(color = Model), size = 7.5) +
  geom_text(aes(label = round(`SES_GroupBottom 40%`, 3), size = 1.5))+
  scale_color_manual(values = c( "lightblue", "#FF9999"))+
  #scale_x_continuous(labels = scales::percent_format())+
  labs(x = "Effect of Bottom 40% by SES on Probability of\n High Academic Performance", y = "City") +
  theme_bw()+
  theme(legend.position = "bottom",
        legend.background = element_rect(size=0.5, linetype="solid", 
                                         colour ="black"))


###### Model Effects
ggpredict(mlm_avg, terms = c("BFI_Task_Performance","SES_Group", "City"),
          type="re") %>% 
  plot()+
  scale_color_manual(values = c("red", "green", "blue"), 
                     breaks = c("Bottom 40%", "Middle 50%", "Top 10%"))+
  theme_bw()+
  theme(legend.position = c(0.85,0.13),
        legend.background = element_rect(#fill="gray",
          size=0.5, linetype="solid", 
          colour ="black")) +
  ylim(0,1)+
  ylab("Probability of High Performance in\n Reading and hanguage")+
  xlab("Task Performance")

ggpredict(mlm_avg, terms = c("BFI_Open_Mindedness","SES_Group", "City"),
          type="re") %>% 
  plot()+
  scale_color_manual(values = c("red", "green", "blue"), 
                     breaks = c("Bottom 40%", "Middle 50%", "Top 10%"))+
  theme_bw()+
  theme(legend.position = c(0.85,0.13),
        legend.background = element_rect(#fill="gray",
          size=0.5, linetype="solid", 
          colour ="black")) +
  ylim(0,1)+
  ylab("Probability of High Performance in\n Reading and hanguage")+
  xlab("Open Mindedness")

ggpredict(mlm_avg, terms = c("BFI_Engaging_with_Others","SES_Group", "City"),
          type="re") %>% 
  plot()+
  scale_color_manual(values = c("red", "green", "blue"), 
                     breaks = c("Bottom 40%", "Middle 50%", "Top 10%"))+
  theme_bw()+
  theme(legend.position = c(0.85,0.13),
        legend.background = element_rect(#fill="gray",
          size=0.5, linetype="solid", 
          colour ="black")) +
  ylim(0,1)+
  ylab("Probability of High Performance in\n Reading and hanguage")+
  xlab("Engaging with Others")

ggpredict(mlm_avg, terms = c("BFI_Emotional_Regulation","SES_Group", "City"),
          type="re") %>% 
  plot()+
  scale_color_manual(values = c("red", "green", "blue"), 
                     breaks = c("Bottom 40%", "Middle 50%", "Top 10%"))+
  theme_bw()+
  theme(legend.position = c(0.85,0.13),
        legend.background = element_rect(#fill="gray",
          size=0.5, linetype="solid", 
          colour ="black")) +
  ylim(0,1)+
  ylab("Probability of High Performance in\n Reading and hanguage")+
  xlab("Emotional Regulation")

ggpredict(mlm_avg, terms = c("BFI_Collaboration","SES_Group", "City"),
          type="re") %>% 
  plot()+
  scale_color_manual(values = c("red", "green", "blue"), 
                     breaks = c("Bottom 40%", "Middle 50%", "Top 10%"))+
  theme_bw()+
  theme(legend.position = c(0.85,0.13),
        legend.background = element_rect(#fill="gray",
          size=0.5, linetype="solid", 
          colour ="black")) +
  ylim(0,1)+
  ylab("Probability of High Performance in\n Reading and hanguage")+
  xlab("Collaboration")













































#OLD
##### high achievement
mlm_ha1 = lmer(Read_Top25 ~ Sex + SES_Group + CohortID + factor(IMMBACK) +
                         BFI_Open_Mindedness + BFI_Task_Performance + BFI_Engaging_with_Others +
                         BFI_Collaboration + BFI_Emotional_Regulation + 
                         (1|City/SchID)  + (1| CohortID) + (1 + SES_Group|City) +
                         (1 + BFI_Open_Mindedness + BFI_Task_Performance + 
                            BFI_Engaging_with_Others +
                            BFI_Collaboration + BFI_Emotional_Regulation || City:SES_Group) ,
                       REML = F, control = lmerControl(optimizer ="Nelder_Mead"),
                       data = model_data_reading)

mlm_reading_ha2 = lmer(Read_Top25 ~ Sex + SES_Group + CohortID + factor(IMMBACK) +
                             BFI_Open_Mindedness + BFI_Task_Performance + BFI_Engaging_with_Others +
                             BFI_Collaboration + BFI_Emotional_Regulation + 
                             (1|City/SchID)  + (1| CohortID) + 
                             (1 + BFI_Open_Mindedness + BFI_Task_Performance + 
                                BFI_Engaging_with_Others +
                                BFI_Collaboration + BFI_Emotional_Regulation || City:SES_Group) ,
                           REML = F, control = lmerControl(optimizer ="Nelder_Mead"),
                           data = model_data_reading)

summary(mlm_ses_reading_int)

ranef(mlm_ses_reading_int)

coef(mlm_ses_reading_int1)

anova(mlm_ses_reading_int, mlm_ses_reading_int1)

sjPlot::tab_model(mlm_ses_reading_int1, show.icc = T, 
                  show.r2 = T, show.re.var = T)

lmerTest::ranova(mlm_ses_reading_int1)


ggpredict(mlm_ses_reading_int1, terms = c("BFI_Task_Performance","SES_Group", "City"),
          type="re") %>% 
  plot()+
  scale_color_manual(values = c("red", "green", "blue"))+
  theme_bw()+
  theme(legend.position = c(0.85,0.13),
        legend.background = element_rect(#fill="gray",
                                         size=0.5, linetype="solid", 
                                         colour ="black")) +
  ylim(0,1)+
  ylab("Probability of High Performance in\n Reading and Language")+
  xlab("Task Performance")


ggpredict(mlm_ses_reading_int1, terms = c("BFI_Open_Mindedness","SES_Group", "City"),
          type="re") %>% 
  plot()+
  scale_color_manual(values = c("red", "pink", "green", "lightblue", "blue"))+
  theme_bw()+
  theme(legend.position = c(0.85,0.13),
        legend.background = element_rect(fill="gray",
                                         size=0.5, linetype="solid", 
                                         colour ="darkblue")) +
  ylim(0,1)+
  ylab("Probability of High Performance in\n Reading and Language")+
  xlab("Open Mindedness")


####### Math

model_data_math =
  students %>%
  select(starts_with("BFI"), SchID, SiteID, Cohort, CohortID, Sex,  SES_Quintile, City, IMMBACK,
         Sgrade_Math, Math_Top) %>%
  drop_na()


mlm_ses_math_int = lmer(Math_Top ~ Sex + SES_Quintile + CohortID + factor(IMMBACK) +
                             BFI_Open_Mindedness + BFI_Task_Performance + BFI_Engaging_with_Others +
                             BFI_Collaboration + BFI_Emotional_Regulation + 
                             (1|City/SchID)  + (1| CohortID) + 
                             (1 + BFI_Open_Mindedness + BFI_Task_Performance + BFI_Engaging_with_Others +
                                BFI_Collaboration + BFI_Emotional_Regulation || City:SES_Quintile) ,
                           REML = F, control = lmerControl(optimizer ="Nelder_Mead"),
                           data = model_data_math)


summary(mlm_ses_math_int)

coef(mlm_ses_math_int)$`City:SES_Quintile`


ggpredict(mlm_ses_math_int, terms = c("BFI_Task_Performance","SES_Quintile", "City"),
          type="re") %>% 
  plot()+
  scale_color_manual(values = c("red", "pink", "green", "lightblue", "blue"))+
  theme_bw()+
  theme(legend.position = c(0.85,0.13),
        legend.background = element_rect(fill="gray",
                                         size=0.5, linetype="solid", 
                                         colour ="darkblue")) +
  ylim(0,1)+
  ylab("Pribability of High Performance in\n Math")+
  xlab("Task Performance")


ggpredict(mlm_ses_math_int, terms = c("BFI_Engaging_with_Others","SES_Quintile", "City"),
          type="re") %>% 
  plot()+
  scale_color_manual(values = c("red", "pink", "green", "lightblue", "blue"))+
  theme_bw()+
  theme(legend.position = c(0.85,0.13),
        legend.background = element_rect(fill="gray",
                                         size=0.5, linetype="solid", 
                                         colour ="darkblue")) +
  ylim(0,1)+
  ylab("Pribability of High Performance in\n Math")+
  xlab("Open Mindedness")



#### assessing model significance
mlm_ses_reading_int1 = lmer(Reading_Top ~ Sex + SES_Quintile + Cohort + factor(IMMBACK) +
                             BFI_Open_Mindedness + BFI_Task_Performance + BFI_Engaging_with_Others +
                             BFI_Collaboration + BFI_Emotional_Regulation + 
                             (1|City/SchID)  + (1| City/Cohort) ,
                             #(1 + BFI_Open_Mindedness + BFI_Task_Performance + BFI_Engaging_with_Others +
                            #    BFI_Collaboration + BFI_Emotional_Regulation || City/SES_Quintile) ,
                           REML = F, control = lmerControl(optimizer ="Nelder_Mead"),
                           data = model_data_reading)

anova(mlm_ses_reading_int1, mlm_ses_reading_int)
