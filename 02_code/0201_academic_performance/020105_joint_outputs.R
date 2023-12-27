##### Produce joint outputs for base and ncs models

# Assuming tidy_mlm1 and tidy_mlm2 are data frames obtained from broom.mixed::tidy() of your models

# Create gtsummary tables for each model
# tbl_mlm_la_base <- 
#   tbl_regression(mlm_la_base, intercept = T,
#                  label = list(
#                    SES_Group = "SES Group", 
#                    `factor(IMMBACK)` = "Migration Background")) %>% 
#   add_glance_table(include = c(nobs)) 
#   
# 
# tbl_mlm_la_ncs  <- tbl_regression(mlm_la_ncs, intercept = T, 
#                                   label = list(
#                                     SES_Group = "SES Group",
#                                     `factor(IMMBACK)` = "Migration Background",
#                                     `BFI_Open_Mindedness` = "Open Mindedness",
#                                     `BFI_Task_Performance` = "Task Performance",
#                                     `BFI_Engaging_with_Others` = "Engaging with Others",
#                                     `BFI_Collaboration` = "Collaboration",
#                                     `BFI_Emotional_Regulation` = "Emotional Regulation"
#                                   )) %>% 
#   add_glance_table(include = c(nobs)) 


tbl_mlm_ha_base <- tbl_regression(mlm_ha_base, intercept = T,
                                  label = list(
                                    SES_Group = "SES Group",
                                    `factor(IMMBACK)` = "Migration Background")) %>% 
  add_glance_table(include = c(nobs)) 

tbl_mlm_ha_ncs  <- tbl_regression(mlm_ha_ncs, intercept = T, 
                                  label = list(
                                    SES_Group = "SES Group",
                                    `factor(IMMBACK)` = "Migration Background",
                                    `BFI_Open_Mindedness` = "Open Mindedness",
                                    `BFI_Task_Performance` = "Task Performance",
                                    `BFI_Engaging_with_Others` = "Engaging with Others",
                                    `BFI_Collaboration` = "Collaboration",
                                    `BFI_Emotional_Regulation` = "Emotional Regulation"
                                  )) %>% 
  add_glance_table(include = c(nobs)) 


#Merge the tables
tbl_merge <- 
  tbl_merge(
  list(#tbl_mlm_la_base, tbl_mlm_la_ncs, 
       tbl_mlm_ha_base, tbl_mlm_ha_ncs),
  tab_spanner = c(
    #"**Low Performance:<br>Model without NCS**",
    #"**Low Performance:<br>Model with NCS**",
    "**Model without NCS**",
    "**Model with NCS**"
  )
) %>%
  modify_caption("Multilevel regressions on probability of high achievement, before and after accounting for non-cognitive skills")

##### Produce joint outputs for master mlm
# mlm_la_master_tidy = 
#   tidy(mlm_la_master) %>% 
#   mutate_if(is.numeric, round, 3) %>%
#   mutate(estimate = ifelse(!is.na(std.error), glue("{estimate} ({std.error})"), estimate),
#          effect = ifelse(effect == "ran_pars", "random", effect)) %>%
#   select(effect, group, term, estimate, p.value)

mlm_ha_master_tidy = 
  tidy(mlm_ha_master) %>% 
  mutate_if(is.numeric, round, 3) %>%
  mutate(estimate = ifelse(!is.na(std.error), glue("{estimate} ({std.error})"), estimate),
         effect = ifelse(effect == "ran_pars", "random", effect)) %>%
  select(effect, group, term, estimate, p.value)

# common_cols <- intersect(names(mlm_la_master_tidy), names(mlm_ha_master_tidy))
# 
# # For each common column, check if the values are the same
# # If they are the same, we will drop them from df2 before binding
# for (col in common_cols) {
#   if (identical(mlm_la_master_tidy[[col]], mlm_ha_master_tidy[[col]])) {
#     mlm_ha_master_tidy <- mlm_ha_master_tidy %>% select(-all_of(col))
#   }
# }

# Now bind the columns. df2 will only have unique or different columns
# mlm_master_tidy <- bind_cols(mlm_la_master_tidy, mlm_ha_master_tidy)


# mlm_master_tidy =
#   mlm_la_master_tidy %>%
#   bind_cols(mlm_ha_master_tidy) 

vpc = 
  mlm_ha_master_tidy %>%
  #select(1,2,3,4,6) %>%
  filter(effect == "random") %>%
  mutate(#`Low Performance` = as.numeric(estimate...4),
         VPC = as.numeric(estimate)) %>%
  mutate(Effect = case_when(group %in% c("SchID.City", "City",  "Cohort") ~ "Random Intercept" ,
                            str_detect(group, "City.SES_Group") ~ "Random Slope",
                            TRUE ~ "Random")) %>%
  mutate(Group = case_when(group == "SchID.City" ~ "City * School ID",
                           str_detect(group, "City.SES_Group") ~ "City * SES Group",
                           T ~ group),
         Term = str_replace(term, "__", " "),
         Term = str_replace(Term, "BFI", ""),
         Term = str_replace(Term, "_", "")
         ) %>%
  mutate(Term = str_replace(Term, "_", " ")) %>%
  mutate(Term = str_replace(Term, "_", " ")) %>%
  select(Effect, Group, Term, VPC
         #`Low Performance`, 
         #`High Performance`
         ) %>%
  mutate(#`Variance: Low Performance` = `Low Performance`*`Low Performance`,
         `Variance: High Performance` = VPC * VPC,
         #`Explained Variance: Low Performance` = `Variance: Low Performance`/sum(`Variance: Low Performance`)*100,
         `Explained Variance (%)` = `Variance: High Performance`/sum(`Variance: High Performance`)*100
         ) %>%
  select(-`Variance: High Performance`) %>%
  mutate_if(is.numeric,round, 3)

vpc_table = 
  gt(vpc, 
     caption = "Table 5. Variance Particioning Coefficients of the Multilevel Model.") %>%
  tab_source_note(
    source_note = "Source: Calculations of the authors based on the OECD SSES 2019 data."
  )


#View(mlm_master_tidy)

# another way to prepare a better table 
# mlm_la_master_table = 
#   tbl_regression(mlm_la_master, intercept = T, 
#                  label = list(
#                    SES_Group = "SES Group",
#                    `factor(IMMBACK)` = "Migration Background",
#                    `BFI_Open_Mindedness` = "Open Mindedness",
#                    `BFI_Task_Performance` = "Task Performance",
#                    `BFI_Engaging_with_Others` = "Engaging with Others",
#                    `BFI_Collaboration` = "Collaboration",
#                    `BFI_Emotional_Regulation` = "Emotional Regulation"
#                  )) %>% 
#   add_glance_table(include = c(nobs)) 



mlm_ha_master_table <- 
  tbl_regression(mlm_ha_master, intercept = TRUE, 
                                     label = list(
                                       SES_Group = "SES Group",
                                       `factor(IMMBACK)` = "Migration Background",
                                       `BFI_Open_Mindedness` = "Open Mindedness",
                                       `BFI_Task_Performance` = "Task Performance",
                                       `BFI_Engaging_with_Others` = "Engaging with Others",
                                       `BFI_Collaboration` = "Collaboration",
                                       `BFI_Emotional_Regulation` = "Emotional Regulation"
                                     )) %>% 
  add_glance_table(include = c(nobs)) %>%
  modify_caption("Multilevel regressions of probability of high achievement 
                 with random slopes of non-cognitive skills") 


# Merge the tables
# mlm_master_table <- tbl_merge(
#   list(mlm_la_master_table, mlm_ha_master_table),
#   tab_spanner = c("**Low Performance**", "**High Performance**")
# )


# Print the updated table
# mlm_master_table
