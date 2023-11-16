# MLM
##### low achievement #####
mlm_la_base = lmer(Grades_Bottom25 ~ Sex + SES_Group  + factor(IMMBACK) +
                     (1|City/SchID)  + (1| Cohort) + (1 + SES_Group|City) ,
                   REML = F, control = lmerControl(optimizer ="Nelder_Mead"),
                   data = model_data)

mlm_la_ncs = lmer(Grades_Bottom25 ~ Sex +  factor(IMMBACK) + SES_Group +
                       BFI_Open_Mindedness + BFI_Task_Performance + BFI_Engaging_with_Others +
                       BFI_Collaboration + BFI_Emotional_Regulation + 
                       (1|City/SchID)  + (1| Cohort) + (1 + SES_Group|City) ,
                     REML = F, control = lmerControl(optimizer ="Nelder_Mead"),
                     data = model_data)


##### compare two models

#carry out the anova test for the oberall comparison
model_comparison = anova(mlm_la_base, mlm_la_ncs)

# Extract the regression coefficients of SES_Group random slopes by City from each model
la_Model1_RE = coef(mlm_la_base)$City["SES_GroupBottom 40%"] %>% as.data.frame() %>%
  rownames_to_column() %>% mutate(Model = "Model without NCS")

la_Model2_RE = coef(mlm_la_ncs)$City["SES_GroupBottom 40%"] %>% as.data.frame() %>% 
  rownames_to_column() %>% mutate(Model = "Model with NCS")

la_Model1_FE = unique(coef(mlm_la_base)$Cohort["SES_GroupBottom 40%"]) %>% as.data.frame() %>% 
  mutate(rowname = "Average Fixed Effect", Model = "Model without NCS")

la_Model2_FE = unique(coef(mlm_la_ncs)$Cohort["SES_GroupBottom 40%"]) %>% as.data.frame() %>% 
  mutate(rowname = "Average Fixed Effect", Model = "Model with NCS")

la_coef_df = 
  la_Model1_FE %>%
  bind_rows(la_Model1_RE, la_Model2_FE, la_Model2_RE)

rownames(la_coef_df) <- NULL

la_diff_mod_comp = 
  la_coef_df %>%
  spread(Model, `SES_GroupBottom 40%`) %>%
  mutate(diff = (`Model without NCS` - `Model with NCS`)*100) %>%
  arrange(diff)

# Plot the coefficients as a geom_point plot
la_mlm_comparison = 
  ggplot(la_coef_df, aes(x = `SES_GroupBottom 40%`, y = fct_reorder(rowname, desc(rowname) ))) +
  geom_point(aes(color = Model), size = 7.5) +
  geom_text(aes(label = round(`SES_GroupBottom 40%`,3)*100), size = 3)+
  scale_color_manual(values = c( "lightblue", "#FF9999"))+
  scale_x_continuous(limits = c(-0.008, 0.12), labels = scales::percent_format())+
  labs(x = "Effect of Bottom 40% by SES on Probability of\n Low Academic Performance", y = "City") +
  theme_bw()+
  theme(legend.position = "bottom",
        legend.background = element_rect(size=0.5, linetype="solid", 
                                         colour ="black"))
# master model
mlm_la_master = lmer(Grades_Bottom25 ~ Sex +  factor(IMMBACK) +
                       BFI_Open_Mindedness + BFI_Task_Performance + BFI_Engaging_with_Others +
                       BFI_Collaboration + BFI_Emotional_Regulation + 
                       (1|City/SchID)  + (1| Cohort) + 
                       (1 + BFI_Open_Mindedness + BFI_Task_Performance + 
                          BFI_Engaging_with_Others +
                          BFI_Collaboration + BFI_Emotional_Regulation || City:SES_Group) ,
                     REML = F, control = lmerControl(optimizer ="Nelder_Mead"),
                     data = model_data)

#summary(mlm_la_master)



mlm_la_by_ses =
  coef(mlm_la_master)$`City:SES_Group` %>%
  as.data.frame() %>%
  rownames_to_column(var = "City") %>%
  filter(str_detect(City, "Bottom")) %>%
  select(City, starts_with("BFI")) %>%
  rename(`Open Mindedness` = BFI_Open_Mindedness,
         `Task Performance` = BFI_Task_Performance,
         `Engaging with Others` = BFI_Engaging_with_Others,
         Collaboration = BFI_Collaboration,
         `Emotional Regulation` = BFI_Emotional_Regulation) %>%
  gather(Skill, Estimate, -City) %>%
  mutate(Estimate = Estimate*100,
         City = str_extract(City, "^[^:]+"))


plot_mlm_la_by_ses =
  ggplot(mlm_la_by_ses, aes(Estimate, City))+
  geom_point(size = 7, color = "lightblue")+
  geom_text(aes(label = round(Estimate, 2)), size = 2, color = "black")+
  #geom_segment( aes(x=0, xend=Estimate, y=SES_Quintile, yend=SES_Quintile), color="skyblue")+
  geom_vline(xintercept=0, linetype="solid",  color = "black")+
  theme_bw()+
  scale_y_discrete(limits=rev)+
  facet_wrap(Skill~.,)+# c(2,3))+
  ylab("")+
  xlab("Probability (%) of Low Achievement for Students\n From the Bottom 40% by SES")
