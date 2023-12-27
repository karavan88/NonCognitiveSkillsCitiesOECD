# MLM
##### high achievement #####
mlm_ha_base = lmer(Grades_Top25 ~ Sex + SES_Group  + factor(IMMBACK) +
                     (1|City/SchID)  + (1| Cohort) + (1 + SES_Group|City) ,
                   REML = F, control = lmerControl(optimizer ="Nelder_Mead"),
                   data = model_data)

mlm_ha_ncs = lmer(Grades_Top25 ~ Sex +  factor(IMMBACK) + SES_Group +
                    BFI_Open_Mindedness + BFI_Task_Performance + BFI_Engaging_with_Others +
                    BFI_Collaboration + BFI_Emotional_Regulation + 
                    (1|City/SchID)  + (1| Cohort) + (1 + SES_Group|City) ,
                  REML = F, control = lmerControl(optimizer ="Nelder_Mead"),
                  data = model_data)


##### compare two models
ha_model_comparison = anova(mlm_ha_base, mlm_ha_ncs)

# Extract the regression coefficients of SES_Group random slopes by City from each model
ha_Model1_RE = coef(mlm_ha_base)$City["SES_GroupBottom 40%"] %>% as.data.frame() %>%
  rownames_to_column() %>% mutate(Model = "Model without NCS")

ha_Model2_RE = coef(mlm_ha_ncs)$City["SES_GroupBottom 40%"] %>% as.data.frame() %>% 
  rownames_to_column() %>% mutate(Model = "Model with NCS")

ha_Model1_FE = unique(coef(mlm_ha_base)$Cohort["SES_GroupBottom 40%"]) %>% as.data.frame() %>% 
  mutate(rowname = "Average Fixed Effect", Model = "Model without NCS")

ha_Model2_FE = unique(coef(mlm_ha_ncs)$Cohort["SES_GroupBottom 40%"]) %>% as.data.frame() %>% 
  mutate(rowname = "Average Fixed Effect", Model = "Model with NCS")

ha_coef_df = 
  ha_Model1_FE %>%
  bind_rows(ha_Model1_RE, ha_Model2_FE, ha_Model2_RE)

rownames(ha_coef_df) <- NULL

# this code is to assess where the effect of NCS would be the biggest
# ha_coef_df %>%
#   spread(Model, `SES_GroupBottom 40%`) %>%
#   mutate(diff = `Model without NCS` - `Model with NCS` ) %>%
#   arrange(diff)


# Plot the coefficients as a geom_point plot
ha_mlm_comparison = 
  ggplot(ha_coef_df, aes(x = `SES_GroupBottom 40%`, y = fct_reorder(rowname, desc(rowname) ))) +
  geom_point(aes(color = Model), size = 4.5) +
  geom_text(aes(label = round(`SES_GroupBottom 40%`,3)*100), size = 3)+
  scale_color_manual(values = c( "lightblue", "#FF9999"))+
  scale_x_continuous(limits = c(-0.27, 0.01), labels = scales::percent_format())+
  labs(x = "Effect of Bottom 40% by SES on Probability of\n High Academic Achievement", y = "City") +
  theme_bw()+
  theme(legend.position = "bottom",
        legend.background = element_rect(size=0.5, linetype="solid", 
                                         colour ="black"))


# master model
mlm_ha_master = lmer(Grades_Top25 ~ Sex +  factor(IMMBACK) +
                       BFI_Open_Mindedness + BFI_Task_Performance + BFI_Engaging_with_Others +
                       BFI_Collaboration + BFI_Emotional_Regulation + 
                       (1|City/SchID)  + (1| Cohort) + 
                       (1 + BFI_Open_Mindedness + BFI_Task_Performance + 
                          BFI_Engaging_with_Others +
                          BFI_Collaboration + BFI_Emotional_Regulation || City:SES_Group) ,
                     REML = F, control = lmerControl(optimizer ="Nelder_Mead"),
                     data = model_data)

# summary(mlm_ha_master)


mlm_ha_by_ses =
  coef(mlm_ha_master)$`City:SES_Group` %>%
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

plot_mlm_ha_by_ses =
  ggplot(mlm_ha_by_ses, aes(Estimate, City))+
  geom_point(size = 7, color = "lightblue")+
  geom_text(aes(label = round(Estimate, 2)), size = 2, color = "black")+
  #geom_segment( aes(x=0, xend=Estimate, y=SES_Quintile, yend=SES_Quintile), color="skyblue")+
  geom_vline(xintercept=0, linetype="solid",  color = "black")+
  theme_bw()+
  scale_y_discrete(limits=rev)+
  facet_wrap(Skill~.,)+# c(2,3))+
  ylab("")+
  xlab("Probability (%) of High Achievement for Students\n From the Bottom 40% by SES")


### predcited probability scatter plots
###### Model Effects
task_perf_scatter = 
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
  ylim(0,0.3)+
  ylab("Probability of High Performance")+
  xlab("Task Performance")+
  ggtitle("")

open_scatter = 
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
  ylim(0,0.3)+
  ylab("Probability of High Performance")+
  xlab("Open Mindedness")+
  ggtitle("")


