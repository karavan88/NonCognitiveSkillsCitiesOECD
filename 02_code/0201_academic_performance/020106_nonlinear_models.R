### non-linear model

mlm_ha_nonlinear = lmer(Grades_Top25 ~ Sex + factor(IMMBACK) +
                          BFI_Open_Mindedness + BFI_Task_Performance +
                          BFI_Engaging_with_Others +
                          BFI_Collaboration + I(BFI_Collaboration^2) +
                          BFI_Emotional_Regulation + I(BFI_Emotional_Regulation^2) +
                          (1|City/SchID) + (1|Cohort) +
                          (1 + BFI_Collaboration + I(BFI_Collaboration^2) +
                             BFI_Emotional_Regulation + I(BFI_Emotional_Regulation^2) || City:SES_Group),
                        REML = F, control = lmerControl(optimizer ="Nelder_Mead"),
                        data = model_data)


collab_scatter =
  ggpredict(mlm_ha_nonlinear, terms = c("BFI_Collaboration","SES_Group", "City"),
          type="re") %>% 
  plot()+
  scale_color_manual(values = c("red", "green", "blue"), 
                     breaks = c("Bottom 40%", "Middle 50%", "Top 10%"))+
  theme_bw()+
  theme(legend.position = c(0.85,0.13),
        legend.background = element_rect(#fill="gray",
          size=0.5, linetype="solid", 
          colour ="black")) +
  ylim(0,0.5)+
  ylab("Probability of High Performance")+
  xlab("Collaboration")+
  ggtitle("")
