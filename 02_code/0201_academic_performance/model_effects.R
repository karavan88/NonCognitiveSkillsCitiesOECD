###### Model Effects
ggpredict(mlm_la_master, terms = c("BFI_Task_Performance","SES_Group", "City"),
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
  ylab("Probability of Low Performance")+
  xlab("Task Performance")

ggpredict(mlm_la_master, terms = c("BFI_Open_Mindedness","SES_Group", "City"),
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
  ylab("Probability of Low Performance")+
  xlab("Open Mindedness")

ggpredict(mlm_la_master, terms = c("BFI_Engaging_with_Others","SES_Group", "City"),
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
  ylab("Probability of Low Performance")+
  xlab("Engaging with Others")

ggpredict(mlm_la_master, terms = c("BFI_Emotional_Regulation","SES_Group", "City"),
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
  ylab("Probability of Low Performance")+
  xlab("Emotional Regulation")

ggpredict(mlm_la_master, terms = c("BFI_Collaboration","SES_Group", "City"),
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
  ylab("Probability of Low Performance")+
  xlab("Collaboration")