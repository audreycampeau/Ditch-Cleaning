

# Scatter with marginal histograms
# Test relationships, difference between relationship, and differences in distributions.





#Install Libraries
install.packages("ggExtra")
library(ggExtra)
library("ggplot2")

library(tidyverse)
library(ggpubr)
library(rstatix)
library(broom)

theme_set(theme_bw(base_size = 16))

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Hydrological Response::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# pH
  ggplot(data=filter(Chemistry, Treatment !="Clearcut"), 
       aes(y=Norg_mgL, x=DOC_mgL_chem, color=Treatment,fill=Treatment))+
  scale_x_log10()+
  geom_point( shape=21,size=3)+
  scale_fill_manual(values=c( "#fc9272ff", "#de2d26ff","#fee0d2ff"))+ 
  scale_color_manual(values=rep( "black", 3))+  
  geom_smooth(method="lm", color="black", se=F, show.legend = F)+
  #labs(x="q (m/d)", y="pH")+
  theme(legend.position="top")+
  facet_wrap(~Site_id, scale="fixed")+
  stat_regline_equation(
    #label.x = rep(30,3), label.y = c(102.5, 101.5, 100.5),
    aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")), 
    #position = "jitter", 
    show.legend = F, size=3)
  
  # [DOC] ~ 14C-DOC
  ggplot(data=filter(DC_Q, Treatment !="Clearcut"), 
         aes(y=DOC_14C_Modern, x=q_md,color=Treatment,fill=Treatment
            ))+
    scale_x_log10()+
    geom_point( aes(),shape=21,size=3)+
    scale_fill_manual(values=c( "#fc9272ff", "#de2d26ff","#fee0d2ff"))+ 
   # geom_abline(intercept=0, slope=1)+
    scale_color_manual(values=rep( "black", 3))+  
    geom_smooth(method="lm", color="black", se=F, show.legend = F)+
    #labs(x="q (m/d)", y="DOC (mgC/L)")+
    theme(legend.position="top")+
    #facet_wrap(~Site_id, scale="fixed")+
    stat_regline_equation(
      #label.x = rep(30,3), label.y = c(102.5, 101.5, 100.5),
      aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")), 
      position = "jitter", show.legend = F, size=3)
  
  summary(lm(DOC_14C_Modern~log(q_md), data=filter(DC1_DC3, 
                                           # Treatment =="After clearcut" & 
                                              Site_id=="DC3" )))
  ?stat_regline_equation
  
  #Run ANCOVA test
  filter(DC1_DC3, Treatment !="During Clearcut") %>% 
    anova_test(DOC_14C_Modern ~ DOCmgL_14C*Treatment)
  
  
  # [DOC] ~ 14C-DOC::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ggplot(data=filter(DC1_DC3, Treatment !="During Clearcut"), 
         aes(y=DOC_mgL_chem, x=q_md, 
             color=Treatment,fill=Treatment))+
    #scale_x_log10()+
    geom_point( shape=21,size=3, color="black")+
    scale_fill_manual(values=c( "#fc9272ff", "#de2d26ff","skyblue"))+ 
    # geom_abline(intercept=0, slope=1)+
    #scale_color_manual(values=rep( "black", 3))+  
    geom_smooth(method="lm", color="black", se=F, show.legend = F)+
    #labs(x="q (m/d)", y="DOC (mgC/L)")+
    theme(legend.position="top")+
    facet_wrap(~Site_id, scale="fixed")+
    stat_regline_equation(
      #label.x = rep(10,3), 
      #label.y = c(102.5, 101.5, 100.5),
      aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~"), color = Treatment), 
      #position = "jitter",
      show.legend = F, size=3)
  
  #Run ANCOVA test
  filter(DC1_DC3, Treatment !="During Clearcut") %>% 
    anova_test(q_md ~ Treatment)
  
  ggplot(Q_DC3)+
    geom_violin(aes(x=as.factor(lubridate::year(Date)), y=q_md))
  
  ggplot(Q_DC3)+
    geom_line(aes(x=Date, y=q_md))
  
  
  
  # [PO4]
  ggplot(data=filter(DC1_DC3, Treatment !="During Clearcut"), 
         aes(y=PO4_ugL_chem/1000, x=DOC_mgL_chem, color=Treatment,fill=Treatment))+
    #scale_x_log10()+
    geom_point( shape=21,size=3)+
    scale_fill_manual(values=c( "#fc9272ff", "#de2d26ff","#fee0d2ff"))+ 
    scale_color_manual(values=rep( "black", 3))+  
    geom_smooth(method="lm", color="black", se=F, show.legend = F)+
    #labs(x="q (m/d)", y="PO4 (mgC/L)")+
    theme(legend.position="top")+
    facet_wrap(~Site_id, scale="fixed")+
    stat_regline_equation(
      #label.x = rep(10,3), 
      #label.y = c(102.5, 101.5, 100.5),
      aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~"), color = Treatment), 
      #position = "jitter",
      show.legend = F, size=3)
  
   
  # [Inorg N]
  ggplot(data=filter(DC1_DC3, Treatment !="During Clearcut"), 
         aes(y=(NH4_ugL_chem+NO2_NO3_ugL_chem)/1000, x=q_md, color=Treatment, fill=Treatment))+
    scale_x_log10()+
    geom_point( shape=21,size=3)+
    scale_fill_manual(values=c( "#fc9272ff", "#de2d26ff","#fee0d2ff"))+ 
    scale_color_manual(values=rep( "black", 3))+  
    geom_smooth(method="lm", color="black", se=F, show.legend = F)+
    labs(x="q (m/d)", y="Inog.N (mgC/L)")+
    theme(legend.position="top")+
    facet_wrap(~Site_id, scale="fixed")
  
  
  # [C:N]
  ggplot(data=filter(DC1_DC3, Treatment !="During Clearcut"), 
         aes(y=DN_mgL_chem/DOC_mgL_chem, 
             x=q_md, color=Treatment,fill=Treatment))+
    scale_x_log10()+
    geom_point( shape=21,size=3)+
    scale_fill_manual(values=c( "#fc9272ff", "#de2d26ff","#fee0d2ff"))+ 
    scale_color_manual(values=rep( "black", 3))+  
    geom_smooth(method="lm", color="black", se=F, show.legend = F)+
    labs(x="q (m/d)", y="C:N")+
    theme(legend.position="top")+
    facet_wrap(~Site_id, scale="fixed")
  
  
  # 14C-DOC
  ggplot(data=filter(DC1_DC3, Treatment !="During Clearcut"), 
         aes(y=DOC_14C_Modern, x=q_md, color=Treatment,fill=Treatment))+
    scale_x_log10()+
    geom_point( shape=21,size=3)+
    scale_fill_manual(values=c( "#fc9272ff", "#de2d26ff","#fee0d2ff"))+ 
    scale_color_manual(values=rep( "black", 3))+  
    geom_smooth(method="lm", color="black", se=F, show.legend = F)+
    labs(x="q (m/d)", y="DOC (mgC/L)")+
    theme(legend.position="top")+
    facet_wrap(~Site_id, scale="fixed")
  
  ggplot(data=filter(DC1_DC3, Treatment !="During Clearcut"), 
         aes(y=CO2_14C_Modern, x=q_md, color=Treatment,fill=Treatment))+
    scale_x_log10()+
    geom_point( shape=21,size=3)+
    scale_fill_manual(values=c( "#fc9272ff", "#de2d26ff","#fee0d2ff"))+ 
    scale_color_manual(values=rep( "black", 3))+  
    geom_smooth(method="lm", color="black", se=F, show.legend = F)+
    labs(x="q (m/d)", y="DOC (mgC/L)")+
    theme(legend.position="top")+
    facet_wrap(~Site_id, scale="fixed")
  
 #[CO2]
   ggplot(data=filter(DC1_DC3, Treatment !="During Clearcut"), 
         aes(y=CO2_mgL_chem, x=q_md, color=Treatment,fill=Treatment))+
    scale_x_log10()+
    geom_point( shape=21,size=3)+
    scale_fill_manual(values=c( "#fc9272ff", "#de2d26ff","#fee0d2ff"))+ 
    scale_color_manual(values=rep( "black", 3))+  
    geom_smooth(method="lm", color="black", se=F, show.legend = F)+
    labs(x="q (m/d)", y="DOC (mgC/L)")+
    theme(legend.position="top")+
    facet_wrap(~Site_id, scale="fixed")
  
   
   #[dexcess]
   ggplot(data=filter(DC1_DC3, Treatment !="During Clearcut"), 
          aes(y=dexcess, x=q_md, color=Treatment,fill=Treatment))+
     scale_x_log10()+
     geom_point( shape=21,size=3)+
     scale_fill_manual(values=c( "#fc9272ff", "#de2d26ff","#fee0d2ff"))+ 
     scale_color_manual(values=rep( "black", 3))+  
     geom_smooth(method="lm", color="black", se=F, show.legend = F)+
     labs(x="q (m/d)", y="dexcess")+
     theme(legend.position="top")+
     facet_wrap(~Site_id, scale="fixed")
   
   #[d18O]
   ggplot(data=filter(DC1_DC3, Treatment !="During Clearcut"), 
          aes(y=d18O_chem, x=q_md, color=Treatment,fill=Treatment))+
     scale_x_log10()+
     geom_point( shape=21,size=3)+
     scale_fill_manual(values=c( "#fc9272ff", "#de2d26ff","#fee0d2ff"))+ 
     scale_color_manual(values=rep( "black", 3))+  
     geom_smooth(method="lm", color="black", se=F, show.legend = F)+
     labs(x="q (m/d)", y="dexcess")+
     theme(legend.position="top")+
     facet_wrap(~Site_id, scale="fixed")
   
as.factor(DC_All_Q$Site_id)
      ggplot(data=filter(DC_All_Q, Site_id %in% c("DC1", "DC2", "DC3", "DC4")) %>% 
               filter(Treatment !="During Clearcut")%>%
              filter(Study_Source !="Audrey"), 
          aes(y=DOC_14C_Modern, x=DOCmgL_14C, color=Treatment))+
     geom_point(aes(fill=Treatment,shape=Site_id),size=6,color="black", show.legend=T)+
     scale_fill_manual(values=c( "#fc9272ff", "#de2d26ff", "#fee0d2ff"))+ 
        scale_shape_manual(values=c(21,24,22,23))+
     geom_smooth(method="lm", se=T, show.legend = F)+
     #stat_ellipse(show.legend=F)+ #aes(color=Treatment),
     scale_color_manual(values=c( "#fc9272ff", "#de2d26ff", "grey30"))+  
     theme(legend.position="top")+
        stat_regline_equation(
          label.x = rep(0.02,3), 
          label.y = c(102.5, 101.5, 100.5),
          aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~"), color = Treatment), 
          #position = "jitter",
          show.legend = F, size=4)
      
    #stat_cor(#label.y=115, 
    #   label.y.npc = "bottom", label.x.npc = "left",
     #  method = "pearson", cor.coef.name = "R",
     #  aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"),color=Treatment),
     #  r.accuracy = 0.01, p.accuracy = 0.01,show.legend=F
     # )#+
     #facet_wrap(~Site_id)
     
     
     data_cor_test=DC1_DC3 %>% 
                    #filter(Site_id %in% "DC1") %>%
                     filter(Treatment %in% c("After clearcut & Ditch cleaning", "After clearcut")) #, "After clearcut"
    
     data_cor_test
     # library(broom)
     #DC1_DC3 %>% 
      # filter(Site_id %in% "DC3") %>%
       #               do(tidy(cor.test(.$DOCmgL_14C, .$DOC_14C_Modern,
        #              method = c("kendall") )))
   
  cor.test(x=log(data_cor_test$q_md), 
                    y=data_cor_test$DOC_14C_Modern, method="pearson",use = "complete.obs")

shapiro.test(data_cor_test$DOC_14C_Modern)  

  ggplot(data_cor_test, aes(x=DOC_14C_Modern))+
  geom_density()
