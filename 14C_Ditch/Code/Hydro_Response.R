


#______________________________________________________________________________________________
# Hydrological response, DC sites
ggplot( filter(DC_Q, Treatment %in% c("Ditch cleaning","Clearcut", "Pristine" )),# %>%
       #filter(Site_id %in% c("DC1", "DC2", "DC3", "DC4")),#%>%
         #filter(Study_Source != "Audrey"), 
       aes(y=DOC_14C_Modern, x=q_md, fill=Treatment, color=Treatment))+ #size=DOCmgL_14C, 
  geom_point(size=3, aes(shape=Site_id))+
  scale_shape_manual(values=sites_symbols)+
  scale_fill_manual(values=treatments_colors2)+ #"
  labs(x="specific discharge (m/d)", y=bquote("∆"^14*"C-DOC  (% modern)"), shape="Watershed ID")+
  scale_x_continuous(limits=c(0,0.025))+
  
  geom_smooth(method="lm", se=F, aes(color=Treatment), show.legend = F)+
  scale_color_manual(values=treatments_colors2)+ #"
  stat_regline_equation(
  label.y.npc = "bottom", label.x.npc = 0.30,
  aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~"), color = Treatment), 
  show.legend = F, size=4)+

theme(legend.position = "right")


#Run ANCOVA test
filter(DC_Q, Treatment %in% c("Ditch cleaning","Clearcut", "Pristine" )) %>% 
  anova_test(DOC_14C_Modern ~ q_md*Treatment)


   # stat_ellipse(aes(color=Treatment))
  #+
  #geom_smooth(method=lm, se=F)+
  #facet_wrap(~Site_id, scale="fixed")


# Keeling response, DC sites
library(ggpmisc) 

write.csv(filter(DC1_DC3, CO2_14C_Modern!= "NA"), "test.csv")


ggplot(filter(DC1_DC3, Treatment!= "NA"), 
       aes(y=CO2_14C_Modern, x=CO2_mgL_MW, color=Treatment))+
  #scale_x_log10()+
  geom_point(size=4, aes(shape=Site_id))+
  scale_color_manual(values=c( "#fc9272ff", "#de2d26ff","black"))+ 
  
  geom_smooth(method="lm", se=F, aes(color=Treatment), show.legend = F)+
  
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., ..p.value.label.., sep = "*` `~~~~")), 
               parse = TRUE,
               label.x.npc = "right",
               vstep = 0.05) # sets vertical spacing
  
  

# Histogram of Q per treatment
library(ggridges)

ggplot(DC1_DC3, aes( y=CO2_14C_Modern, x=Treatment))+
  geom_violin()

  #scale_x_log10()+
  #facet_wrap(~DC1$)

ggplot(Q_DC3_Meteo)+
  geom_histogram(aes( y=q_md),alpha=0.5)+
  #scale_x_log10()+
  facet_wrap(~as.factor(lubridate::year(Q_DC3_Meteo$Date)))


ggplot(Q_DC3_Meteo, 
       aes( y=q_md, x=lag(P,1))+
  geom_point(alpha=0.5, show.legend=F)+
  #scale_y_log10()+
  facet_wrap(~as.factor(lubridate::year(Q_DC3_Meteo$Date)))

library(forecast)
ggCcf(
  x=DC3[which(DC3$Treatment=="After clearcut"), "P"],
  y=DC3[which(DC3$Treatment=="After clearcut"), "q_md"],
  lag.max = NULL,
  type = c("correlation"),
  plot = TRUE,
  na.action = na.contiguous)

ggCcf(
  x=DC3[which(DC3$Treatment=="After clearcut & Ditch cleaning"), "P"],
  y=DC3[which(DC3$Treatment=="After clearcut & Ditch cleaning"), "q_md"],
  lag.max = NULL,
  type = c("correlation"),
  plot = TRUE,
  na.action = na.pass)

ggCcf(
  x=DC3[which(DC3$Treatment=="Pristine"), "P"],
  y=DC3[which(DC3$Treatment=="Pristine"), "q_md"],
  lag.max = NULL,
  type = c("correlation", "covariance"),
  plot = TRUE,
  na.action = na.contiguous)

# Histogram of q per Site
ggplot(filter(DC_All_Q, Treatment!= "During Clearcut"), 
       aes( x=q_md, fill=Site_id, color=Site_id))+
  geom_density(alpha=0.5)+
  scale_x_log10()+
  facet_wrap(~Treatment, scale="fixed")

library(tidyverse)

# METEORIC WATER LINE
ggplot(DC3, aes(y=d2H, x=d18O_chem,color=Treatment))+
  geom_point()+
  geom_smooth(method=lm, se=F)+
#global meteoric water line (dashed line:δD=8δ18O+10).
geom_abline(intercept=10,slope=8)

# Long term trend, C18 and C2
ggplot()+
  geom_point(data=C18, aes (x=q_md, y=DOC_14C_Modern, color=Study_Source), fill=1,size=3)+
  geom_smooth(method="lm",data=C18, aes (x=q_md, y=DOC_14C_Modern,color=Study_Source), se=F)+
labs(title= "14C-DOC ~ q relation between study periods at C18")

ggplot()
#scale_x_log10()

ggplot()+
  geom_point(data=C2, aes (x=q_md, y=DOC_14C_Modern, color=Study_Source), fill=1,size=3)+
  geom_smooth(method="lm",data=C18, aes (x=q_md, y=DOC_14C_Modern,color=Study_Source), se=F)+
#scale_x_log10()
labs(title= "14C-DOC ~ q relation between study periods at C2")



ggplot(data=C18, aes(y=DOC_14C_Modern, x=Study_Source))+
  geom_boxplot()+
geom_dotplot(binaxis = "y", stackdir = "center",
               fill = "white")+
  labs(title= "Boxplot 14C-DOC at C18")


ggplot(data=C4, aes(y=DOC_14C_Modern, x=q_md))+
  geom_point()+
  #geom_dotplot(binaxis = "y", stackdir = "center",
  #             fill = "white")+
  labs(title= "Boxplot 14C-DOC at C18")


#df2 %>%
#  mutate(Lag_3 = lag(Value, 3), Lag_6 = lag(Value, 6)) %>% 
#  left_join(df1, ., by = c("Week1" = "Week2")) %>%
#  rename(Lag_0 = Value)

#left_join(filter(C14_wide_chemistry, Site_id == "C18"),
#          Q_C18[,-2],
 #              by=join_by("Date"),
 #              suffix = c(" ", " "))
#filter(C14_wide_chemistry, Site_id == "C18")

plot(x=lag(DC1$q_md,0), y=lag(DC1$d13C_CO2,0))


plot(x=log(lag(C18$q_md,9)), y=lag(C18$DOC_14C_Modern,0))

plot(x=lag(C4$q_md,0), y=lag(C4$DOC_14C_Modern,10))

plot(x=lag(C2$q_md,1), y=lag(C2$DOC_14C_Modern,0))



ggplot()+
  geom_point(data=DC2, aes (x=q_md, y=DOC_14C_Modern), fill=1, shape=21, size=3)+
  geom_smooth(method="lm", data=DC2, aes (x=q_md, y=DOC_14C_Modern), color=1, se=F)+
  
  geom_point(data=DC3, aes (x=q_md, y=DOC_14C_Modern), fill=2, shape=21,size=3 )+
  geom_smooth(method="lm", data=DC3, aes (x=q_md, y=DOC_14C_Modern), color=2, se=F)+
  
  geom_point(data=C2, aes (x=q_md, y=DOC_14C_Modern), fill=3, shape=21, size=3)+
  geom_smooth(method="lm", data=C2, aes (x=q_md, y=DOC_14C_Modern), color=3, se=F)+
  
  geom_point(data=C1, aes (x=q_md, y=DOC_14C_Modern), fill=4, shape=21, size=3)+
  geom_smooth(method="lm", data=C1, aes (x=q_md, y=DOC_14C_Modern), color=4, se=F)+
  
  geom_point(data=C18, aes (x=q_md, y=DOC_14C_Modern), fill=5, shape=21, size=3)+
  geom_smooth(method="lm", data=C18, aes (x=q_md, y=DOC_14C_Modern), color=5, se=F)+
  
  geom_point(data=C4, aes (x=q_md, y=DOC_14C_Modern), fill=6, shape=21, size=3)+
  geom_smooth(method="lm", data=C4, aes (x=q_md, y=DOC_14C_Modern), color=6, se=F)+
  
  
  #geom_point(aes (x=Q, y=), shape=2)+
  #scale_x_log10()+
  scale_x_continuous(limits=c(0,0.02))+
  scale_y_continuous(limits=c(95,115))+
  labs(title="14C Offset increases with Q at C2", subtitle="DC2 (black), DC3(red), C2 (green), C1 (blue), C18 (turquoise), C4 (magenta)")




ggplot(data=DC3)+
  geom_line(aes (x=Date, y=Q+100))+
  geom_point(aes (x=Date, y=CO2_14C_Modern), size=3, color="green")+
  geom_point(aes (x=Date, y=DOC_14C_Modern),size=3, color="orange")+
  #scale_y_continuous(limits=c(95,115))+
  scale_x_date(limits=c(as.Date("2020-01-01"),as.Date("2023-01-01")))+
  labs(title="DC3: Hydrograph with 14C", subtitle="14C-CO2 (green), 14C-DOC (orange), Q+100")


