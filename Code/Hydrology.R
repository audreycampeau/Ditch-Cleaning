# Define operation periods
clearcut_start <- as.Date("2020-07-01")
clearcut_end <- as.Date("2020-08-25")
ditch_cleaning_start <- as.Date("2021-09-01")
ditch_cleaning_end <- as.Date("2021-09-30")

DC_Q$Date
# Create the hydrograph with operations markdowns
ggplot(filter(DC_Q, Treatment %in% c("Ditch cleaning","Clearcut", "Pristine")) %>%
         filter(Site_id %in% c("DC2", "DC3")),
       aes(y = q_md, x = as.Date(Date), color = Site_id)) +
  
  # Add gray background bars for operations
  annotate("rect", 
           xmin = clearcut_start, xmax = clearcut_end,
           ymin = -Inf, ymax = Inf,
           fill = "gray70", alpha = 0.3) +
  
  annotate("rect", 
           xmin = ditch_cleaning_start, xmax = ditch_cleaning_end,
           ymin = -Inf, ymax = Inf,
           fill = "gray70", alpha = 0.3) +
  
  # Add the discharge lines
  geom_line() +
  scale_color_manual(values=site_colors)+ #"
  
  
  # Add black dots for 14C-DOC sampling dates
  geom_point(data = DC_Q %>% 
               filter (Site_id %in% c("DC3")) %>%
               filter(!is.na(DOC_14C_Modern)), 
             aes(x = as.Date(Date), y = rep(0,12)), 
             color = "black", 
             size = 1.5) +
  
  
  # Add text labels for operations
 annotate("text", 
           x = clearcut_start + (clearcut_end - clearcut_start)/2,
           y = Inf, 
           label = "Forest clear cut operations\n(July 2020)",
           vjust = 1.2, hjust = 0.5,
           size = 3.5, color = "black") +
  
  annotate("text", 
           x = ditch_cleaning_start + (ditch_cleaning_end - ditch_cleaning_start)/2,
           y = Inf, 
           label = "Ditch cleaning operations\n(September 2021)",
           vjust = 1.2, hjust = 0.5,
           size = 3.5, color = "black") +
  
  labs(y = "Specific discharge (m/d)", 
       x = "Date",
       color = "Stream") +
  theme(legend.position = "top")
  

#______________________________________________________________________________________________
library(ggpubr)

DC2_DC3_wide=filter(DC_Q, Site_id %in% c("DC2", "DC3")) %>%
  select(Date, Site_id, P, TA, q_md, Treatment) %>%  # Select only necessary columns
  pivot_wider(names_from = Site_id, 
              values_from = q_md,
              names_prefix = "q_md_",
              values_fn = mean)  # Take mean of multiple values)#%>%
glimpse(DC2_DC3_wide)


ggplot(data=DC2_DC3_wide,
       aes(x = q_md_DC2*1000, y = q_md_DC3*1000, color = as.factor(lubridate::month(Date))))+
        geom_point(size = 2, alpha = 0.7) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +  # 1:1 line
        #scale_color_manual(values=treatment_colors)+ 
        #facet_wrap(~Treatment)+
  scale_y_continuous(limits=c(0,0.4))+
  scale_x_continuous(limits=c(0,0.4))+
  
        labs(x = "Specific discharge DC2 (mm/d)",
             y = "Specific discharge DC3 (mm/d)",
             color = "Month") +
        theme_minimal() +
        theme(legend.position = "top")










# Hydrological response, 14C-DOC
ggplot( filter(DC_Q, Treatment %in% c("Ditch cleaning","Clearcut", "Pristine" )),# %>%

       aes(y=DOC_14C_Modern, x=q_md, color=Treatment))+ #size=DOCmgL_14C, 
  geom_point( size=3)+ #
  #scale_shape_manual(values=sites_symbols)+
  #scale_fill_manual(values=treatments_colors)+ #"
  #labs(x="DOC (mgCL)", y=bquote("∆"^14*"C-DOC  (% modern)"), shape="Watershed ID")+
  #scale_x_continuous(limits=c(0,0.025))+
  #facet_wrap(~Site_id)+
  geom_smooth(method="lm", se=F, aes(color=Treatment), show.legend = F)+
  scale_color_manual(values=treatment_colors)+ #"
  stat_regline_equation(
  label.y.npc = "top", label.x.npc = 0.5,
  aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~"), color = Treatment), 
  show.legend = F, size=4)+
theme_minimal()+
theme(legend.position = "right")


#Run ANCOVA test
filter(DC_Q, Treatment %in% c("Ditch cleaning","Clearcut", "Pristine" )) %>% 
  anova_test(DOC_14C_Modern ~ q_md*Treatment)



#Hydroresponse of C concentrations____________________________________________________________


ggplot( filter(DC_Q, Treatment %in% c("Ditch cleaning","Clearcut", "Pristine" )),# %>%
        aes(y=DOC_mgL, x=q_md, fill=Treatment, color=Treatment))+ #size=DOCmgL_14C, 
  geom_point(size=3, aes(shape=Site_id))+
  scale_shape_manual(values=sites_symbols)+
  scale_fill_manual(values=treatments_colors2)+ #"
  labs(x="specific discharge (m/d)", y=bquote("DOC (mg C L"^-1*")"), shape="Watershed ID")+
  #scale_x_log10()+
  facet_wrap(~Site_id)+
  
  geom_smooth(method="lm", se=F, aes(color=Treatment), show.legend = F)+
  scale_color_manual(values=treatments_colors2)+ #"
  stat_regline_equation(label.y.npc = "top", label.x.npc = 0.30,
                        aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~"), color = Treatment), 
                        show.legend = F, size=4)+
  
  
  theme(legend.position = "right")


ggplot( filter(DC_Q, Treatment %in% c("Ditch cleaning","Clearcut", "Pristine" )),# %>%
        aes(y=CO2_mgL, x=q_md, fill=Treatment, color=Treatment))+ #size=DOCmgL_14C, 
  geom_point(size=3, aes(shape=Site_id))+
  scale_shape_manual(values=sites_symbols)+
  scale_fill_manual(values=treatments_colors2)+ #"
  labs(x="specific discharge (m/d)", y=bquote("CO2 (mg C L"^-1*")"), shape="Watershed ID")+
  #scale_x_log10()+
  facet_wrap(~Site_id)+
  
  geom_smooth(method="lm", se=F, aes(color=Treatment), show.legend = F)+
  scale_color_manual(values=treatments_colors2)+ #"
  stat_regline_equation(label.y.npc = "top", label.x.npc = 0.30,
                        aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~"), color = Treatment), 
                        show.legend = F, size=4)+
  
  
  theme(legend.position = "right")






ggplot(filter(DC_Q, Treatment %in% c("Ditch cleaning","Clearcut", "Pristine" )),
       
       aes(fill=Treatment, 
           y=DOC_14C_Modern-CO2_14C_Modern, 
           x=Treatment))+
  geom_violin(alpha=0.5)+
  geom_jitter(aes(shape=Site_id), width=0.2, size=3) +
  #geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 0.3)+
  scale_fill_manual(values=c(treatments_colors))+
  scale_shape_manual(values=sites_symbols)+ 
  #facet_wrap(~Carbon_specie, scales = "fixed")+
  ggtitle("No change in 14C-gap between CO2 and DOC with treatments")+
  theme_bw(base_size = 12)





# Keeling response, DC sites
library(ggpmisc) 



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


