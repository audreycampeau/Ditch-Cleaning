
library(tidyverse)
library(ggpubr)# show reg.line equation 

# Make a hydrograph showing treatments and sampling periods

# Define operation periods for treatments 
clearcut_start <- as.Date("2020-07-01")
clearcut_end <- as.Date("2020-08-25")
ditch_cleaning_start <- as.Date("2021-09-01")
ditch_cleaning_end <- as.Date("2021-09-30")


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
  







ggplot(DC_Q %>%
         filter(Site_id %in% c("DC1")),
       aes(y = P, x = as.Date(Date), color = Site_id)) +
  
  # Add gray background bars for operations
  annotate("rect", 
           xmin = clearcut_start, xmax = clearcut_end,
           ymin = -Inf, ymax = Inf,
           fill = "gray70", alpha = 0.3) +
  
  annotate("rect", 
           xmin = ditch_cleaning_start, xmax = ditch_cleaning_end,
           ymin = -Inf, ymax = Inf,
           fill = "gray70", alpha = 0.3) +
  
  geom_bar(stat = 'identity', fill=Site_id) +
  labs( y= "Precipitation (mm/day)") +
  scale_y_reverse()+
  scale_color_manual(values=site_colors_6)+ #"
  scale_x_date(limits=c(as.Date("2020-01-01"), as.Date("2022-11-01")),
               date_labels = "%Y", date_breaks = "1 year") +
  
  
  labs(y = "q (mm/d)", 
       x = "Date",
       color = "Stream") +
  theme(legend.position = "top", axis.title.x = element_blank())


#______________________________________________________________________________________________
library(ggpubr)

# Is specific discharge at DC2 adn DC3 related?

DC2_DC3_wide=filter(DC_Q, Site_id %in% c("DC2", "DC3")) %>%
  select(Date, Site_id, P, TA, q_md, Treatment) %>%  # Select only necessary columns
  pivot_wider(names_from = Site_id, 
              values_from = q_md,
              names_prefix = "q_md_",
              values_fn = mean)  # Take mean of multiple values)#%>%


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







#______________________________________________________________________________
# Hydrological control over CO2 concentration

ggplot( DC_Q, #filter(DC_Q, Site_id %in% c("DC1", "DC3")), #filter(DC_Q, Treatment %in% c("Ditch cleaning","Clearcut", "Pristine" )),# %>%
        aes(y=CO2_mgL, x=q_md_filled, fill=Treatment, color=Treatment))+ #size=DOCmgL_14C, 
  geom_point(size=3, aes(shape=Site_id))+
  scale_shape_manual(values=site_symbols)+
  scale_fill_manual(values=treatment_colors)+ #"
  labs(x="specific discharge (m/d)", y=bquote("CO2 (mg C L"^-1*")"), shape="Watershed ID")+
  scale_x_log10()+
  scale_y_log10()+
  
  facet_wrap(~Site_id)+
  
  geom_smooth(method="lm", se=F, aes(color=Treatment), show.legend = F)+
  scale_color_manual(values=treatment_colors)+ #"
  stat_regline_equation(label.y.npc = "top", label.x.npc = "left", #0.30
                        aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~"), color = Treatment), 
                        show.legend = F, size=4)+
  
  
  theme(legend.position = "right")




# Keeling response, DC sites
library(ggpmisc) 

ggplot(DC_Q, 
       aes(y=d13C_CO2, x=1/CO2_mgL_filled, color=Treatment))+
  #scale_x_log10()+
  geom_point(size=4, aes(shape=Site_id))+
  scale_color_manual(values=treatment_colors)+ 
  
  geom_smooth(method="lm", se=F, aes(color=Treatment), show.legend = F)+
  
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., ..p.value.label.., sep = "*` `~~~~")), 
               parse = TRUE,
               label.x.npc = "right",
               vstep = 0.05) # sets vertical spacing
  
  




# Histogram of Q per treatment
library(ggridges)

ggplot(DC_Q, aes( y=CO2_14C_Modern, x=Site_id))+
  geom_violin()+
  stat_bin2d()

ggplot(DC_Q, aes( y=DOC_14C_Modern, x=Site_id))+
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

