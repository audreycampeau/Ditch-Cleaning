
library(tidyverse)
theme_set(theme_bw(base_size = 14))


#_________________________________________________
# TIMESERIES
#_________________________________________________

# Timeseries 14C per site, CO2 and DOC seperately
ggplot(data=C14, aes( x=DOY, y=perc_Modern, color=Site_id))+
  geom_line()+
  geom_hline(yintercept=100)+
  geom_point(size=3)+
  facet_grid(Material_Type~Year)

# Timeseries 14C-CO2
ggplot(data= filter(C14, Material_Type == "CO2"), 
       aes(y=perc_Modern, x=Date, color=Site_id))+
  geom_point(size=3)+
  geom_line()+
  facet_wrap(~Site_id,  scales = "fixed")+
  scale_x_date(date_labels = "%b-%y")+
  scale_y_continuous(limits=c(95,112))

#_________________________________________________
# SCATTERPLOTS
#_________________________________________________

# Scatterplot 13C vs. 14C
ggplot(data=C14, aes(x=perc_Modern, y=d13C, fill=Site_id))+
  geom_point(size=3, shape=21)+
  facet_grid(Material_Type~Site_id)+
  stat_ellipse()+
  labs(title="Scatterplot d13C ~ 14C")


# Scatterplot 14C vs. C vol
ggplot(data=C14_wide_chemistry, aes(y=CO2_14C_Modern, x=CO2_mgL_C14, color=Site_id))+
  geom_point(size=3)+
  scale_y_continuous(limits=c(90,116))+
  #facet_wrap(~Material_Type,  scales = "free")+
  facet_wrap(~Site_id,  scales = "free")+
  stat_ellipse()+
  labs( x= "CO2 (mgC/L)", y="14C-CO2 (% modern)", title="Concentration and age relationship for CO2")

# Scatterplot 14C vs. C vol DOC
ggplot(data=DC_database,
       aes(y=EC_uScm, x=pH_chem, fill=Treatment))+
  geom_point( shape=21, size=3)+
  #scale_shape_manual(values=c(1, 21,24))+  #scale_y_continuous(limits=c(90,116))+
  facet_wrap(~Site_id,  scales = "free")+
  stat_ellipse()#+
  #labs( x= "DOC (mg/L)", y="14C-DOC (% modern)", title="Concentration and age relationship for DOC")


ggplot(data=C14_wide_chemistry, aes(y=DOC_14C_Modern, x=CO2_vol_ml_or_DOCmgL_CO2, color=Site_id))+
  geom_point(size=3)+
  #facet_wrap(~Material_Type,  scales = "free")+
  stat_ellipse()


# Scatterplot 14C vs. C vol CO2
ggplot(data=DC_database,
       aes(y=CO2_14C_Modern, x=d18O_chem, fill=Treatment))+
  geom_point( shape=21, size=3)+
  #scale_shape_manual(values=c(1, 21,24))+  #scale_y_continuous(limits=c(90,116))+
  facet_wrap(~Site_id,  scales = "fixed")+
  stat_ellipse()+
  labs( x= "d18O", y="14C-CO2 (% modern)", 
        title="Water source and CO2 age relationship")




# Keeling CO2
ggplot(data= DC_database, 
       aes(y=d13C_CO2, x=1/CO2_mgL_C14, fill=Site_id ))+
  geom_point(size=3, shape=21)+
  #scale_shape_manual(values=c(21))+  #scale_y_continuous(limits=c(90,116))+
  #geom_smooth(method=lm, aes(color=Site_id),se=F)#+  
  stat_ellipse( aes(color=Site_id))#+
  #facet_wrap(~Site_id,  scales = "fixed")

summary(lm(d13C~(1/CO2_vol_ml_or_DOCmgL), data=filter(C14, Material_Type == "CO2", Site_id=="C1")))
lm(d13C~(1/CO2_vol_ml_or_DOCmgL), data=filter(C14, Material_Type == "CO2", Site_id=="C2"))
lm(d13C~(1/CO2_vol_ml_or_DOCmgL), data=filter(C14, Material_Type == "CO2", Site_id=="DC1"))
lm(d13C~(1/CO2_vol_ml_or_DOCmgL), data=filter(C14, Material_Type == "CO2", Site_id=="DC2"))
lm(d13C~(1/CO2_vol_ml_or_DOCmgL), data=filter(C14, Material_Type == "CO2", Site_id=="DC3"))
lm(d13C~(1/CO2_vol_ml_or_DOCmgL), data=filter(C14, Material_Type == "CO2", Site_id=="DC4"))

#Scatter 14C-CO2 vs 14C-DOC
ggplot(data=C14_wide,
       aes(x=perc_Modern_CO2, y=perc_Modern_DOC, color=Site_id))+
         geom_point(size=3)+
  stat_ellipse()+
  geom_hline(yintercept = 100, linetype="dotted")+
  geom_vline(xintercept = 100,linetype="dotted")+
  geom_abline(intercept=0, slope =1)+
  geom_abline(intercept=10, slope =1, linetype="dashed")+
  
  #scale_x_continuous(limits=c(95,112))+
  labs(x="14C-CO2 (%)", y="14C-DOC (%)", title="Radiocarbon offset between CO2 and DOC")

#Scatter 14C-DOC v.s. Concentration
ggplot(data=C14_wide_chemistry, #filter(C14_wide_chemistry, Site_id=="C2"),
       aes(x=DOC_mgL, y=perc_Modern_DOC-perc_Modern_CO2, fill=Site_id))+
  geom_point(size=3, shape=21)+
  stat_ellipse(aes(color=Site_id))+
  #geom_smooth(method="lm", se=F, aes(color=Site_id))#+
facet_wrap(~Site_id, scales="free" )


#Scatter 14C-DOC v.s. C:N
ggplot(data=C14_wide_chemistry,
       aes(x=DOC_mgL/DN_mgL, y=perc_Modern_DOC-perc_Modern_CO2, fill=Site_id))+
  geom_point(size=3, shape=21)+
  #stat_ellipse()+
  geom_smooth(method="lm", se=F, aes(color=Site_id))+
  facet_wrap(~Site_id, scales="free" )



#Scatter 14C-DOC v.s. d18O
ggplot(data=C14_wide_chemistry,
       aes(x=d18O, y=perc_Modern_DOC, fill=Site_id))+
  geom_point(size=3, shape=21)+
  #stat_ellipse()+
  geom_smooth(method="lm", se=F, color="grey15")+
  facet_wrap(~Site_id, scales="free" )+
  labs(title= "Relationship between d18O-water and 14C-DOC")



#Scatter 14C-CO2 v.s. d18O
ggplot(data=C14_wide_chemistry,
       aes(x=d18O, y=d13C_CO2, fill=Site_id))+
  geom_point(size=3, shape=21)+
  #stat_ellipse()+
 # geom_smooth(method="lm", se=F, color="grey15")+
  facet_wrap(~Site_id, scales="fixed" )+
  #scale_y_continuous(limits=c(95,120))+
  labs(title= "Relationship between d18O-water and 14C-CO2")
#_________________________________________________
# Boxplots
#_________________________________________________


ggplot(data= filter(C14, Material_Type == "CO2"),
       aes(x=Site_id, y=d13C))+
  geom_violin()+
geom_dotplot(binaxis='y', stackdir='center', dotsize=1)



ggplot(data= filter(C14, Material_Type == "CO2"),
       aes(x=Site_id, y=perc_Modern))+
  geom_violin()+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=1)

