

# Carbon Isotope plots



# 14C-CO2 vs q_md
ggplot(data=filter(DC1_DC3, Treatment !="During Clearcut"), 
       aes(y=CO2_14C_Modern, x=q_md, fill=Treatment, color=Treatment))+
  geom_point(size=3, color="black", aes(shape=Site_id))+
  scale_shape_manual(values=c(21,24))+
  scale_fill_manual(values=c( "#fc9272ff", "#de2d26ff","#fee0d2ff"))+ 
  #scale_color_manual(values=rep( "black", 3))+  
  #geom_smooth(method="lm", color="black", se=F, show.legend = F)+
  stat_ellipse(show.legend=F)+ #
  scale_color_manual(values=c( "#fc9272ff", "#de2d26ff","#fee0d2ff"))+ 
  #labs(x="q (m/d)", y="DN (mgC/L)")+
  theme(legend.position="top")+
  scale_y_continuous(limits=c(90,115))+
  geom_abline(intercept=0, slope =1, linetype="dashed")#+
#facet_wrap(~Site_id, scale="fixed")

ggplot(data=filter(DC1_DC3, Treatment !="During Clearcut"), 
       aes(y=DOC_14C_Modern, x=q_md, fill=Treatment, color=Treatment))+
  geom_point(size=3, color="black", aes(shape=Site_id))+
  scale_shape_manual(values=c(21,24))+
  scale_fill_manual(values=c( "#fc9272ff", "#de2d26ff","#fee0d2ff"))+ 
  #scale_color_manual(values=rep( "black", 3))+  
  #geom_smooth(method="lm", color="black", se=F, show.legend = F)+
  stat_ellipse(show.legend=F)+ #
  scale_color_manual(values=c( "#fc9272ff", "#de2d26ff","#fee0d2ff"))+ 
  #labs(x="q (m/d)", y="DN (mgC/L)")+
  theme(legend.position="top")+
  scale_y_continuous(limits=c(90,115))+
  geom_abline(intercept=0, slope =1, linetype="dashed")#+
#facet_wrap(~Site_id, scale="fixed")


# 14C-CO2 vs 14CDOC
ggplot(data=filter(DC1_DC3, Treatment !="During Clearcut"), 
       aes(y=CO2_14C_Modern, x=DOC_14C_Modern, fill=Treatment, color=Treatment))+
  geom_point(size=3, color="black", aes(shape=Site_id))+
  scale_shape_manual(values=c(21,24))+
  scale_fill_manual(values=c( "#fc9272ff", "#de2d26ff","#fee0d2ff"))+ 
  #scale_color_manual(values=rep( "black", 3))+  
  #geom_smooth(method="lm", color="black", se=F, show.legend = F)+
  stat_ellipse(show.legend=F)+ #
  scale_color_manual(values=c( "#fc9272ff", "#de2d26ff","#fee0d2ff"))+ 
  #labs(x="q (m/d)", y="DN (mgC/L)")+
  theme(legend.position="top")+
  geom_abline(intercept=0, slope =1, linetype="dashed")#+
#facet_wrap(~Site_id, scale="fixed")

# d13C-CO2 vs d13CDOC
ggplot(data=filter(DC1_DC3, Treatment !="During Clearcut"), 
       aes(y=d13C_CO2, x=d13C_DOC, fill=Treatment, color=Treatment))+
  geom_point(size=3, color="black", aes(shape=Site_id))+
  scale_shape_manual(values=c(21,24))+
  scale_fill_manual(values=c( "#fc9272ff", "#de2d26ff","#fee0d2ff"))+ 
  #scale_color_manual(values=rep( "black", 3))+  
  #geom_smooth(method="lm", color="black", se=F, show.legend = F)+
  stat_ellipse(show.legend=F)+ #
  scale_color_manual(values=c( "#fc9272ff", "#de2d26ff","#fee0d2ff"))+ 
  #labs(x="q (m/d)", y="DN (mgC/L)")+
  theme(legend.position="top")+
  geom_abline(intercept=5, slope =1, linetype="dashed")#+
#facet_wrap(~Site_id, scale="fixed")
