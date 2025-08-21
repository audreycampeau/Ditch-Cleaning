


# Dexcess `CO2`
ggplot( DC_Q, #filter(DC_Q, Site_id %in% c("DC1", "DC3")), 
        aes(y=DOC_14C_Modern-CO2_14C_Modern, 
            x=dexcess_filled,
            color=Site_id))+
  geom_point( aes(fill=Treatment))+
  
  scale_color_manual(values=site_colors_6)+ #"
  scale_fill_manual(values=site_colors_6)+ #"
  
  stat_cor(
    method = "spearman",
    label.y.npc = "top", label.x.npc = "left", cor.coef.name = "rho",
    aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"), color=Site_id),
    r.accuracy = 0.01, p.accuracy = 0.01, show.legend = F
  ) +
  stat_chull()+
  #geom_smooth(method="lm", se=F, #aes(color=Treatment), 
  #            show.legend = F)+
  
  #stat_regline_equation(label.y.npc = "top", label.x.npc = "left", #0.30
  #                      aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")), #, color = Treatment
  #                      show.legend = F, size=4)+
  
  #labs(x="specific discharge (m/d)", y=bquote("CO2 (mg C L"^-1*")"), shape="Watershed ID")+
  
  theme(legend.position = "right")



# Dexcess `CO2` # Effect present only for ditch clearned sites. 
ggplot( DC_Q, #filter(DC_Q, Site_id %in% c("DC1", "DC3")), 
        aes(y=CO2_14C_Modern - DOC_14C_Modern, x=d18O_filled, color=Site_id
        ))+
  geom_point( aes(fill=Treatment, ))+
  
  scale_shape_manual(values=site_symbols)+
  scale_fill_manual(values=site_colors_6)+ #"
  scale_color_manual(values=site_colors_6)+ #"
  
  stat_chull()+
  # stat_ellipse(aes(color=Treatment), size = 1.5, show.legend = F) +
  
  stat_cor(
    method = "spearman",
    label.y.npc = "top", label.x.npc = "left", cor.coef.name = "rho",
    aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"), color=Site_id),
    r.accuracy = 0.01, p.accuracy = 0.01, show.legend = F
  ) +
  #labs(x="specific discharge (m/d)", y=bquote("CO2 (mg C L"^-1*")"), shape="Watershed ID")+
  
  #facet_wrap(~Site_id)+
  
  #geom_smooth(method="lm", se=F, #aes(color=Treatment), 
  #            show.legend = F)+
  #stat_regline_equation(label.y.npc = "top", label.x.npc = "left", #0.30
  #                      aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")), #, color = Treatment
  #                      show.legend = F, size=4)+
  
  
  theme(legend.position = "right")


