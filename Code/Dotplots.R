
#Differences in 14C-gap between CO2 and DOC

#Does the 14C gap between CO2 and DOC remains constant between both treatments and sites?

library(ggpubr)

# Convert data to long format for plotting
C14_DC_long <- filter(DC_Q, Site_id %in% c("DC1","DC2", "DC3", "DC4" )) %>%
  pivot_longer(
    cols = c(CO2_14C_Modern, DOC_14C_Modern),
    names_to = "Carbon_specie",
    values_to = "C14_value"
  )


C14_DC_long$Carbon_specie=recode(C14_DC_long$Carbon_specie, CO2_14C_Modern = "CO2", DOC_14C_Modern="DOC") #Rename the CO2 and DOC fields
C14_DC_long$Treatment <- factor(C14_DC_long$Treatment, #reorder the treatment types
                                levels = c("Pristine", "Clearcut", "Ditch cleaning"))


# Create the plot
ggplot(filter(C14_DC_long, !is.na(Treatment)) , aes(x = Carbon_specie, y = C14_value, 
                                                    group = interaction(row_id, Treatment),
                                                    
                                                    fill = Treatment)) +
  geom_line(alpha = 0.6) +
  geom_point(#aes(shape=Site_id), 
    size=3, shape=21) +
  facet_grid(rows=vars(Site_id), cols=vars(Treatment), scales = "fixed") +
  scale_fill_manual(values = c(treatments_colors)) +
  #scale_shape_manual(values = c(21))+
  labs(x = "Carbon Species",
       y = "14C Content",
       color = "Treatment") +
  theme_cleveland() +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )
#ggtitle("The CO2 is almost always less enriched in 14C than DOC")










#Dot plots 14C ~ treatment ~ sites
DC_Q$Treatment <- factor(DC_Q$Treatment, #reorder the treatment types
                                levels = c("Pristine", "Clearcut", "Ditch cleaning"))

#violin_DOC= 
ggplot(filter(DC_Q, Treatment %in% c("Ditch cleaning","Clearcut", "Pristine" )) %>%
                   pivot_longer(
                     cols = c(CO2_14C_Modern, DOC_14C_Modern),
                     names_to = "Carbon_specie",
                     values_to = "C14_value"
                   ),
       
                   
                   aes(fill=Treatment, y=C14_value, x=Treatment))+
  geom_violin(alpha=0.5)+
  geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 0.3)+
  scale_fill_manual(values=c(treatments_colors))+
  facet_wrap(~Carbon_specie, scales = "fixed")
#facet_grid(rows=vars(Site_id), cols=vars(Treatment), scales = "fixed") 
  

violin_CO2=ggplot(filter(DC_Q, Treatment %in% c("Ditch cleaning","Clearcut", "Pristine" )),
       aes(fill=Treatment, y=CO2_14C_Modern, x=Treatment))+
  geom_violin(alpha=0.5)+
  geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 0.3)+
  scale_fill_manual(values=c(treatments_colors))


library(ggpubr)
ggarrange()


#__________________________________________________________________
# Connected dot plot
#__________________________________________________________________

# Convert data to long format for plotting
C14_DC_long <- filter(DC_Q, Site_id %in% c("DC1","DC2", "DC3", "DC4" )) %>%
  pivot_longer(
    cols = c(CO2_14C_Modern, DOC_14C_Modern),
    names_to = "Carbon_specie",
    values_to = "C14_value"
  )


C14_DC_long$Carbon_specie=recode(C14_DC_long$Carbon_specie, CO2_14C_Modern = "CO2", DOC_14C_Modern="DOC") #Rename the CO2 and DOC fields
C14_DC_long$Treatment <- factor(C14_DC_long$Treatment, #reorder the treatment types
                                levels = c("Pristine", "Clearcut", "Ditch cleaning"))


# Create the plot
ggplot(filter(C14_DC_long, !is.na(Treatment)) , aes(x = Carbon_specie, y = C14_value, 
                      group = interaction(row_id, Treatment),
                      
                      color = Treatment)) +
  geom_line(alpha = 0.6) +
  geom_point(aes(size=CO2_mgL)) +
  facet_grid(rows=vars(Site_id), cols=vars(Treatment), scales = "fixed") +
  scale_color_manual(values = c(treatments_colors)) +
  labs(x = "Carbon Specie",
       y = "14C Content",
       color = "Treatment") +
  theme_cleveland() +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )
     