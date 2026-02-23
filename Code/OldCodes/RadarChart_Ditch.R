

# Graphical Abstract
#sizes= as.data.frame(c(mean(qc_subset$DHg_Mean, na.rm=T),
#         mean(qc_subset$DOC_Mean, na.rm=T),
#         mean(qc_subset$DMeHg_Mean, na.rm=T),
 #        mean(qc_subset$Cgas_mgCL, na.rm=T)))

#sizes$x=c(1,2,3,4)
#colnames(sizes)[1]="size"
#ggplot(sizes, aes( x=x, y=x))+
#  geom_point( aes(size=size) ,shape=21)+
#  scale_size(range = c(10, 40))

# USING GGRADAR ::::::::::::::::::::::::::::::::::::::::::::::::::::
devtools::install_github("ricardo-bion/ggradar")
library("ggradar")

 library(ggradar)
 #library(palmerpenguins)
 library(tidyverse)
 library(scales)
 library(showtext)

DC_database
DC_Q
colnames(DC_database)

filter(DC_database, Site_id =="DC1") %>%
filter(Treatment!="During Clearcut")

# Prepare Data 
radar <- DC_database %>% # Keep only DC1 site
  filter(Treatment !="During Clearcut")%>% # Remove "During Clearcut" data
  select(Site_id, Treatment, # Select variables to plot in Radar chart
         DOC_mgL_chem, CO2_mgL_chem, CH4_ugL_chem, 
         dexcess,Si_ugL,pH_chem,DN_mgL_chem,PO4_ugL_chem)%>%
 # drop_na() %>%
  group_by(Site_id,Treatment, #Group by Treatment type
           .drop=F) %>%
  summarise(  #Calculate the mean on 8 selected variables for Radar Chart
    #Site_id = first(Site_id),
    DOC = mean(DOC_mgL_chem,na.rm = TRUE),
    CO2 = mean(CO2_mgL_chem,na.rm = TRUE),
    CH4 = mean(CH4_ugL_chem,na.rm = TRUE),
    dexcess = mean(dexcess,na.rm = TRUE),
    Si = mean(Si_ugL,na.rm = TRUE),
    pH = mean(pH_chem,na.rm = TRUE),  # SHOULD I LOG OR NOT??
    DN = mean(DN_mgL_chem,na.rm = TRUE),
    PO4 = mean(PO4_ugL_chem,na.rm = TRUE)
  ) %>% 
  ungroup() %>%
  mutate_at(vars(-c(Treatment,Site_id)),  #Rescale the data linerarly from 0 to 1
            rescale)
radar
# Identify the pristine row
pristine_row <- radar_DC1 %>% filter(Treatment == "Pristine")

# Divide all rows by the pristine row
result <- radar_DC1 %>%
  mutate(across(c(DOC, CO2, CH4, dexcess, Si, pH, DN, PO4), 
                ~ . / pristine_row[[cur_column()]]))

result%>%
  mutate_at(vars(-Treatment),  #Rescale the data linerarly from 0 to 1
            rescale)

?pivot_longer()


result%>%
pivot_longer(cols=!c(Treatment,Site_id), names_to="variable", values_to = "data")

# Horizontal version
ggplot(result%>%
         pivot_longer(cols=!c(Treatment,Site_id), names_to="variable", values_to = "data"), 
       
       aes(x=variable, y=data, color=Treatment)) +
  geom_segment( aes(x=variable, xend=variable, y=0, yend=data), color="skyblue") +
  geom_point(  size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )





#radar_region= unique(merge(radar, Treatment %>%
 #              select(Site_id),      by.y="ID"))


# Color for the lines
lcols <- c("#EEA236", "#5CB85C", "#46B8DA")
#radar=radar_region[which(radar_region$Region=="NS"),]
HM_radar=radar_region[which(radar_region$Region=="HM"),]
LR_radar=radar_region[which(radar_region$Region=="LR"),]

#radar_a=
  ggradar(radar,
        values.radar = c("0", "0.5", "1"),
        background.circle.colour = "white",
        axis.line.colour = "gray60",
        gridline.min.colour = "gray60",
        gridline.mid.colour = "gray60",
        gridline.max.colour = "gray60",
        group.colours = rep(c("#EEA236", "#5CB85C", "#46B8DA")),
        group.line.width = 0.6,
        group.point.size = 0, fill=T, fill.alpha = 0.5)+
  theme(legend.position = "none")+

?ggradar
radar_b=ggradar(HM_radar[,-10], #filter(radar, Region=="NS"),
        values.radar = c("Min", NA, "Max"),
        background.circle.colour = "white",
        axis.line.colour = "gray60",
        gridline.min.colour = "gray60",
        gridline.mid.colour = "gray60",
        gridline.max.colour = "gray60",
        group.colours = rep(colorramp[1],length(NS_radar)),
        group.line.width = 0.3,
        group.point.size = 0, fill=F, fill.alpha = 0.1)+
  theme(legend.position = "none")

radar_c=ggradar(LR_radar[,-10], #filter(radar, Region=="NS"),
        values.radar = c("Min", NA, "Max"),
        background.circle.colour = "white",
        axis.line.colour = "gray60",
        gridline.min.colour = "gray60",
        gridline.mid.colour = "gray60",
        gridline.max.colour = "gray60",
        group.colours = rep(colorramp[2],length(NS_radar)),
        group.line.width = 0.3,
        group.point.size = 0, fill=F, fill.alpha = 0.1)+
  theme(legend.position = "none")+
  labs( title="C)")


quartz(width=12,height=5,pointsize=12)

radars=ggarrange(radar_b,radar_c, radar_a,   nrow=1 #labels=c("A)", "B)", "C)")
                 )
quartz.save("/Users/audreycampeau/Documents/DATA/JF/R Codes/JF/RadarCharts.png", type="png", dpi=600)




#__________________________________________________________________________________________________
#__________________________________________________________________________________________________
#__________________________________________________________________________________________________
# Second method for plotting Radar Charts
#__________________________________________________________________________________________________
#__________________________________________________________________________________________________
#__________________________________________________________________________________________________



# Radar Chart
install.packages("fmsb")
library(fmsb)

NS_radar=select(NS, DOC_Mean, C1, DHg_Mean, C4,DMeHg_Mean,CO2_mgCL,CH4_mgCL,MOX)

# Define the variable ranges: maximum and minimum
max_min <- data.frame(
  DOC = range(NS$DOC_Mean, na.rm=T), 
  C1  = range(NS$C1, na.rm=T),
  DHg = range(NS$DHg_Mean, na.rm=T), 
  C4  = range(NS$C4, na.rm=T),
  DMeHg = range(NS$DMeHg_Mean, na.rm=T), 
  CO2 = range(NS$CO2_mgCL, na.rm=T), 
  CH4 = range(NS$CH4_mgCL, na.rm=T), 
  MOX = range(NS$MOX, na.rm=T)
)
rownames(max_min) <- c("Max", "Min")


# Bind the variable ranges to the data
colnames(NS_radar) <- colnames(max_min) 
rbind(max_min, NS_radar)

# Plot the data for student 1
student1_data <- df[c("Max", "Min", "Student.1"), ]
radarchart(NS_radar)


create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
  
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}


# Reduce plot margin using par()
op <- par(mar = c(1, 2, 2, 2))


# Create the radar charts
create_beautiful_radarchart(
  data = NS_radar, caxislabels = c(0, 5, 10, 15, 20),
  color = c("#00AFBB", "#E7B800", "#FC4E07")
)
# Add an horizontal legend
legend(
  x = "bottom", legend = rownames(df[-c(1,2),]), horiz = TRUE,
  bty = "n", pch = 20 , col = c("#00AFBB", "#E7B800", "#FC4E07"),
  text.col = "black", cex = 1, pt.cex = 1.5
)
par(op)


