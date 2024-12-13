

#Parallel Coordinate Plot


# Libraries
library(GGally)
library(dplyr)




# Data set is provided by R natively
data <- filter(DC_database, Site_id=="DC1") %>% # Keep only DC1 site
  filter(Treatment !="During Clearcut")%>% # Remove "During Clearcut" data
  select(Site_id, Treatment, # Select variables to plot in Radar chart
         DOC_mgL_chem, pH_chem, #CO2_mgL_chem, #CH4_ugL_chem, 
         dexcess,Si_ugL, Ca_ugL, Mg_ugL,DN_mgL_chem,PO4_ugL_chem)
data
#data_long=data%>%
#  pivot_longer(cols=!c(Treatment,Site_id), names_to="variable", values_to = "data")


# Identify the pristine row
pristine_row <- radar[3,] # Select the mean DCI pristine row (from Radar calc. below)
pristine_row
head(data)

# Divide all rows by the pristine row
data_divMean <- data %>%
  mutate(across(c(DOC,pH,dexcess,Si,Ca,Mg,DN,PO4), 
                ~ . / pristine_row[[cur_column()]]))

head(result)


# Parallel Coordinate Plot

ggparcoord( data, showPoints=F, scale="center",
            columns = 3:10, groupColumn = 2, alpha=1) +
  #scale_y_log10()+
  scale_color_manual(values=c( "dodgerblue", "seagreen", "gray70") ) +
  theme_bw()





# Lollipop plot___________________________________________________________________________________
# Prepare Data 
radar <- DC_database %>% # Keep only DC1 site
  filter(Treatment !="During Clearcut")%>% # Remove "During Clearcut" data
  select(Site_id, Treatment, # Select variables to plot in Radar chart
         DOC_mgL_chem, pH_chem, #CO2_mgL_chem, #CH4_ugL_chem, 
         dexcess,Si_ugL, Ca_ugL, Mg_ugL,DN_mgL_chem,PO4_ugL_chem)%>%
  # drop_na() %>%
  group_by(Site_id,Treatment, #Group by Treatment type
           .drop=F) %>%
  summarise(  #Calculate the mean on 8 selected variables for Radar Chart
    #Site_id = first(Site_id),
    DOC = mean(DOC_mgL_chem,na.rm = TRUE),
    pH = mean(pH_chem,na.rm = TRUE),  
    dexcess = mean(dexcess,na.rm = TRUE),
    
    Si = mean(Si_ugL,na.rm = TRUE),
    Ca = mean(Ca_ugL,na.rm = TRUE),
    Mg = mean(Mg_ugL,na.rm = TRUE),
    
    DN = mean(DN_mgL_chem,na.rm = TRUE),
    PO4 = mean(PO4_ugL_chem,na.rm = TRUE)
  ) %>% 
  ungroup() #%>%
  #mutate_at(vars(-c(Treatment,Site_id)),  #Rescale the data linerarly from 0 to 1
  #          rescale)
radar
# Identify the pristine row
#pristine_row <- radar_DC1 %>% filter(Treatment == "Pristine")

# Divide all rows by the pristine row
result <- radar_DC1 %>%
  mutate(across(c(DOC, CO2, CH4, dexcess, Si, pH, DN, PO4), 
                ~ . / pristine_row[[cur_column()]]))

result%>%
  mutate_at(vars(-Treatment),  #Rescale the data linerarly from 0 to 1
            rescale)

result%>%
  pivot_longer(cols=!c(Treatment,Site_id), names_to="variable", values_to = "data")ggplot(result%>%
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


