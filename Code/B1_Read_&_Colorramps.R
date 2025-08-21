#DC_Q_origin=readRDS( "Output/Data/DC_Q.rds")
#write.csv(DC_Q_origin, "Output/Data/DC_Q_origin.csv")
#write.csv(DC_Q, "Output/Data/DC_Q_Meteo_chem_14C_20250811.csv")




#DC_Q=readRDS( "Output/Data/DC_Q_Meteo_chem_14C.rds")
DC_Q=readRDS( "Output/Data/DC_C_Q_Meteo_chem_14C.rds")

DC_Q$Date=as.Date(DC_Q$Date)

# Remove one oulier 14C-CO2 140%modern at C2 once. 
DC_Q$CO2_14C_Modern= ifelse(DC_Q$CO2_14C_Modern >130, NA, DC_Q$CO2_14C_Modern)

# Order factors site pairs 
DC_Q$Site_id <- factor(DC_Q$Site_id, 
                       levels = c("C2", "C1", "DC2", "DC4", "DC1", "DC3"))


DC_Q$CO2_mgL_filled_keeling= 1/DC_Q$CO2_mgL_filled
DC_Q$CO2_mgL_filled_keeling = ifelse(is.infinite (DC_Q$CO2_mgL_filled_keeling), NA, DC_Q$CO2_mgL_filled_keeling)

# Create color ramp and syminfert# Create color ramp and symbols
treatment_colors <- c(
  "Pristine" = "deepskyblue",     # light blue
  "Clearcut" = "#fc9272ff",     # light red
  "Ditch cleaning" = "#de2d26ff" # dark red
)


site_colors <- c(
  "DC1"= "#2E5894", #Dark blue
    "DC2"= "#E69B00", # Light orange
    "DC3"= "#7EA6E0", #Light blue
    "DC4"= "#994F00") #Dark brown
    

site_colors_6 <- c(
  # Pristine sites (C2 and C1)
  "C2" = "#228B22",  # Forest green
  "C1" = "#90EE90",  # Light green
  
  # Clearcut pair - warm earth tones (your current DC1/DC2)
  "DC2" = "#E69B00", # Light orange
  "DC4" = "#994F00", #Dark brown
  
  # Clearcut+DitchCleaning pair - disturbed/muddy tones
  "DC1" = "#2E5894", #Dark blue
  "DC3" = "#7EA6E0" #Light blue
)



treatment_symbols <- c(
  "Pristine" = 21,     
  "Clearcut" = 23,     
  "Ditch cleaning" = 24 
)


site_symbols <- c(
  "DC1" = 21,     
  "DC2" = 3,     
  "DC3" = 22,
  "DC4" =4
)



#treatment_colors2 <- c(
# "Pristine" = "deepskyblue", #"#88CCEE",     # light blue
# "Clearcut" = "#CC6677",     # light red
# "Ditch cleaning" = "#882255" # dark red
#)

