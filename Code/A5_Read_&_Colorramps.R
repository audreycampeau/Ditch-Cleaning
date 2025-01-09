

DC_Q=readRDS( "Output/Data/DC_Q.rds")


# Create color ramp and symbols
treatments_colors <- c(
  "Pristine" = "deepskyblue",     # light blue
  "Clearcut" = "#fc9272ff",     # light red
  "Ditch cleaning" = "#de2d26ff" # dark red
)


treatments_colors2 <- c(
  "Pristine" = "deepskyblue", #"#88CCEE",     # light blue
  "Clearcut" = "#CC6677",     # light red
  "Ditch cleaning" = "#882255" # dark red
)


site_colors <- c(
  "DC1"= "#2E5894", #Dark blue
    "DC2"= "#E69B00", # Light orange
    "DC3"= "#7EA6E0", #Light blue
    "DC4"= "#994F00") #Dark brown
    

treatments_symbols <- c(
  "Pristine" = 21,     
  "Clearcut" = 23,     
  "Ditch cleaning" = 24 
)


sites_symbols <- c(
  "DC1" = 21,     
  "DC2" = 3,     
  "DC3" = 22,
  "DC4" =4
)

