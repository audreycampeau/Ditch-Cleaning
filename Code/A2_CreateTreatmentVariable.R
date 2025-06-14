

# Here is the information provided by Alberto on April 15th 2024 
  #concerning the dates of clearcut and ditch cleaning for different sites

        #Here are the periods of time when the operations occurred:
        #DC sites – clear cuts – depends on the catchment, Start 20 July 2020 - End 24 August 2020 (but wood was stacked on the road, so some trucks were visiting the site for at least one more month;
        #(+ Site preparation - July 6, 2021 (maybe one day before/after))
        #DC sites – ditch cleaning - 9:00am on the 20th of September, 2021, typically worked from 8:00-17:00 until the 22nd or 23rd.  
                                                                                                     
        #Given this, we started counting the treatments as when they were completed. Here are the reference dates:
        #DC – clearcut – 25 August 2020
        #DC ditch cleaning – 23 September 2021 (but the “during” effects of this could be big..)
                                                                                             

#Make a treatment categorical variable
library(dplyr)


#Create a new variable called Treatment
range(DC1_Q_Meteo$Date, na.rm=T)
#range of dates "2018-12-18" "2023-10-25"

DC1_Q_Meteo=DC1_Q_Meteo %>%
  mutate(Treatment = case_when(
    between(Date, as.Date("2018-12-18"), as.Date("2020-08-24")) ~ "Pristine",
    between(Date, as.Date("2020-08-25"), as.Date("2021-09-22")) ~ "Clearcut",
    between(Date, as.Date("2021-09-23"), as.Date("2023-10-25")) ~ "Ditch cleaning"
  ))

DC3_Q_Meteo=DC3_Q_Meteo %>%
  mutate(Treatment = case_when(
    between(Date, as.Date("2018-12-18"), as.Date("2020-08-24")) ~ "Pristine",
    between(Date, as.Date("2020-08-25"), as.Date("2021-09-22")) ~ "Clearcut",
    between(Date, as.Date("2021-09-23"), as.Date("2023-10-25")) ~ "Ditch cleaning"
  ))


DC2_Q_Meteo=DC2_Q_Meteo %>%
  mutate(Treatment = case_when(
    between(Date, as.Date("2018-12-18"), as.Date("2020-08-24")) ~ "Pristine",
    between(Date, as.Date("2020-08-25"), as.Date("2023-10-25")) ~ "Clearcut"
    # No Ditch cleaning
  ))

DC4_Q_Meteo =DC4_Q_Meteo %>%
  mutate(Treatment = case_when(
    between(Date, as.Date("2018-12-18"), as.Date("2020-08-24")) ~ "Pristine",
    between(Date, as.Date("2020-08-25"), as.Date("2023-10-25")) ~ "Clearcut"
    # No Ditch cleaning
  ))

# Combine the four DC_Q_Meteo data frames
DC_Q_Meteo=rbind(DC1_Q_Meteo, DC2_Q_Meteo, DC3_Q_Meteo, DC4_Q_Meteo)

# Format to factor Site_id and Treatment
DC_Q_Meteo$Site_id=as.factor(DC_Q_Meteo$Site_id)
DC_Q_Meteo$Treatment=as.factor(DC_Q_Meteo$Treatment)

#Order factors
DC_Q_Meteo$Treatment <- factor(DC_Q_Meteo$Treatment, #reorder the treatment types so they are in the right sequence
                         levels = c("Pristine", "Clearcut", "Ditch cleaning"))



#Save DC_Q_Meteo dataset to output folder
saveRDS(DC_Q_Meteo, "Output/Data/DC_Q_Meteo.rds")



