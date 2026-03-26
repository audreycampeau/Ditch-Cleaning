library(readxl)
library(tidyverse)

# Site naming system:
# DC1 = C58 ---> FC_DC1
# DC2 = C59 ---> FC1
# DC3 = C60 ---> FC_DC2
# DC4 -C57 ---> FC2

# Catchment areas 
#DC2_Area_m2=4.4*10000
#DC1_Area_m2=8.4*10000 
#DC3_Area_m2=8.4*10000
#DC4_Area_m2=10.7*10000


# Open Discharge data _____________________________________________________________________________
Q_Mosquera=read_xlsx("Input/Virginia Q data/DB.Q.Interpolated.Audrey.xlsx")
colnames(Q_Mosquera)[3]="q_int_mmd"


ggplot(data=Q_Mosquera, 
       aes (x=Date, y=q_int_mmd, colour = as.factor(Site)))+
  geom_point()


Q_DC2= Q_Mosquera %>% filter (Site == "59")
Q_DC2= Q_DC2 %>% select (c("Date","Q","q_int_mmd"))

Q_DC3= Q_Mosquera %>% filter (Site == "60")
Q_DC3= Q_DC3 %>% select (c("Date","Q","q_int_mmd"))


#Open Meteo Daily _________________________________________________________________________________
Meteo=read_xlsx("Input/Q and Meteo/Meteo_Daily.xlsx")
Meteo$Date=as.Date(Meteo$TimeStamp)

Meteo= Meteo %>% # Remove dates in 2019, and 2023
        filter(Date >= as.Date("2020-01-01") & Date <= as.Date("2022-12-31"))
        


# Combine meteo data to Q_DC
DC2_Q_Meteo=left_join(Meteo, Q_DC2, by = 'Date', suffix = c( "_Meteo", ""))
DC4_Q_Meteo=DC2_Q_Meteo 

DC3_Q_Meteo=left_join(Meteo, Q_DC3, by = 'Date', suffix = c( "_Meteo", ""))
DC1_Q_Meteo=DC3_Q_Meteo 


# Add a column to identify the site
DC2_Q_Meteo$Site_id=rep("DC2", nrow(DC4_Q_Meteo))
DC4_Q_Meteo$Site_id=rep("DC4", nrow(DC4_Q_Meteo))
DC3_Q_Meteo$Site_id=rep("DC3", nrow(DC3_Q_Meteo))
DC1_Q_Meteo$Site_id=rep("DC1", nrow(DC1_Q_Meteo))



# Timeseries of Q in all sites _______________________________________________________________________
ggplot(data=rbind(DC4_Q_Meteo, DC3_Q_Meteo), aes(x=as.Date(Date), y=q_int_mmd, color=Site_id))+
  geom_point()+
  scale_x_date(limits= c(as.Date("2020-01-01"), as.Date("2022-12-31")))+
  labs(x="Date", y="q (mm/d)")


