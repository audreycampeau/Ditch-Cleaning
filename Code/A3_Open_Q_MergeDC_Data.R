# Open Water balance, Discharge data

#Data Source: Johannes Tiwari (Sent by Alberto in April 2024)
#Q units are most likely L s-1
# Q units can't be L/s, they are too high!! 
Q_DC4=read.csv("Input/Q and Meteo/Q_59_daily.Time_Series_Data.2024031816491438.csv", skip=11)
Q_DC2=read.csv("Input/Q and Meteo/Q_C57_Daily.Time_Series_Data.2024031816461304.csv", skip=11)
Q_DC3=read.csv("Input/Q and Meteo/Q_60_daily.Time_Series_Data.2024031816502188.csv", skip=11)
Q_C1=read.csv("Input/Q and Meteo/Q_C01_Daily_PRELIMINARY.Time_Series_Data.2024031816562806.csv", skip=11)


#Data Source: SITES data Portal (retreived in April 2024)
#Q units are m3 s1 (from Sites Data Portal)
Q_C2=read.csv("Input/Q and Meteo/SITES_WB-Q_SVB-VAB-C2_20050406-20231116_daily.csv", skip=24)
Q_C4=read.csv("Input/Q and Meteo/SITES_WB-Q_SVB_KKM-C4_20070529-20231113_L2_daily.csv", skip=23)
Q_C18=read.csv("Input/Q and Meteo/SITES_WB-Q_SVB_DEG-C18_20120425-20231123_L2_daily.csv", skip=22)



#Format Dates
Q_DC4$Date=as.Date(Q_DC4$TimeStamp)
Q_DC2$Date=as.Date(Q_DC2$TimeStamp)
Q_DC3$Date=as.Date(Q_DC3$TimeStamp)
Q_C1$Date=as.Date(Q_C1$TimeStamp)
Q_C2$Date=as.Date(Q_C2$TIMESTAMP)
Q_C4$Date=as.Date(Q_C4$TIMESTAMP)
Q_C18$Date=as.Date(Q_C18$TIMESTAMP)


#Convert all Q measurements to m3/d
## Remove all conversion and assume that Q data is already in m3/d (DC3 range from 0.0001 to 47, which makes sense)
Q_DC4$Q_m3d=Q_DC4$Q #/1000*60*60*24
Q_DC2$Q_m3d=Q_DC2$Q #/1000*60*60*24
Q_DC3$Q_m3d=Q_DC3$Q #/1000*60*60*24
Q_C1$Q_m3d=Q_C1$Q# /1000*60*60*24

Q_C2$Q_m3d=Q_C2$Q*60*60*24
Q_C4$Q_m3d=Q_C4$Q*60*60*24
Q_C18$Q_m3d=Q_C18$Q*60*60*24

#Catchment Area in m2 (originally in ha)
C2_Area_m2=12*10000
C1_Area_m2=48*10000
C4_Area_m2=18*10000
C18_Area_m2=650*10000

DC1_Area_m2=6.7*10000
DC2_Area_m2=4.4*10000
DC3_Area_m2=8.4*10000
DC4_Area_m2=10.7*10000

#Convert all Q measurements to Specific Discharge m/d
Q_DC4$q_md=Q_DC4$Q_m3d/DC4_Area_m2
Q_DC2$q_md=Q_DC2$Q_m3d/DC2_Area_m2
Q_DC3$q_md=Q_DC3$Q_m3d/DC3_Area_m2
#Q_DC1$q_md=Q_DC1$Q_m3d/DC1_Area_m2

Q_C1$q_md=Q_C1$Q_m3d/C1_Area_m2
Q_C2$q_md=Q_C2$Q_m3d/C2_Area_m2
Q_C4$q_md=Q_C4$Q_m3d/C4_Area_m2
Q_C18$q_md=Q_C18$Q_m3d/C18_Area_m2


#Open Meteo Daily
Meteo=read_xlsx("Input/Q and Meteo/Meteo_Daily.xlsx")
Meteo$Date=as.Date(Meteo$TimeStamp)
plot(y=Meteo$P,Meteo$TimeStamp, type="l")

# Add meteo data to Q_DC3
Q_DC3_Meteo=left_join(Q_DC3, Meteo, by = 'Date')


# Timeseries of Q in all sites 
ggplot()+
  geom_line(data=Q_DC2, aes(x=Date, y=q_md), color=2)+
  geom_line(data=Q_DC4, aes(x=Date, y=q_md), color=3)+
  geom_line(data=Q_DC3, aes(x=Date, y=q_md), color=4)+
  geom_line(data=Q_C1, aes(x=Date, y=q_md), color=5)+
  
  geom_line(data=Q_C2, aes(x=Date, y=q_md))+
  geom_line(data=Q_C4, aes(x=Date, y=q_md), color=6)+
  geom_line(data=Q_C18, aes(x=Date, y=q_md), color=7)+
  scale_x_date(limits= c(as.Date("2020-01-01"), as.Date("2023-12-31")))


#________________________________________________________________________________________
#________________________________________________________________________________________
#________________________________________________________________________________________


#Open dataset
C14_wide_chemistry_clean <- readRDS(file = "Output/Data/C14_wide_chemistry.rds")

colnames(C14_wide_chemistry_clean)[4]="Date"




#________________________________________________________________________________________
# Join databases
#________________________________________________________________________________________

C2= full_join(Q_C2[,3:5],
              filter(C14_wide_chemistry_clean, Site_id == "C2"), 
              by=join_by("Date"),
              suffix = c(" ", " "))

C4= full_join(Q_C4[,3:5],
              filter(C14_wide_chemistry_clean, Site_id == "C4"), 
              by=join_by("Date"),
              suffix = c(" ", " "))

C18= full_join(Q_C18[,3:5],
               filter(C14_wide_chemistry_clean, Site_id == "C18"), 
               by=join_by("Date"),
               suffix = c(" ", " "))

C1= full_join(Q_C1[,3:5],
              filter(C14_wide_chemistry_clean, Site_id == "C1"), 
              by=join_by("Date"),
              suffix = c(" ", " "))

DC2= full_join(Q_DC3_Meteo[,c(3:5,12)],# Merge with Q data from DC3
               filter(C14_wide_chemistry_clean, Site_id == "DC2"), 
               by=join_by("Date"),
               suffix = c(" ", " "))
DC2$Site_id=rep("DC2", nrow(DC2))


DC3= full_join(Q_DC3_Meteo[,c(3:5,12)],
               filter(C14_wide_chemistry_clean, Site_id == "DC3"), 
               by=join_by("Date"),
               suffix = c(" ", " "))
DC3$Site_id=rep("DC3", nrow(DC3))

DC4= full_join(Q_DC3_Meteo[,c(3:5,12)], # Merge with Q data from DC3
               filter(C14_wide_chemistry_clean, Site_id == "DC4"), 
               by=join_by("Date"),
               suffix = c(" ", " "))
DC4$Site_id=rep("DC4", nrow(DC4))

DC1= full_join(Q_DC3_Meteo[,c(3:5,12)], # Merge with Q data from DC3
               filter(C14_wide_chemistry_clean, Site_id == "DC1"), 
               by=join_by("Date"),
               suffix = c(" ", " "))
DC1$Site_id=rep("DC1", nrow(DC1))

