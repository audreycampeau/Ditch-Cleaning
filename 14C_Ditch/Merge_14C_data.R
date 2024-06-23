#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#Open C14 Wide database
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

C14_wide=read_xlsx("/Users/audreycampeau/Documents/DATA/TROLLBERGET DITCH/C14_wide_chemistry_data.xlsx", sheet=1)
C14_wide$Date=as.Date(C14_wide$Date)


# Join General chemistry with C14_wide data
C14_wide_chemistry= full_join(C14_wide, 
                              Chemistry, 
                              by=join_by("Site_id", 
                                         closest(Date>=Date)),
                              suffix = c(" ", "_chem"))



colnames(C14_wide_chemistry)[4]=c("Date")

C14_wide_chemistry$Date_chem=as.Date(C14_wide_chemistry$Date_chem)
C14_wide_chemistry$Date_chem_chem=as.Date(C14_wide_chemistry$Date_chem_chem)

C14_wide_chemistry$Date <- as.Date(ifelse(is.na(C14_wide_chemistry$Date),
                                          C14_wide_chemistry$Date_chem_chem, 
                                          C14_wide_chemistry$Date)) 

write.csv(C14_wide_chemistry, "/Users/audreycampeau/Documents/DATA/TROLLBERGET DITCH/C14_wide_chemistry_data_3.csv")



#geom_point(aes (x=Q, y=CO2_14C_Modern), shape=2)+
#scale_x_log10()+
#scale_y_continuous(limits=c(95,115))
