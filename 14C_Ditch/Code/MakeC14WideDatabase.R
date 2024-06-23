#Open 14C Alberto database
#C14=read_xlsx("/Users/audreycampeau/Documents/DATA/TROLLBERGET DITCH/14C_Data_Combined_AC.xlsx")
#C14$Date=as.Date(C14$Date, format="%d-%m-%Y")
#C14$Material_Type=as.factor(C14$Material_Type)
#C14$DOY=lubridate::yday(C14$Date)
#C14$Year=lubridate::year(C14$Date)
#C14$CO2_vol_ml_or_DOCmgL=as.numeric(C14$CO2_vol_ml_or_DOCmgL)

#nrow(filter(C14, Material_Type == "CO2"))
#nrow(filter(C14, Material_Type == "DOC", Site_id!=c("WR1","WR2")))


# Join 14C-Co2 and 14C-DOC WIDE
#C14_wide=full_join(filter(C14, Material_Type == "DOC"), 
#                    filter(C14, Material_Type == "CO2"), 
#                    by=join_by("Site_id", 
#                               closest(Date >= Date)),
#                    suffix = c("_DOC", "_CO2"),
#                    )

#colnames(C14_wide)[3]=c("Date")
