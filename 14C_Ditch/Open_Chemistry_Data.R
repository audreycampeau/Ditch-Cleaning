
#Open and merge datasets
library(readxl)
library(lubridate)
library(tidyverse)



# Reopen database
C14_wide_chemistry=read.csv("/Users/audreycampeau/Documents/DATA/TROLLBERGET DITCH/C14_wide_chemistry_data_2.csv")
#read_xlsx("/Users/audreycampeau/Documents/DATA/TROLLBERGET DITCH/C14_wide_chemistry_data_2.xlsx", sheet=2)
colnames(C14_wide_chemistry)
#C14_wide_chemistry[,c(28:40)]=sapply(C14_wide_chemistry[,c(28:40)], FUN=as.numeric)

C14_wide_chemistry$Date=as.Date(C14_wide_chemistry$Date)


DC_database=filter(C14_wide_chemistry, Site_id %in% c("DC1","DC2","DC3","DC4"))


# Calculate excess deuterieum (Index of evaporation)
C14_wide_chemistry$dexcess= C14_wide_chemistry$d2H - (8*C14_wide_chemistry$d18O_chem)
DC_database$dexcess= DC_database$d2H - (8*DC_database$d18O_chem)

#_________________________________________________________________________________________________
# Codes to generate the C14_wide_chemistry database




#Open General Chemistry database
#file.choose()
chemistry=read_xlsx("/Users/audreycampeau/Documents/DATA/TROLLBERGET DITCH/Second Batch of Data/Trollberget data - 2024-04-12.xlsx", sheet=1)
#chemistry=read_xlsx("/Users/audreycampeau/Documents/DATA/TROLLBERGET DITCH/Trollberget_chemistry DC sites_compiled_1403_AC.xlsx", sheet=3)
chemistry$Date=as.Date(chemistry$Date)
chemistry[,3:26]=sapply(chemistry[,3:26], as.numeric)



# Merge chemistry with Marcus's DIC calculations
#file.choose()
DIC_MW=read_xlsx("/Users/audreycampeau/Documents/DATA/TROLLBERGET DITCH/DC&C_Trollberget_GHG_data_AZ.xlsx", sheet=1)
DIC_MW$Date=as.Date(DIC_MW$Date)
DIC_MW=DIC_MW[,c(1:2, 4:8)] # Erase the last two columns that contained Marcus comment
DIC_MW$Date_MW=DIC_MW$Date #Copy Marcus Date
colnames(DIC_MW)=c("Site_id", "Date","pH_MW","WT_MW","DIC_mgL_MW","CO2_mgL_MW",
                   "CH4_ugL_MW","Date_MW")


# Join General chemistry with C14_wide data
chemistry_DIC_MW= full_join(chemistry, 
                            DIC_MW, 
                              by=join_by("Site_id", 
                                         "Date"),
                                         #closest(Date>=Date)),
                              suffix = c(" ", " "))

chemistry_DIC_MW_cleaned=chemistry_DIC_MW[ -which(is.na(chemistry_DIC_MW$DOC_mgL)), ]


write.csv(chemistry_DIC_MW_cleaned, "/Users/audreycampeau/Documents/DATA/TROLLBERGET DITCH/chemistry_DIC_MW_2024.06.03.csv")


#Reopen Chemistry with Marcu's data, and duplicated removed (by hand)
#file.choose()
Chemistry=read.csv("/Users/audreycampeau/Documents/DATA/TROLLBERGET DITCH/chemistry_DIC_MW_2024.06.03.csv")
Chemistry$Date=as.Date(Chemistry$Date)
colnames(Chemistry)



# Derive Ratios______________________________________________________

# Alkalinity
Chemistry$Alk_mgL=Chemistry$DIC_mgL_MW - Chemistry$CO2_mgL_MW

# (Ca+Mg)/Alk (like in Marcus' Klaus Paper on GW)
Chemistry$Ca_Mg_Alk=((Chemistry$Ca_ugL/40.078) + (Chemistry$Mg_ugL/24.3050)) / (Chemistry$Alk_mgL/12.01)

#OrgN
Chemistry$Norg_mgL=Chemistry$DN_mgL - ((Chemistry$NO2_NO3_ugL+Chemistry$NH4_ugL)/1000)

#C:N (org)
Chemistry$C_N_org=(Chemistry$Norg_mgL/14.01 )/(Chemistry$DOC_mgL/12.01)

#Na:Mg
Chemistry$Na_Mg=(Chemistry$Na_ugL/22.989770)/(Chemistry$Mg_ugL/24.3050)



