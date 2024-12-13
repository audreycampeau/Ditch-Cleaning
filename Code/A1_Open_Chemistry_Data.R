
#Open and merge datasets
library(readxl)
library(lubridate)
library(tidyverse)



# Reopen database
#C14_wide_chemistry=read.csv("/Users/audreycampeau/Documents/DATA/TROLLBERGET DITCH/C14_wide_chemistry_data_2.csv")
#read_xlsx("/Users/audreycampeau/Documents/DATA/TROLLBERGET DITCH/C14_wide_chemistry_data_2.xlsx", sheet=2)
#colnames(C14_wide_chemistry)
#C14_wide_chemistry[,c(28:40)]=sapply(C14_wide_chemistry[,c(28:40)], FUN=as.numeric)

#C14_wide_chemistry$Date=as.Date(C14_wide_chemistry$Date)


#DC_database=filter(C14_wide_chemistry, Site_id %in% c("DC1","DC2","DC3","DC4"))


# Calculate excess deuterieum (Index of evaporation)
#C14_wide_chemistry$dexcess= C14_wide_chemistry$d2H - (8*C14_wide_chemistry$d18O_chem)
#DC_database$dexcess= DC_database$d2H - (8*DC_database$d18O_chem)



#_________________________________________________________________________________________________
# Open general water chemistry database (from second batch of data)


#Open General Chemistry database
chemistry=read_xlsx("Input/Trollberget data - 2024-04-12.xlsx", sheet=1)
chemistry$Date=as.Date(chemistry$Date)
chemistry[,3:26]=sapply(chemistry[,3:26], as.numeric)


#_________________________________________________________________________________________________
# Open results from Marcus' DIC calculations (from Third batch of data)

DIC_MW=read_xlsx("Input/DC&C_Trollberget_GHG_data_AZ.xlsx", sheet=1)
DIC_MW$Date=as.Date(DIC_MW$Date)
DIC_MW=DIC_MW[,c(1:2, 4:8)] # Erase the last two columns that contained Marcus comment
DIC_MW$Date_MW=DIC_MW$Date #Copy Marcus Date
colnames(DIC_MW)=c("Site_id", "Date","pH_MW","WT_MW","DIC_mgL_MW","CO2_mgL_MW", "CH4_ugL_MW","Date_MW")




#_________________________________________________________________________________________________
# Join General chemistry with DIC_MW data
chemistry_DIC_MW= full_join(chemistry, 
                            DIC_MW, 
                              by=join_by("Site_id", 
                                         "Date"),
                                         #closest(Date>=Date)),
                              suffix = c(" ", " "))


chemistry_DIC_MW_cleaned=chemistry_DIC_MW[ -which(is.na(chemistry_DIC_MW$DOC_mgL)), ]
#Make a row ID column
chemistry_DIC_MW_cleaned$row_id=seq(1,nrow(chemistry_DIC_MW_cleaned),1)




# Check for complete duplicates across all columns
complete_duplicates = chemistry_DIC_MW_cleaned[duplicated(chemistry_DIC_MW_cleaned), ]

# Check for duplicates based on Site_id and Date only
key_duplicates = chemistry_DIC_MW_cleaned[duplicated(chemistry_DIC_MW_cleaned[, c("Site_id", "Date")]) | 
                                       duplicated(chemistry_DIC_MW_cleaned[, c("Site_id", "Date")], fromLast = TRUE), ]


# Inspect the duplicate rows
key_duplicates[, c("row_id", "Site_id", "Date", "DOC_mgL", "DIC_mgL", "CO2_mgL")]

row_to_delete = key_duplicates$row_id #get row id for all duplicated rows

row_to_delete=row_to_delete[seq(2, length(row_to_delete), 2)] #get row id for every second row



# Remove duplicates keeping first occurrence
chemistry_DIC_MW_cleaned <- chemistry_DIC_MW_cleaned[-row_to_delete, ]



#Export the chemistry data
write.csv(chemistry_DIC_MW_cleaned, "Output/Data/chemistry_DIC_MW_2024.06.03.csv")








