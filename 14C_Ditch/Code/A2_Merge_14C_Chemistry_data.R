#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#Open C14 Wide database
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
library(readxl)
C14_wide=read_xlsx("Input/C14_wide_data.xlsx", sheet=1)
C14_wide$Date=as.Date(C14_wide$Date)





#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#Open Chemistry with Marcu's data
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

Chemistry=read.csv("Output/Data/chemistry_DIC_MW_2024.06.03.csv")
Chemistry$Date=as.Date(Chemistry$Date)


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



#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Join General chemistry with C14_wide data
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

C14_wide_chemistry= full_join(C14_wide, 
                              Chemistry, 
                              by=join_by("Site_id", 
                                         closest(Date>=Date)),
                              suffix = c("", "_chem"))




C14_wide_chemistry$Date_chem=as.Date(C14_wide_chemistry$Date_chem)
C14_wide_chemistry$Date_chem_chem=as.Date(C14_wide_chemistry$Date_chem_chem)

C14_wide_chemistry$Date <- as.Date(ifelse(is.na(C14_wide_chemistry$Date),
                                          C14_wide_chemistry$Date_chem_chem, 
                                          C14_wide_chemistry$Date)) 


#Order factors
#C14_wide_chemistry$Treatment <- factor(DC_Q$Treatment, #reorder the treatment types
#                         levels = c("Pristine", "Clearcut", "Ditch cleaning"))


colnames(C14_wide_chemistry)




#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Merge chemistry data from C14_wide (incomplete) with Chemistry and MW data (more complete)
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#Merge CO2
C14_wide_chemistry$CO2_mgL=ifelse(is.na(C14_wide_chemistry$CO2_mgL), C14_wide_chemistry$CO2_mgL_chem, C14_wide_chemistry$CO2_mgL)
C14_wide_chemistry$CO2_mgL=ifelse(is.na(C14_wide_chemistry$CO2_mgL), C14_wide_chemistry$CO2_mgL_MW, C14_wide_chemistry$CO2_mgL)

#Merge pH
C14_wide_chemistry$pH=ifelse(is.na(C14_wide_chemistry$pH), C14_wide_chemistry$pH_chem, C14_wide_chemistry$pH)
C14_wide_chemistry$pH=ifelse(is.na(C14_wide_chemistry$pH), C14_wide_chemistry$pH_MW, C14_wide_chemistry$pH)

#Merge Conductivity
C14_wide_chemistry$Cond_uScm=ifelse(is.na(C14_wide_chemistry$Cond_uScm), C14_wide_chemistry$EC_uScm, C14_wide_chemistry$Cond_uScm)

#Merge DOC
C14_wide_chemistry$DOC_mgL=ifelse(is.na(C14_wide_chemistry$DOC_mgL), C14_wide_chemistry$DOC_mgL_chem, C14_wide_chemistry$DOC_mgL)
C14_wide_chemistry$DOC_mgL=ifelse(is.na(C14_wide_chemistry$DOC_mgL), C14_wide_chemistry$DOCmgL_14C_DOC, C14_wide_chemistry$DOC_mgL) # Merge DOC from 14C analysis too


#Merge NO2_NO3
C14_wide_chemistry$NO2_NO3_ugL=ifelse(is.na(C14_wide_chemistry$NO2_NO3_ugL), C14_wide_chemistry$NO2_NO3_ugL_chem, C14_wide_chemistry$NO2_NO3_ugL)

#Merge DN
C14_wide_chemistry$DN_mgL=ifelse(is.na(C14_wide_chemistry$DN_mgL), C14_wide_chemistry$DN_mgL_chem, C14_wide_chemistry$DN_mgL)

#Merge NH4
C14_wide_chemistry$NH4_ugL=ifelse(is.na(C14_wide_chemistry$NH4_ugL), C14_wide_chemistry$NH4_ugL_chem, C14_wide_chemistry$NH4_ugL)

#Merge PO4
C14_wide_chemistry$PO4_ugL=ifelse(is.na(C14_wide_chemistry$PO4_ugL), C14_wide_chemistry$PO4_ugL_chem, C14_wide_chemistry$PO4_ugL)

#Merge SO4
C14_wide_chemistry$SO4_ugL=ifelse(is.na(C14_wide_chemistry$SO4_ugL), C14_wide_chemistry$SO4_ugL_chem, C14_wide_chemistry$SO4_ugL)

#Merge Fe
C14_wide_chemistry$Fe_ugL=ifelse(is.na(C14_wide_chemistry$Fe_ugL), C14_wide_chemistry$Fe_ugL_chem, C14_wide_chemistry$Fe_ugL)

#Merge d18O
C14_wide_chemistry$d18O=ifelse(is.na(C14_wide_chemistry$d18O), C14_wide_chemistry$d18O_chem, C14_wide_chemistry$d18O)

#Merge DIC
C14_wide_chemistry$DIC_mgL=ifelse(is.na(C14_wide_chemistry$DIC_mgL), C14_wide_chemistry$DIC_mgL_chem, C14_wide_chemistry$DIC_mgL)
C14_wide_chemistry$DIC_mgL=ifelse(is.na(C14_wide_chemistry$DIC_mgL), C14_wide_chemistry$DIC_mgL_MW, C14_wide_chemistry$DIC_mgL)




#Merge pCO2
C14_wide_chemistry$pCO2_uatm=ifelse(is.na(C14_wide_chemistry$pCO2_uatm), C14_wide_chemistry$pCO2_uatm_chem, C14_wide_chemistry$pCO2_uatm)

#Merge CH4_
C14_wide_chemistry$CH4_ugL=ifelse(is.na(C14_wide_chemistry$CH4_ugL), C14_wide_chemistry$CH4_ugL_chem, C14_wide_chemistry$CH4_ugL)
C14_wide_chemistry$CH4_ugL=ifelse(is.na(C14_wide_chemistry$CH4_ugL), C14_wide_chemistry$CH4_ugL_MW, C14_wide_chemistry$CH4_ugL)


# Drop columns from _chem and _MW that I no longer need
C14_wide_chemistry_clean <- C14_wide_chemistry %>% select(-CO2_mgL_chem, -CO2_mgL_MW,
                                                            -pH_chem, -pH_MW,
                                                            -EC_uScm,
                                                            -DOC_mgL_chem,
                                                            -NO2_NO3_ugL_chem,
                                                            -DN_mgL_chem,
                                                            -NH4_ugL_chem,
                                                            -PO4_ugL_chem,
                                                            -SO4_ugL_chem,
                                                            -Fe_ugL_chem,
                                                            -d18O_chem,
                                                            -DIC_mgL_chem, -DIC_mgL_MW,
                                                            -pCO2_uatm_chem,
                                                            -CH4_ugL_chem, -CH4_ugL_MW,
                                                            -X, -Date_chem_chem, -Date_MW 
                                                            )



print(tibble(colnames(C14_wide_chemistry_clean)), n=70)              


#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#Identify duplicate rows
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


# Check for complete duplicates across all columns
complete_duplicates = C14_wide_chemistry_clean[duplicated(C14_wide_chemistry_clean), ]

# Check for duplicates based on Site_id and Date only
key_duplicates = C14_wide_chemistry_clean[duplicated(C14_wide_chemistry_clean[, c("Publication Code_DOC")]) | 
                                            duplicated(C14_wide_chemistry_clean[, c("Publication Code_DOC")], fromLast = TRUE), ]


# Inspect the duplicate rows

key_duplicates[, c("n", "Publication Code_DOC", "DOC_14C_Modern")]

row_to_delete = key_duplicates$n #get row id for all duplicated rows

row_to_delete=row_to_delete[seq(2, length(row_to_delete), 2)] #get row id for every second row


C14_wide_chemistry_clean=C14_wide_chemistry_clean[-c(row_to_delete[1:3]),]




saveRDS(C14_wide_chemistry_clean, "Output/Data/C14_wide_chemistry.rds")

