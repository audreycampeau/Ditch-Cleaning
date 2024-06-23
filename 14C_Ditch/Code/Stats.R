


#Stats tests

lm_14CDOC_q_DC1.3_trtmCCDC= lm(DOC_14C_Modern~q_md, 
                                 data=filter(DC1_DC3, Treatment %in% c("After clearcut & Ditch cleaning")))
                              
shapiro.test(lm_14CDOC_q_DC1.3_trtmCCDC$residuals)


lm_14CDOC_q_DC1.3_trtmCC= lm(DOC_14C_Modern~q_md, 
   data=filter(DC1_DC3, Treatment %in% c("After clearcut" )))
        
shapiro.test(lm_14CDOC_q_DC1.3_trtmCC$residuals)

summary(lm(DOC_14C_Modern~q_md, 
           data=filter(DC_Q, Treatment %in% c("Pristine")))
        )


kruskal.test(DOC_14C_Modern ~ Treatment, data = DC1_DC3)
kruskal.test(CO2_14C_Modern ~ Treatment, data = DC1_DC3)


# Dunn's test in R
library(tidyverse)
library(dunn.test)
library(multcompView)
library(PMCMR)



#shapiro.test(DC1_DC3$DOC_14C_Modern)


ggplot(data=DC1_DC3)+
  geom_boxplot(aes(x=Treatment, y=DOC_14C_Modern))

dunnTest(DOC_14C_Modern ~ Treatment, data = DC1_DC3, method = "bonferroni")
dunnTest(CO2_14C_Modern ~ Treatment, data = DC1_DC3, method = "bonferroni")


# Remove rows with non-finite values
DC1_DC3_clean <- DC1_DC3 %>%
  filter(is.finite(DOC_14C_Modern) & is.finite(Treatment))



# Perform the test and get multcomp letters
multcompLetters(get.pvalues(PMCMRplus::kwAllPairsDunnTest(
  DC1_DC3$DOC_14C_Modern ~ DC1_DC3$Treatment, p.adjust="bonf")),
  threshold=0.05)


multcompLetters(get.pvalues(PMCMRplus::kwAllPairsDunnTest(
  DC_Q$DOC_14C_Modern ~ DC_Q$Treatment, p.adjust="bonf")),
  threshold=0.05)

summary(lm(DOC_14C_Modern~q_md, 
           data=filter(DC_Q, Treatment %in% c("After clearcut & Ditch cleaning")))
)


summary(lm(DOC_14C_Modern ~ q_md, 
           data=filter(DC_All_Q, Treatment %in% c("Pristine")) %>%
             filter (Site_id %in% c("DC1", "DC2", "DC3", "DC4", "C1")))
)

filter(DC1_DC3, Treatment %in% c("After clearcut & Ditch cleaning","After clearcut", "Pristine" )) %>% 
  anova_test(DOC_14C_Modern ~ q_md*Treatment)
