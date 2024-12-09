

#Dot plots 14C ~ treatment ~ sites


ggplot(filter(DC_Q, Treatment %in% c("Ditch cleaning","Clearcut", "Pristine" )),
       aes(fill=Site_id, y=DOC_14C_Modern, x=Treatment))+
  geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 0.1)#+
  #scale_fill_manual(values = c( "#fc9272ff","#de2d26ff","deepskyblue"))
C14_wide_chemistry$Treatment
View(DC_Q)

     