#Assessing for correlation

  variablelist<-c("bovines", "residence_type", "n_household_members", "IAP", "smokes_indoors", "health_scheme","wealth_index", "state")
  cramervals<-data.frame(matrix(NA, nrow = 0, ncol = length(variablelist)))
  colnames(cramervals)<-variablelist
  cramervalvec<-c()
  for (i in variablelist){
    for (m in variablelist) {
      
        cramervalvec[m]<-round((cramerV(x=pull(data_household[,i]), y=pull(data_household[,m]))),3)
    }
    cramervals[i,]<-cramervalvec
  }
  
cramervals<-cramervals %>% 
  dplyr::rename("residence"=residence_type, "household size"=n_household_members, "IAP"=IAP, "smokes indoors"=smokes_indoors, 
         "bovines"=bovines, "health scheme"=health_scheme, "wealth index"=wealth_index, "state"=state)
 
  cor_HH<-ggcorr_edit(data=NULL, cor_matrix=as.matrix(cramervals),
                  low="#f7f7f7", mid="#e08214", high="#7f3b08", #specify colours for low, mid and high values 
         midpoint=0.5, #specify midpoint of scale
         limits=c(0, 1), #limits of Cramer's v 0-1
         #label=TRUE, #display correlation coefficient in figure
         label_round=2, #decimal rounding of correlation coefficient
         size = 5, #text size
         hjust = 0.75,
         angle = -35,
         label_size = 4,
         legend.size=15,
         layout.exp=1.5, #expand horizontal axis to avoid variable names being cut
         family="serif",
         name="Cramer's V \ncorrelation\n")+
    theme(text = element_text(size = 20, family="serif"),
          legend.title = element_text(size = 20))
  
  ggsave("./figures/corHH.png", width=10, height=8)
  
