#Calibration of selected models


require(ROCR)

#x<-(prediction(inla_household_base_binom$summary.fitted.values$mode,data_household$TB_present==1))
#y<-(prediction(inla_household_iid_binom$summary.fitted.values$mode,data_household$TB_present==1))
#z<-(prediction(inla_household_bym_binom$summary.fitted.values$mode,data_household$TB_present==1))

#no mode available - listed as NA, reason? use mean instead


require(ROCR)

#1a.  household model for self-reported TB with bovine ownership as exposure variable

#plot ROC curve for three different models
x<-(prediction(inla_household_base_binom1$summary.fitted.values$mean,data_household$TB_present==1))
y<-(prediction(inla_household_iid_binom1$summary.fitted.values$mean,data_household$TB_present==1))
z<-(prediction(inla_household_bym_binom1$summary.fitted.values$mean,data_household$TB_present==1))
plot(performance(x,'tpr','fpr'))
plot(performance(y,'tpr','fpr'),col='red',add=T)
plot(performance(z,'tpr','fpr'),col='blue',add=T)
abline(a=0,b=1)

data_household <- data_household %>% mutate(pred=inla_household_bym_binom1$summary.fitted.values$mean) #use BYM model instead

mkAUC <- function(obs,pred)
{
  # Return missing value if observed data is all zeros or ones
  if(sum(obs)==0 | sum(obs)==length(pred)){return(NA)}else{
    return(performance(prediction(pred,obs),measure='auc')@y.values[[1]])}
}

map_AUC <- data_household %>% group_by(state,district) %>% summarise(AUC = mkAUC(TB_present==1,pred))

ggplot(simp_map %>% 
         left_join(map_AUC),
       aes(fill=AUC)) +
  geom_sf(lwd=0) +
  labs(fill="AUC")+
  scale_fill_viridis(option = "plasma", direction=-1)+
  theme_void() +
  theme(text=element_text(size=18, family="serif"))


#2b.  household model for self-reported TB with bovine density as exposure variable

#plot ROC curve for three different models
x<-(prediction(inla_household_base_binom2$summary.fitted.values$mean,data_household$TB_present==1))
y<-(prediction(inla_household_iid_binom2$summary.fitted.values$mean,data_household$TB_present==1))
z<-(prediction(inla_household_bym_binom2$summary.fitted.values$mean,data_household$TB_present==1))
plot(performance(x,'tpr','fpr'))
plot(performance(y,'tpr','fpr'),col='red',add=T)
plot(performance(z,'tpr','fpr'),col='blue',add=T)
abline(a=0,b=1)

data_household <- data_household %>% mutate(pred=inla_household_bym_binom2$summary.fitted.values$mean) #use BYM model instead

mkAUC <- function(obs,pred)
{
  # Return missing value if observed data is all zeros or ones
  if(sum(obs)==0 | sum(obs)==length(pred)){return(NA)}else{
    return(performance(prediction(pred,obs),measure='auc')@y.values[[1]])}
}

map_AUC <- data_household %>% group_by(state,district) %>% summarise(AUC = mkAUC(TB_present==1,pred))

ggplot(simp_map %>% 
         left_join(map_AUC),
       aes(fill=AUC)) +
  geom_sf(lwd=0) +
  labs(fill="AUC")+
  scale_fill_viridis(option = "plasma", direction=-1)+
  theme_void() +
  theme(text=element_text(size=18, family="serif"))


#district level

diagnose_model <- function(model,data_obs){
  par(mfrow=c(2,2))
  plot(model$summary.fitted.values$mode,data_obs,pch=19,log='xy',xlab='Predicted',ylab='Observations')
  abline(a=0,b=1,col='red',lwd=2)
  
  pit <- model$cpo$pit
  cpo <- model$cpo$cpo
  
  hist(pit,probability = T)
  
  plot(abs(model$summary.fitted.values$mode-data_obs),-log(cpo),pch=19,xlab='|pred-obs|')
  
  z=sort(pit)
  uniquant <- (1:length(z))/(length(z)+1)
  plot(uniquant,z , xlab="uniform quantiles", ylab="Sorted PIT values",pch=19)
  
  abline(0,1,col='red',lwd=2)
  
}

#2a. district level with officially reported TB and combined district level and aggregated HH predictor variables
#with bovine ownership as exposure variable

diagnose_model(inla_district_notified_bym_NB1,data_household_agg$human_TB_notified)

ggplot(simp_map %>% 
         mutate(lcpo=-log(inla_district_notified_bym_NB1$cpo$cpo)),
       aes(fill=lcpo)) + 
  geom_sf(lwd=0) + 
  labs(fill="log cpo")+
  scale_fill_viridis(option = "plasma", direction=-1)+
  theme_void()  + 
  theme(text=element_text(size=18, family="serif"))


#2b. with bovine density as exposure variable

diagnose_model(inla_district_notified_bym_NB2,data_household_agg$human_TB_notified)

ggplot(simp_map %>% 
         mutate(lcpo=-log(inla_district_notified_bym_NB2$cpo$cpo)),
       aes(fill=lcpo)) + 
  geom_sf(lwd=0) + 
  labs(fill="log cpo")+
  scale_fill_viridis(option = "plasma", direction=-1)+
  theme_void()  + 
  theme(text=element_text(size=18, family="serif"))


#3a. district level with aggregated household level of self-reported TB combined with district level predictor variables
#with bovine ownership as exposure variable

diagnose_model(inla_district_households_bym_NB1, data_household_agg$HH_tb)

ggplot(simp_map %>% 
         mutate(lcpo=-log(inla_district_households_bym_NB1$cpo$cpo)),
       aes(fill=(lcpo))) + 
  geom_sf(lwd=0) + 
  labs(fill="log cpo")+
  scale_fill_viridis(option = "plasma", direction=-1)+
  theme_void() + 
  theme(text=element_text(size=18, family="serif"))


#3b.with bovine density as exposure variable

diagnose_model(inla_district_households_bym_NB2, data_household_agg$HH_tb)

ggplot(simp_map %>% 
         mutate(lcpo=-log(inla_district_households_bym_NB2$cpo$cpo)),
       aes(fill=(lcpo))) + 
  geom_sf(lwd=0) + 
  labs(fill="log cpo")+
  scale_fill_viridis(option = "plasma", direction=-1)+
  theme_void() + 
  theme(text=element_text(size=18, family="serif"))








