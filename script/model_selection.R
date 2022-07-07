#Model selection

#INLA model 1a: household model for self reported TB with bovine ownership as exposure variable

load('./data/results/fitted_modelsH_bovine_ownership.RData')

summary(inla_household_base_binom1)
summary(inla_household_iid_binom1)
summary(inla_household_bym_binom1)

uniquantH <- (1:nrow(data_household))/(nrow(data_household)+1)

model_selectionHH1a <- tibble(exposure="bovine_ownership", response='Households', family='binom', spatial='null',
                            mlik = inla_household_base_binom1$mlik[1],
                            dic=inla_household_base_binom1$dic$dic,
                            waic=inla_household_base_binom1$waic$waic,
                            log_score=mean(-log(inla_household_base_binom1$cpo$cpo)))

model_selectionHH1a <- model_selectionHH1a %>% bind_rows(tibble(exposure="bovine_ownership", response='Households', family='binom', spatial='iid',
                                                            mlik = inla_household_iid_binom1$mlik[1],
                                                            dic=inla_household_iid_binom1$dic$dic,
                                                            waic=inla_household_iid_binom1$waic$waic,
                                                            log_score=mean(-log(inla_household_iid_binom1$cpo$cpo))))

model_selectionHH1a <- model_selectionHH1a %>% bind_rows(tibble(exposure="bovine_ownership", response='Households', family='binom', spatial='bym',
                                                            mlik = inla_household_bym_binom1$mlik[1],
                                                            dic=inla_household_bym_binom1$dic$dic,
                                                            waic=inla_household_bym_binom1$waic$waic,
                                                            log_score=mean(-log(inla_household_bym_binom1$cpo$cpo))))

#summary table of model selection
model_selection<-model_selectionHH1a %>% 
  arrange(dic) %>% 
  mutate(pit_unif=NA) 

#save selected model in separate file
save(inla_household_bym_binom1,
     file='./data/results/inla_household_bym_binom1.RData')


#INLA model 1b: household model for self reported TB with bovine density as exposure variable

#load data
load('./data/results/fitted_modelsH_bovine_density.RData')

summary(inla_household_base_binom2)
summary(inla_household_iid_binom2)
summary(inla_household_bym_binom2)

uniquantH <- (1:nrow(data_household))/(nrow(data_household)+1)

model_selectionHH1b <- tibble(exposure="bovine_density", response='Households', family='binom', spatial='null',
                            mlik = inla_household_base_binom2$mlik[1],
                            dic=inla_household_base_binom2$dic$dic,
                            waic=inla_household_base_binom2$waic$waic,
                            log_score=mean(-log(inla_household_base_binom2$cpo$cpo)))

model_selectionHH1b <- model_selectionHH1b %>% bind_rows(tibble(exposure="bovine_density", response='Households', family='binom', spatial='iid',
                                                            mlik = inla_household_iid_binom2$mlik[1],
                                                            dic=inla_household_iid_binom2$dic$dic,
                                                            waic=inla_household_iid_binom2$waic$waic,
                                                            log_score=mean(-log(inla_household_iid_binom2$cpo$cpo))))

model_selectionHH1b <- model_selectionHH1b %>% bind_rows(tibble(exposure="bovine_density", response='Households', family='binom', spatial='bym',
                                                            mlik = inla_household_bym_binom2$mlik[1],
                                                            dic=inla_household_bym_binom2$dic$dic,
                                                            waic=inla_household_bym_binom2$waic$waic,
                                                            log_score=mean(-log(inla_household_bym_binom2$cpo$cpo))))


#summary table of model selection
model_selection<-model_selection %>% 
  add_row(model_selectionHH1b %>% arrange(dic) %>% mutate(pit_unif=NA))

#save selected model in separate file
save(inla_household_bym_binom2,
     file='./data/results/inla_household_bym_binom2.RData')


##INLA model selection 2a: district level with officially reported TB and combined district level and aggregated HH predictor variables
#with bovine ownership as exposure variable

#load data
load('./data/results/fitted_modelsD.RData')

data_household_agg2 <- data_household_agg %>% 
  drop_na(human_TB_notified)  

uniquant<- (1:nrow(data_household_agg2))/(nrow(data_household_agg2)+1)

#null baseline model
model_selection_HHdistrict2a <- tibble(exposure="bovine_ownership", response='Notifications', family='NB', spatial='null',
                          mlik=inla_district_notified_base_NB1$mlik[1],
                          dic=inla_district_notified_base_NB1$dic$dic,
                          waic=inla_district_notified_base_NB1$waic$waic,
                          log_score=mean(-log(inla_district_notified_base_NB1$cpo$cpo),na.rm=T),
                          pit_unif=mean(sort(inla_district_notified_base_NB1$cpo$pit)*
                                          log(sort(inla_district_notified_base_NB1$cpo$pit)/uniquant),na.rm=T))

#spatial random effect term: independent identically distributed (IID)
model_selection_HHdistrict2a <- model_selection_HHdistrict2a %>% bind_rows(tibble(exposure="bovine_ownership", response='Notifications', family='NB', spatial='iid',
                                                        mlik=inla_district_notified_iid_NB1$mlik[1],
                                                        dic=inla_district_notified_iid_NB1$dic$dic,
                                                        waic=inla_district_notified_iid_NB1$waic$waic,
                                                        log_score=mean(-log(inla_district_notified_iid_NB1$cpo$cpo),na.rm=T),
                                                        pit_unif=mean(sort(inla_district_notified_iid_NB1$cpo$pit)*
                                                                        log(sort(inla_district_notified_iid_NB1$cpo$pit)/uniquant),na.rm=T)))

#spatial random effect term: Besag-York-Mollie (BYM)
model_selection_HHdistrict2a <- model_selection_HHdistrict2a %>% bind_rows(tibble(exposure="bovine_ownership", response='Notifications', family='NB', spatial='bym',
                                                        mlik=inla_district_notified_bym_NB1$mlik[1],
                                                        dic=inla_district_notified_bym_NB1$dic$dic,
                                                        waic=inla_district_notified_bym_NB1$waic$waic,
                                                        log_score=mean(-log(inla_district_notified_bym_NB1$cpo$cpo),na.rm=T),
                                                        pit_unif=mean(sort(inla_district_notified_bym_NB1$cpo$pit)*
                                                                        log(sort(inla_district_notified_bym_NB1$cpo$pit)/uniquant),na.rm=T)))

##INLA model selection 2b: district level with officially reported TB and combined district level and aggregated HH predictor variables
#with bovine density as exposure variable

data_household_agg2 <- data_household_agg %>% 
  drop_na(human_TB_notified)  

uniquant<- (1:nrow(data_household_agg2))/(nrow(data_household_agg2)+1)

#null baseline model
model_selection_HHdistrict2b <- tibble(exposure="bovine_density", response='Notifications', family='NB', spatial='null',
                                      mlik=inla_district_notified_base_NB2$mlik[1],
                                      dic=inla_district_notified_base_NB2$dic$dic,
                                      waic=inla_district_notified_base_NB2$waic$waic,
                                      log_score=mean(-log(inla_district_notified_base_NB2$cpo$cpo),na.rm=T),
                                      pit_unif=mean(sort(inla_district_notified_base_NB2$cpo$pit)*
                                                      log(sort(inla_district_notified_base_NB2$cpo$pit)/uniquant),na.rm=T))

#spatial random effect term: independent identically distributed (IID)
model_selection_HHdistrict2b <- model_selection_HHdistrict2b %>% bind_rows(tibble(exposure="bovine_density", response='Notifications', family='NB', spatial='iid',
                                                                                mlik=inla_district_notified_iid_NB2$mlik[1],
                                                                                dic=inla_district_notified_iid_NB2$dic$dic,
                                                                                waic=inla_district_notified_iid_NB2$waic$waic,
                                                                                log_score=mean(-log(inla_district_notified_iid_NB2$cpo$cpo),na.rm=T),
                                                                                pit_unif=mean(sort(inla_district_notified_iid_NB2$cpo$pit)*
                                                                                                log(sort(inla_district_notified_iid_NB2$cpo$pit)/uniquant),na.rm=T)))

#spatial random effect term: Besag-York-Mollie (BYM)
model_selection_HHdistrict2b <- model_selection_HHdistrict2b %>% bind_rows(tibble(exposure="bovine_density", response='Notifications', family='NB', spatial='bym',
                                                                                mlik=inla_district_notified_bym_NB2$mlik[1],
                                                                                dic=inla_district_notified_bym_NB2$dic$dic,
                                                                                waic=inla_district_notified_bym_NB2$waic$waic,
                                                                                log_score=mean(-log(inla_district_notified_bym_NB2$cpo$cpo),na.rm=T),
                                                                                pit_unif=mean(sort(inla_district_notified_bym_NB2$cpo$pit)*
                                                                                                log(sort(inla_district_notified_bym_NB2$cpo$pit)/uniquant),na.rm=T)))





#INLA model selection 3a: district level with aggregated household level of self-reported TB combined with district level predictor variables
#with bovine ownership as exposure variable

uniquant <- (1:nrow(data_household_agg))/(nrow(data_household_agg)+1)

model_selection_HHdistrict3a <- tibble(exposure="bovine_ownership", response='Households', family='NB', spatial='null',
                           mlik = inla_district_households_base_NB1$mlik[1],
                           dic=inla_district_households_base_NB1$dic$dic,
                           waic=inla_district_households_base_NB1$waic$waic,
                           log_score=mean(-log(inla_district_households_base_NB1$cpo$cpo),na.rm=T),
                           pit_unif=mean(sort(inla_district_households_base_NB1$cpo$pit)*
                                           log(sort(inla_district_households_base_NB1$cpo$pit)/uniquant),na.rm=T))

model_selection_HHdistrict3a <- model_selection_HHdistrict3a %>% bind_rows(tibble(exposure="bovine_ownership", response='Households', family='NB', spatial='iid',
                                                          mlik = inla_district_households_iid_NB1$mlik[1],
                                                          dic=inla_district_households_iid_NB1$dic$dic,
                                                          waic=inla_district_households_iid_NB1$waic$waic,
                                                          log_score=mean(-log(inla_district_households_iid_NB1$cpo$cpo),na.rm=T),
                                                          pit_unif=mean(sort(inla_district_households_iid_NB1$cpo$pit)*
                                                                          log(sort(inla_district_households_iid_NB1$cpo$pit)/uniquant),na.rm=T)))

model_selection_HHdistrict3a <- model_selection_HHdistrict3a %>% bind_rows(tibble(exposure="bovine_ownership", response='Households', family='NB', spatial='bym',
                                                          mlik = inla_district_households_bym_NB1$mlik[1],
                                                          dic=inla_district_households_bym_NB1$dic$dic,
                                                          waic=inla_district_households_bym_NB1$waic$waic,
                                                          log_score=mean(-log(inla_district_households_bym_NB1$cpo$cpo),na.rm=T),
                                                          pit_unif=mean(sort(inla_district_households_bym_NB1$cpo$pit)*
                                                                          log(sort(inla_district_households_bym_NB1$cpo$pit)/uniquant),na.rm=T)))


#INLA model selection 3b: district level with aggregated household level of self-reported TB combined with district level predictor variables
#with bovine density as exposure variable

uniquant <- (1:nrow(data_household_agg))/(nrow(data_household_agg)+1)

model_selection_HHdistrict3b <- tibble(exposure="bovine_density", response='Households', family='NB', spatial='null',
                                      mlik = inla_district_households_base_NB2$mlik[1],
                                      dic=inla_district_households_base_NB2$dic$dic,
                                      waic=inla_district_households_base_NB2$waic$waic,
                                      log_score=mean(-log(inla_district_households_base_NB2$cpo$cpo),na.rm=T),
                                      pit_unif=mean(sort(inla_district_households_base_NB2$cpo$pit)*
                                                      log(sort(inla_district_households_base_NB2$cpo$pit)/uniquant),na.rm=T))

model_selection_HHdistrict3b <- model_selection_HHdistrict3b %>% bind_rows(tibble(exposure="bovine_density", response='Households', family='NB', spatial='iid',
                                                                                mlik = inla_district_households_iid_NB2$mlik[1],
                                                                                dic=inla_district_households_iid_NB2$dic$dic,
                                                                                waic=inla_district_households_iid_NB2$waic$waic,
                                                                                log_score=mean(-log(inla_district_households_iid_NB2$cpo$cpo),na.rm=T),
                                                                                pit_unif=mean(sort(inla_district_households_iid_NB2$cpo$pit)*
                                                                                                log(sort(inla_district_households_iid_NB2$cpo$pit)/uniquant),na.rm=T)))

model_selection_HHdistrict3b <- model_selection_HHdistrict3b %>% bind_rows(tibble(exposure="bovine_density", response='Households', family='NB', spatial='bym',
                                                                                mlik = inla_district_households_bym_NB2$mlik[1],
                                                                                dic=inla_district_households_bym_NB2$dic$dic,
                                                                                waic=inla_district_households_bym_NB2$waic$waic,
                                                                                log_score=mean(-log(inla_district_households_bym_NB2$cpo$cpo),na.rm=T),
                                                                                pit_unif=mean(sort(inla_district_households_bym_NB2$cpo$pit)*
                                                                                                log(sort(inla_district_households_bym_NB2$cpo$pit)/uniquant),na.rm=T)))



#summary table of model selection
model_selection<-model_selection %>% 
  add_row(model_selection_HHdistrict2a %>% arrange(dic)) %>% 
  add_row(model_selection_HHdistrict2b %>% arrange(dic)) %>% 
  add_row(model_selection_HHdistrict3a %>% arrange(dic)) %>% 
  add_row(model_selection_HHdistrict3b %>% arrange(dic))

write.xlsx(model_selection, "./data/results/model_selection.xlsx", sheetName="INLA")  










#remove models and data not selected
rm(data_household_agg2, inla_district_households_base_NB1, inla_district_households_base_NB2,
   inla_district_households_iid_NB1,  inla_district_households_iid_NB2,
   inla_district_notified_base_NB1, inla_district_notified_base_NB2,
   inla_district_notified_iid_NB1, inla_district_notified_iid_NB2, model_selection,
   model_selection_HHdistrict2a, model_selection_HHdistrict2b, model_selection_HHdistrict3a,
   model_selection_HHdistrict3b, uniquant)

gc()


#save selected models
save(data_household,
     data_household_agg,
     #inla_household_bym_binom1,
     #inla_household_bym_binom2,
     inla_district_notified_bym_NB1,
     inla_district_notified_bym_NB2,
     inla_district_households_bym_NB1,
     inla_district_households_bym_NB2,
     file='./data/results/fitted_modelsD_selected.RData')
