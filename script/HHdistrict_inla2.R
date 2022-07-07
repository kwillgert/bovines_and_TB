#INLA district level with aggregated household level of self-reported TB combined with district level predictor variables

#3a. using bovine ownership as measure for bovines:

inla_district_households_base_NB1 <- inla(HH_tb ~ HH_bovines.z +
                                            h_density.z + 
                                            urban_HH.z +
                                            HH_size.z +
                                            HH_smokes_indoors.z + 
                                            HH_health_scheme.z +
                                            HH_low_wealth_index.z, 
                                          family="nbinomial", 
                                          data=data_household_agg, 
                                          offset=log(households),
                                          control.predictor = list(compute = TRUE,link=1),
                                          control.compute=list(waic=TRUE,cpo=TRUE,dic=TRUE,config = TRUE))

inla_district_households_iid_NB1 <- inla(HH_tb ~ HH_bovines.z +
                                           h_density.z + 
                                           urban_HH.z +
                                           HH_size.z +
                                           HH_smokes_indoors.z + 
                                           HH_health_scheme.z +
                                           HH_low_wealth_index.z + f(ID, model="iid", graph="./data/india_graph"), 
                                         family="nbinomial", 
                                         data=data_household_agg, 
                                         offset=log(households),
                                         control.predictor = list(compute = TRUE,link=1),
                                         control.compute=list(waic=TRUE,cpo=TRUE,dic=TRUE,config = TRUE))

inla_district_households_bym_NB1 <- inla(HH_tb ~ HH_bovines.z +
                                           h_density.z + 
                                           urban_HH.z +
                                           HH_size.z +
                                           HH_smokes_indoors.z + 
                                           HH_health_scheme.z +
                                           HH_low_wealth_index.z + f(ID, model="bym", graph="./data/india_graph"), 
                                         family="nbinomial", 
                                         data=data_household_agg, 
                                         offset=log(households),
                                         control.predictor = list(compute = TRUE,link=1),
                                         control.compute=list(waic=TRUE,cpo=TRUE,dic=TRUE,config = TRUE))


#3b. using bovine density as measure for bovines:

inla_district_households_base_NB2 <- inla(HH_tb ~ cattle_density.z + 
                                            buffalo_density.z + 
                                            h_density.z + 
                                            urban_HH.z +
                                            HH_size.z +
                                            HH_smokes_indoors.z + 
                                            HH_health_scheme.z +
                                            HH_low_wealth_index.z, 
                                          family="nbinomial", 
                                          data=data_household_agg, 
                                          offset=log(households),
                                          control.predictor = list(compute = TRUE,link=1),
                                          control.compute=list(waic=TRUE,cpo=TRUE,dic=TRUE,config = TRUE))

inla_district_households_iid_NB2 <- inla(HH_tb ~ cattle_density.z + 
                                           buffalo_density.z + 
                                           h_density.z + 
                                           urban_HH.z +
                                           HH_size.z +
                                           HH_smokes_indoors.z + 
                                           HH_health_scheme.z +
                                           HH_low_wealth_index.z + f(ID, model="iid", graph="./data/india_graph"), 
                                         family="nbinomial", 
                                         data=data_household_agg, 
                                         offset=log(households),
                                         control.predictor = list(compute = TRUE,link=1),
                                         control.compute=list(waic=TRUE,cpo=TRUE,dic=TRUE,config = TRUE))

inla_district_households_bym_NB2 <- inla(HH_tb ~ cattle_density.z + 
                                           buffalo_density.z + 
                                           h_density.z + 
                                           urban_HH.z +
                                           HH_size.z +
                                           HH_smokes_indoors.z + 
                                           HH_health_scheme.z +
                                           HH_low_wealth_index.z + f(ID, model="bym", graph="./data/india_graph"), 
                                         family="nbinomial", 
                                         data=data_household_agg, 
                                         offset=log(households),
                                         control.predictor = list(compute = TRUE,link=1),
                                         control.compute=list(waic=TRUE,cpo=TRUE,dic=TRUE,config = TRUE))

