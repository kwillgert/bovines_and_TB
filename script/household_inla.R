# Household models for self reported TB status

#1a) using bovine ownership as measure for bovines

inla_household_base_binom1 <- inla(TB_present ~ bovines + 
                                           h_density.z+
                                           residence_type +
                                           n_household_members + 
                                           smokes_indoors + 
                                           health_scheme + 
                                           wealth_index, 
                                   data=data_household,
                                   family='binomial', 
                                   control.compute=list(waic=TRUE,dic=TRUE, cpo=TRUE, config = TRUE))

inla_household_iid_binom1 <- inla(TB_present ~ bovines + 
                                          h_density.z+
                                          residence_type +
                                          n_household_members + 
                                          smokes_indoors + 
                                          health_scheme + 
                                          wealth_index + f(ID, model="iid", graph="./data/india_graph"), #spatial variable instead of state
                                  data=data_household,
                                  family='binomial',
                                  control.compute=list(waic=TRUE,dic=TRUE,cpo=TRUE, config = TRUE))

inla_household_bym_binom1 <- inla(TB_present ~ bovines + 
                                          h_density.z + 
                                          residence_type + 
                                          n_household_members + 
                                          smokes_indoors + 
                                          health_scheme + 
                                          wealth_index + f(ID, model="bym", graph="./data/india_graph"),
                                  data=data_household,
                                  family='binomial',
                                  num.threads=1, #limit number of threads used
                                  control.compute=list(waic=TRUE,dic=TRUE,cpo=TRUE, config = TRUE))

save(data_household,
     inla_household_base_binom1,
     inla_household_iid_binom1,
     inla_household_bym_binom1,
     file='./data/results/fitted_modelsH_bovine_ownership.RData')


#1b) using bovine density as measure for bovines

inla_household_base_binom2 <- inla(TB_present ~ cattle_density.z +
                                           buffalo_density.z +
                                           h_density.z +
                                           residence_type +
                                           n_household_members + 
                                           smokes_indoors + 
                                           health_scheme + 
                                           wealth_index, 
                                   data=data_household,
                                   family='binomial', 
                                   control.compute=list(waic=TRUE,dic=TRUE, cpo=TRUE, config = TRUE))

inla_household_iid_binom2 <- inla(TB_present ~ cattle_density.z +
                                          buffalo_density.z +
                                          h_density.z +
                                          residence_type +
                                          n_household_members + 
                                          smokes_indoors + 
                                          health_scheme + 
                                          wealth_index + f(ID, model="iid", graph="./data/india_graph"), #spatial variable instead of state
                                  data=data_household,
                                  family='binomial',
                                  control.compute=list(waic=TRUE,dic=TRUE,cpo=TRUE, config = TRUE))

inla_household_bym_binom2 <- inla(TB_present ~ cattle_density.z +
                                          buffalo_density.z +
                                          h_density.z + 
                                          residence_type + 
                                          n_household_members + 
                                          smokes_indoors + 
                                          health_scheme + 
                                          wealth_index + f(ID, model="bym", graph="./data/india_graph"),
                                  data=data_household,
                                  family='binomial',
                                  num.threads=1, #limit number of threads used
                                  control.compute=list(waic=TRUE,dic=TRUE,cpo=TRUE, config = TRUE))


save(data_household,
     inla_household_base_binom2,
     inla_household_iid_binom2,
     inla_household_bym_binom2,
     file='./data/results/fitted_modelsH_bovine_density.RData')

