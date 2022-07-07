##univariable analysis of district level data and 
#aggregated households at district level with self-reported TB

#Model 3: self-reported TB cases aggregated at district level

data_household_agg %>% select(HH_tb,
                              h_density.z,
                              MPI.z,
                              cattle_density.z,
                              buffalo_density.z,
                              urban_HH.z,
                              HH_size.z,
                              HH_IAP,
                              HH_smokes_indoors.z,
                              HH_bovines.z,
                              HH_health_scheme.z,
                              HH_low_wealth_index.z,
                              state) %>% 

  tbl_uvregression(method = glm,
                   y = HH_tb,
                   method.args = list(family = 'poisson',offset=log(data_household_agg$households)),
                   exponentiate = TRUE)

