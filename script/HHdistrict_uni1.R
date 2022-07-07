##univariable analysis of district level data with officially reported TB cases and 
#aggregated households at district level

#Model 2. officially reported TB cases at district level
data_household_agg %>% select(human_TB_notified,
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
                   y = human_TB_notified,
                   method.args = list(family = 'poisson',offset=log(data_household_agg$human_population)),
                   exponentiate = TRUE)

