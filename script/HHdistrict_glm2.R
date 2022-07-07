#district level data with officially reported TB cases and 
#aggregated households at district level


#Model 3a: using bovine ownership as measure for bovines

HHdistrict_glm2a<-glm(HH_tb ~  
                      HH_bovines.z +
                      h_density.z + 
                      urban_HH.z +
                      HH_size.z +
                      HH_smokes_indoors.z + 
                      HH_health_scheme.z +
                      HH_low_wealth_index.z +
                      state, 
                  data=data_household_agg, 
                  family = 'poisson',
                  offset=log(data_household_agg$households))

summary(HHdistrict_glm2a)

#create regression table
HHdistrict_glm2a %>% 
  tbl_regression(exponentiate = TRUE) # Logical indicating whether to exponentiate the coefficient estimates (display IRR)


#Model 3b: using bovine density as measure for bovines

HHdistrict_glm2b<-glm(HH_tb ~  
                       cattle_density.z + 
                       buffalo_density.z + 
                       h_density.z + 
                       urban_HH.z +
                       HH_size.z +
                       HH_smokes_indoors.z + 
                       HH_health_scheme.z +
                       HH_low_wealth_index.z +
                       state, 
                     data=data_household_agg, 
                     family = 'poisson',
                     offset=log(data_household_agg$households))

summary(HHdistrict_glm2b)

#create regression table
HHdistrict_glm2b %>% 
  tbl_regression(exponentiate = TRUE) # Logical indicating whether to exponentiate the coefficient estimates (display IRR)









