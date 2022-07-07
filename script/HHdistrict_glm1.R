#district level data with officially reported TB cases and 
#aggregated households at district level


#Model 2a: using bovine ownership as measure for bovines

HHdistrict_glm1a<-glm(human_TB_notified ~  
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
                  offset=log(data_household_agg$human_population))

summary(HHdistrict_glm1a)


#create regression table
HHdistrict_glm1a_tab<-HHdistrict_glm1a %>% 
  tbl_regression(exponentiate = TRUE) # Logical indicating whether to exponentiate the coefficient estimates (display IRR)



#Model 2b: using bovine density as measure for bovines

HHdistrict_glm1b<-glm(human_TB_notified ~  
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
                      offset=log(data_household_agg$human_population))

summary(HHdistrict_glm1b)

HHdistrict_glm1b_tab<-HHdistrict_glm1b %>% 
  tbl_regression(exponentiate = TRUE) 



