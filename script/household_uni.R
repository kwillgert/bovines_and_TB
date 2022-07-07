#univariable analysis of household data at household level

#Univariable assessment using self-reported TB in households (HH_tb) as response variables. 

#Binomial regression model

HH_uni<-data_household %>% select(TB_present, 
                                  h_density.z,
                                  cattle_density.z,
                                  buffalo_density.z,
                                  residence_type, 
                                  n_household_members,
                                  IAP, #Removed due to high correlation in district level model for consistency
                                  smokes_indoors, 
                                  bovines,
                                  health_scheme,
                                  wealth_index,
                                  state) %>%
  tbl_uvregression(method = glm,
                   y = TB_present,
                   method.args = list(family = 'binomial'),
                   exponentiate = TRUE)    # Logical indicating whether to exponentiate the coefficient estimates (display OR)
