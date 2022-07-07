#glm analysis of household data at household level

#Model 1a: using bovine ownership as measure for bovines

household_glm1a <- glm(TB_present ~  
                         bovines + 
                         h_density.z+
                         residence_type +
                         n_household_members + 
                         smokes_indoors + 
                         health_scheme + 
                         wealth_index + 
                         state, 
                       data=data_household,family='binomial')

summary(household_glm1a)

#create regression model table
HH_glm1a<-household_glm1a %>% 
  tbl_regression(exponentiate = TRUE) # Logical indicating whether to exponentiate the coefficient estimates (display OR)


#Model 1b: using bovine density as measure for bovines

household_glm1b <- glm(TB_present ~  
                         cattle_density.z +
                         buffalo_density.z +
                         h_density.z+
                         residence_type +
                         n_household_members + 
                         smokes_indoors + 
                         health_scheme + 
                         wealth_index + 
                         state, 
                       data=data_household,family='binomial')

summary(household_glm1a)

#create regression model table
HH_glm1b<-household_glm1b %>% 
  tbl_regression(exponentiate = TRUE) # Logical indicating whether to exponentiate the coefficient estimates (display OR)

