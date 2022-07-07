#extract data for fixed effects

#load selected household level models
load('./data/results/inla_household_bym_binom1.RData')

#extract data for fixed effects
inla_household_bym_binom1<-as_tibble(inla_household_bym_binom1$summary.fixed,rownames='Variable')

load('./data/results/inla_household_bym_binom2.RData')

#extract data for fixed effects
inla_household_bym_binom2<-as_tibble(inla_household_bym_binom2$summary.fixed,rownames='Variable')

#load district data
load('./data/results/fitted_modelsD.RData')
