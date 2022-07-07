
#Title: Is bovine density and ownership associated with human tuberculosis in India? 
#Authors: K Willgert, S da Silva, AJK Conlan
#Date: July 2022

#main script

#load packages
source("./script/packages.R")

#load functions
source("./script/functions.R")

#set text font to New Times Roman for session
par(family="serif")

#prepare India shape file
source("./script/India_geo.R")

###1. Household level model###

#read household level data
data_household<-read.csv("./data/data_household.csv")
#household level dataset containing columns with the following data for each household: state, district, 
#TB_present, n_household_members, IAP, smokes_indoors, residence_type, bovines, health_scheme, wealth_index, 
#h_density.z, buffalo_density.z, cattle_density.z 

#assess for correlation
source("./script/cor_HH.R") 

#univariable
source("./script/household_uni.R")

#multivariable
source("./script/household_glm.R")


###Aggregated household data and district level data combined,

#read aggregated household level data
data_household<-read.csv("./data/data_household_agg.csv")
#district level dataset containing columns with the following data for each district:
#state, district, HH_tb, households, HH_pop, HH_IAP, HH_smokes_indoors, urban_HH, HH_bovines,
#HH_health_scheme, HH_low_wealth_index, HH_size, HH_IAP.z, HH_smokes_indoors.z, urban_HH.z, 
#HH_bovines.z, HH_health_scheme.z, HH_low_wealth_index.z", HH_size.z, human_population
#total_cattle, total_buffalo, total_bovines, human_TB_notified, MPI, district_areas, h_density,
#bovine_density, buffalo_density, cattle_density, MPI.z, h_density.z, bovine_density.z,     
#cattle_density.z, buffalo_density.z  

#check for correlation between predictor variables
source("./script/cor_distr.R")


#2. Model 2 - district level: officially reported TB cases 

#univariable
source("./script/HHdistrict_uni1.R")

#multivariable
source("./script/HHdistrict_glm1.R")



#3. Model 3 - district level: with self-reported cases as response variable

#univariable
source("./script/HHdistrict_uni2.R")


#multivariable
source("./script/HHdistrict_glm2.R")


##spatial autocorrelation assessment and residual error
source("./script/geo_autocorr.R")


#INLA

#prepare data sets
source("./script/inla_prep.R")

###1. Household level INLA###

source("./script/household_inla.R")

###2. District level INLA with officially reported TB cases
source("./script/HHdistrict_inla1.R")

###3. District level INLA with self-reported household TB aggregated to district level
source("./script/HHdistrict_inla2.R")

#save district level INLA outputs
source("./script/save_inlaD.R")

#visualise TB occurrence and bovines
source("./script/visualisations.R")


#model selection
source("./script/model_selection.R")

#calibration of selected models
source("./script/calibration.R") #needs to be updated for separate bovine measure models

#prepare data of fixed effects
source("./script/prep_fixed_effects.R")

#fixed effects
source("./script/fixed_effects_by_exposure.R")

#spatial random effects
source("./script/spatial_effects_by_exposure.R")

