#save district level INLA outputs

save(data_household,
     data_household_agg,
     inla_district_notified_base_NB1,
     inla_district_notified_iid_NB1,
     inla_district_notified_bym_NB1,
     inla_district_notified_base_NB2,
     inla_district_notified_iid_NB2,
     inla_district_notified_bym_NB2,
     inla_district_households_base_NB1,
     inla_district_households_iid_NB1,
     inla_district_households_bym_NB1,
     inla_district_households_base_NB2,
     inla_district_households_iid_NB2,
     inla_district_households_bym_NB2,
     file='./data/results/fitted_modelsD.RData')
