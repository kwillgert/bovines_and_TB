#preparation for INLA

load('./data/DHS_India_shp.RData')

# Create neighbour list
nb <- poly2nb(DHS_India, row.names="district")
summary(nb)

#Convert adjacency graph to format for INLA and save to disk
nb2INLA("./data/india_graph", nb)

# Join ID to data tables

data_household <- data_household %>% 
  left_join(st_drop_geometry(DHS_India %>% dplyr::select(ID,state,district))) %>% 
  relocate(ID,.before=state)

data_household_agg <- data_household_agg %>% 
  left_join(st_drop_geometry(DHS_India %>% dplyr::select(ID,state,district))) %>% 
  relocate(ID,.before=state)


