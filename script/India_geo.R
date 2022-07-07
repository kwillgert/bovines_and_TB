#prepare India shape file
#Source: https://spatialdata.dhsprogram.com/boundaries/#view=table&countryId=IA

DHS_India<-read_sf('./data/sdr_subnational_boundaries_2021-07-02/shps/sdr_subnational_boundaries2.shp', stringsAsFactors = FALSE)
DHS_India <- DHS_India %>% 
  dplyr::select(state_code=OTHREGCO,state=OTHREGNA,district_code=REGCODE,district=REGNAME) %>% 
  arrange(state,district) %>% 
  filter(!is.element(state, c('Andaman and Nicobar Islands','Lakshadweep'))) #drop islands as INLA requires continous grid

#add ID to allow INLA to reference map/nb and data tables
DHS_India <- DHS_India %>% 
  mutate(ID=1:nrow(DHS_India),.before='state_code') 

DHS_India<-st_make_valid(DHS_India)
DHS_India <- DHS_India %>% mutate(area_km2=st_area(DHS_India)*1e-6,.before=geometry) %>% 
  mutate(area_km2=as.numeric(area_km2))


#save India shape file
save(DHS_India,file='./data/DHS_India_shp.RData')


#map of India including disputed territories
India_DT<-read_sf('./data/Shapefiles_India/Ind_adm0.shp', stringsAsFactors = FALSE)
