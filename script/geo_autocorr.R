#spatial autocorrelation assessment

load('./data/DHS_India_shp.RData')

#Model 2: district level data with officially reported TB cases

#2a) bovine ownership

resid<-data_household_agg %>% 
  drop_na() %>% 
  select(state, district) %>% 
  mutate(residuals=residuals(HHdistrict_glm1a))

DHS_India<-DHS_India %>% 
  left_join(resid)

DHS_India<-na.omit(DHS_India)

#create neighbour list
nb <- poly2nb(DHS_India)
summary(nb)

resnb1 <- sapply(nb, function(x) mean(HHdistrict_glm1a$residuals[x]))
plot(x=DHS_India$residuals,
     y=resnb1, 
     xlab='Residuals', 
     ylab='Mean adjacent residuals')

#spatial weights for neighbour lists
lw <- nb2listw(nb,zero.policy=TRUE)

#assess spatial autocorrelation
moran.mc(DHS_India$residuals, lw, 999,zero.policy=TRUE)


#2b) bovine density

load('./data/DHS_India_shp.RData')

resid<-data_household_agg %>% 
  drop_na() %>% 
  select(state, district) %>% 
  mutate(residuals=residuals(HHdistrict_glm1b))

DHS_India<-DHS_India %>% 
  left_join(resid)

DHS_India<-na.omit(DHS_India)

#create neighbour list
nb <- poly2nb(DHS_India)
summary(nb)

resnb1 <- sapply(nb, function(x) mean(HHdistrict_glm1b$residuals[x]))
plot(x=DHS_India$residuals,
     y=resnb1, 
     xlab='Residuals', 
     ylab='Mean adjacent residuals')

#spatial weights for neighbour lists
lw <- nb2listw(nb,zero.policy=TRUE)

#assess spatial autocorrelation
moran.mc(DHS_India$residuals, lw, 999,zero.policy=TRUE)



#model 3: self-reported TB in households at district level

#3a) bovine ownership

load('./data/DHS_India_shp.RData')

resid<-data_household_agg %>% 
  select(-human_TB_notified) %>% 
  drop_na(buffalo_density) %>% 
  select(state, district) %>% 
  mutate(residuals=residuals(HHdistrict_glm2a))

DHS_India<-DHS_India %>% 
  left_join(resid)

DHS_India<-na.omit(DHS_India)

#create neighbour list
nb <- poly2nb(DHS_India)
summary(nb)

resnb2 <- sapply(nb, function(x) mean(HHdistrict_glm2a$residuals[x]))
plot(x=DHS_India$residuals,
     y=resnb2, 
     xlab='Residuals', 
     ylab='Mean adjacent residuals')

#spatial weights for neighbour lists
lw <- nb2listw(nb,zero.policy=TRUE)

#assess spatial autocorrelation
moran.mc(DHS_India$residuals, lw, 999,zero.policy=TRUE)


#3b) bovine density

load('./data/DHS_India_shp.RData')

resid<-data_household_agg %>% 
  select(-human_TB_notified) %>% 
  drop_na(buffalo_density) %>% 
  select(state, district) %>% 
  mutate(residuals=residuals(HHdistrict_glm2b))

DHS_India<-DHS_India %>% 
  left_join(resid)

DHS_India<-na.omit(DHS_India)

#create neighbour list
nb <- poly2nb(DHS_India)
summary(nb)

resnb2 <- sapply(nb, function(x) mean(HHdistrict_glm2b$residuals[x]))
plot(x=DHS_India$residuals,
     y=resnb2, 
     xlab='Residuals', 
     ylab='Mean adjacent residuals')

#spatial weights for neighbour lists
lw <- nb2listw(nb,zero.policy=TRUE)

#assess spatial autocorrelation
moran.mc(DHS_India$residuals, lw, 999,zero.policy=TRUE)

