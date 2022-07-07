#Spatial random effects

load('./data/DHS_India_shp.RData')
load('./data/India_DT.RData')

spatial_effects <- function(mod1,this_shp,level='district')
{
  Nareas<-dim(this_shp)[1]
  
  #We calculate zeta=exp(csi) where csi=upsilon + nu
  
  m <- mod1$marginals.random$ID
  
  areas_not_null = which(unlist(lapply(m,function(x){!is.null(x)})))
  
  m <- mod1$marginals.random$ID[1:Nareas]
  zeta <- lapply(m,function(x){inla.emarginal(exp,x)})
  
  #We now calculate the probability that the spatial effects zeta are above 1, 
  #identifying areas with excess risk of household TB. This is equivalent to 
  #calculating the probability that csi is above 0, which is easier to obtain
  
  if(level=='district')
  {
    a=0
    inlaprob<-lapply(mod1$marginals.random$ID[1:Nareas], function(X){
      1-inla.pmarginal(a, X)
    })
  }else{
    a=0
    inlaprob<-lapply(mod1$marginals.random$ID_S[1:Nareas], function(X){
      1-inla.pmarginal(a, X)
    })
    
  }
  
  
  return(tibble(state=this_shp$state, district=this_shp$district,RR=unlist(zeta),
                pp=unlist(inlaprob)))
}


#district level models

x1<-spatial_effects(inla_district_notified_bym_NB1,DHS_India)
x2<-spatial_effects(inla_district_notified_bym_NB2,DHS_India)
y1<-spatial_effects(inla_district_households_bym_NB1,DHS_India)
y2<-spatial_effects(inla_district_households_bym_NB2,DHS_India)


min_RR = min(c(x1$RR, x2$RR, y1$RR, y2$RR))
max_RR = max(c(x1$RR, x2$RR, y1$RR, y2$RR))

p_distr2a = India_border +
  geom_sf(data=(simp_map %>% right_join(x1)),
          aes(fill=RR), lwd=0) +
  labs(fill="Relative risk")+
  scale_fill_gradient2(midpoint=1,low='darkblue',mid='#f7f7f7',high='red',limits=c(min_RR,max_RR))+
  theme(legend.position='right') +
  theme_void()+
  theme(text=element_text(size=18, family="serif"))

p_distr2b = India_border +
  geom_sf(data=(simp_map %>% right_join(x2)),
          aes(fill=RR), lwd=0) +
  labs(fill="Relative risk")+
  scale_fill_gradient2(midpoint=1,low='darkblue',mid='#f7f7f7',high='red',limits=c(min_RR,max_RR))+
  theme(legend.position='right') +
  theme_void()+
  theme(text=element_text(size=18, family="serif"))

p_distr3a = India_border +
  geom_sf(data=(simp_map %>% left_join(y1)),aes(fill=RR), lwd=0) +
  labs(fill="Relative risk")+
  scale_fill_gradient2(midpoint=1,low='darkblue',mid='#f7f7f7',high='red',limits=c(min_RR,max_RR))+
  theme(legend.position='right')+
  theme_void()+
  theme(text=element_text(size=18, family="serif"))

p_distr3b = India_border +
  geom_sf(data=(simp_map %>% left_join(y2)),aes(fill=RR), lwd=0) +
  labs(fill="Relative risk")+
  scale_fill_gradient2(midpoint=1,low='darkblue',mid='#f7f7f7',high='red',limits=c(min_RR,max_RR))+
  theme(legend.position='right')+
  theme_void()+
  theme(text=element_text(size=18, family="serif"))


### 1a) Household level model with bovine ownership as explanatory variable

load('./data/results/inla_household_bym_binom1.RData')

z1<-spatial_effects(inla_household_bym_binom1,DHS_India)


### 1b) Household level model with bovine density as explanatory variable

load('./data/results/inla_household_bym_binom2.RData')

z2<-spatial_effects(inla_household_bym_binom2,DHS_India)

min(z1$RR, z2$RR)
max(z1$RR, z2$RR)

pHH1a = India_border +
  geom_sf(data=(simp_map %>% left_join(z1)),aes(fill=(RR)), lwd=0) +
  scale_fill_gradient2(midpoint=1,low='darkblue',mid='#f7f7f7',high='red',limits=c(min_RR,max_RR))+
  labs(fill="Relative risk")+
  theme(legend.position='right') +
  theme_void() +
  theme(text=element_text(size=18, family="serif"))

pHH1b = India_border +
  geom_sf(data=(simp_map %>% left_join(z2)),aes(fill=(RR)), lwd=0) +
  scale_fill_gradient2(midpoint=1,low='darkblue',mid='#f7f7f7',high='red',limits=c(min_RR,max_RR))+
  labs(fill="Relative risk")+
  theme(legend.position='right') +
  theme_void() +
  theme(text=element_text(size=18, family="serif"))

# extract the legend from one of the plots to assign single legend

legend_RR <- cowplot::get_legend(
  p_distr2a + 
    theme(legend.box.margin = margin(0, 0, 0, 0.5)) # create some space to the left of the legend
)

#combine RR figures into single plot

spatial_RR<-cowplot::plot_grid(
  pHH1a+
    theme(legend.position = "none", 
          plot.margin = unit(c(40, 5.8, -50, 5.5), "pt")),#remove space above and below plot
  NULL, #include empty plot to increase space between plots, size specified in rel_widths
  p_distr2a+
    theme(legend.position = "none", 
          plot.margin = unit(c(40, 5.8, -50, 5.5), "pt")),#remove space above and below plot
  NULL, #include empty plot to increase space between plots, size specified in rel_widths
  p_distr3a+
    theme(legend.position = "none", 
          plot.margin = unit(c(40, 5.8, -50, 5.5), "pt")), #remove space above and below plot
  pHH1b+
    theme(legend.position = "none",
          plot.margin = unit(c(-95, 5.8, 5.5, 5.5), "pt")),#remove space above plot
  NULL, #include empty plot to increase space between plots, size specified in rel_widths
  p_distr2b+
    theme(legend.position = "none",
          plot.margin = unit(c(-95, 5.8, 5.5, 5.5), "pt")),#remove space above plot
  NULL, #include empty plot to increase space between plots, size specified in rel_widths
  p_distr3b+
    theme(legend.position = "none",
          plot.margin = unit(c(-95, 5.8, 5.5, 5.5), "pt")),#remove space above plot
  nrow=2,
  rel_widths = c(1, 0.04, 1, 0.04, 1, 1, 0.04, 1, 0.04, 1), #specify width of each plot
  labels = c("1) Self-reported TB: household level\n\nA) bovine ownership", "", "2) Officially notified TB: district level\n\n", "", "3) Self-reported TB: district level\n\n",
             "B) bovine density", "","","",""),
  hjust=0,
  label_x=0.045, #move label on x-axis
  label_y=1, #move label on y-axis
  label_size=22,
  #labels="AUTO",
  label_fontfamily="serif")

# add the legend to the plot. Give it 0.4 of the width of one plot (via rel_widths)

spatial_RR2<-cowplot::plot_grid(spatial_RR, legend_RR, rel_widths = c(3, .4))

ggsave("./figures/spatial_RR.png", width=20, height=16)    

