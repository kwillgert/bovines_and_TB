load('./data/results/fitted_modelsD.RData')
load('./data/DHS_India_shp.RData')

#map of India including disputed territories 
India_DT<-st_make_valid(India_DT)
India_DT<-st_as_sf(gSimplify(as(India_DT, "Spatial"), 0.01, topologyPreserve=FALSE))

#extract country border
India_border<-India_DT %>% 
  summarise()

India_border<-ggplot()+
  geom_sf(data=India_border,
        fill="grey",
        colour="black",
        size=0.2
        )+
  theme_void()+
  theme(text=element_text(size=18, family="serif"))

#save India border file including disputed territories
save(India_border,file='./data/India_DT.RData')

simp_map<-st_as_sf(gSimplify(as(DHS_India, "Spatial"),0.01,topologyPreserve=FALSE))

#create sf object
simp_map<-st_as_sf(gSimplify(as(DHS_India, "Spatial"),0.01,topologyPreserve=FALSE))
simp_map<-simp_map %>% mutate(state=DHS_India$state,district=DHS_India$district) 

p1=India_border+
  geom_sf(data=(simp_map %>% 
            left_join(data_household_agg) %>% 
            mutate(HouseholdsTB=1000*HH_tb/households)),aes(fill=HouseholdsTB), lwd=0) + 
  labs(fill="Self-reported TB per \n1000 households\n")+
  scale_fill_viridis(option = "plasma", direction=-1, na.value="grey") + 
  theme_void()+
  theme(text=element_text(size=18, family="serif"))

#plot officially notified TB cases: cases per 1000 human population

p2=India_border+
  geom_sf(data=(simp_map %>% 
            left_join(data_household_agg) %>% 
            mutate(Notifications=1000*human_TB_notified/human_population)),aes(fill=Notifications), lwd=0) + 
  labs(fill="TB notifications per \n1000 population\n")+
  scale_fill_viridis(option = "plasma", direction=-1, na.value="grey") + 
  theme_void() +
  theme(text=element_text(size=18, family="serif"))

#visualise bovine density and ownership

#create scaled variable for proportion of households with bovines
data_household_agg<-data_household_agg %>% 
  mutate(HHprop_bovines=as.numeric(HH_bovines/households)) %>% 
  mutate(HHprop_bovines.z=(HHprop_bovines-mean(HHprop_bovines, na.rm=TRUE))/(2*sd(HHprop_bovines, na.rm=TRUE)))

min_z = min(c(as.numeric(data_household_agg$cattle_density.z),as.numeric(data_household_agg$buffalo_density.z),
              as.numeric(data_household_agg$HHprop_bovines.z)),na.rm=T)
max_z = max(c(as.numeric(data_household_agg$cattle_density.z),as.numeric(data_household_agg$buffalo_density.z),
              as.numeric(data_household_agg$HHprop_bovines.z)),na.rm=T)

p1a=India_border + 
  geom_sf(data=(simp_map %>% left_join(data_household_agg) %>% mutate(z=as.numeric(cattle_density.z))),aes(fill=z),lwd=0) + 
  scale_fill_viridis(option = "plasma", direction=-1, na.value="grey", limits=c(min_z,max_z)) +
  theme(legend.position="right") + 
  theme_void() + 
  labs(fill='Scaled variable')+
  theme(text=element_text(size=18, family="serif"))

p2a=India_border + 
  geom_sf(data=(simp_map %>% left_join(data_household_agg) %>% mutate(z=as.numeric(buffalo_density.z))), aes(fill=z), lwd=0) + 
  scale_fill_viridis(option = "plasma", direction=-1, na.value="grey", limits=c(min_z,max_z)) +
  theme(legend.position="right") + 
  theme_void() + 
  labs(fill='Scaled variable')+
  theme(text=element_text(size=18, family="serif"))

p3a=India_border + 
  geom_sf(data=(simp_map %>% left_join(data_household_agg) %>% mutate(z=as.numeric(HHprop_bovines.z))),aes(fill=z), lwd=0) + 
  scale_fill_viridis(option = "plasma", direction=-1, na.value="grey", limits=c(min_z,max_z)) +
  theme(legend.position="right") + 
  theme_void() + 
  labs(fill='Scaled variable')+
  theme(text=element_text(size=18, family="serif"))

# extract the legend from one of the plots to assign single legend
legend_bovines <- cowplot::get_legend(
  p1a + 
    theme(legend.box.margin = margin(0, 0, 0, 0.5)) # create some space to the left of the legend
)

bovines_distr<-cowplot::plot_grid(
  p1a+
    theme(legend.position = "none"),
  NULL, #include empty plot to increase space between plots
  p2a+
    theme(legend.position = "none"),
  NULL, #include empty plot to increase space between plots
  p3a+
    theme(legend.position = "none"), 
  rel_widths = c(1, 0.04, 1, 0.04, 1), #specify width of each plot
  labels = c("C) Cattle density", "", "D) Buffalo density", "", "E) Bovine ownership"),
  label_x=-0.15, #move label on x-axis
  label_y=0.87, #move label on y-axis
  label_size=22,
  label_fontfamily="serif",
  nrow=1)

# add the legend to the plot
bovines_distr2<-cowplot::plot_grid(bovines_distr, legend_bovines, rel_widths = c(3, .4))

#combine with TB notifications
cowplot::plot_grid(
  tb_report,
  bovines_distr2,
  nrow=2
)

ggsave("./figures/TB_and_bovines.png", width=20, height=20)    
