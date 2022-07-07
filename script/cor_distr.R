#check for correlation between response variables

ggpairs(data_household_agg %>% select(human_TB_notified,HH_tb))


#check for correlation between predictor variables
cor_dist<-ggcorr_edit((data_household_agg %>% 
                               select(HH_bovines.z, cattle_density.z, buffalo_density.z, h_density.z,  
                                      urban_HH.z, HH_size.z, HH_IAP.z, HH_smokes_indoors.z, 
                                      HH_health_scheme.z, HH_low_wealth_index.z, MPI.z) %>%
                        dplyr::rename("human_density"=h_density.z, "MPI"=MPI.z, "cattle_density"=cattle_density.z , "buffalo_density"=buffalo_density.z,  
                                      "HH_urban"=urban_HH.z, "HH_size"=HH_size.z, "HH_IAP"=HH_IAP.z, "HH_smoking"=HH_smokes_indoors.z, 
                                      "HH_bovines"=HH_bovines.z, "HH_health_scheme"=HH_health_scheme.z, "HH_low_wealth"=HH_low_wealth_index.z)) %>% 
       drop_na(),
       low="#542788", mid="#f7f7f7", high="#b35806", #specify colours for low, mid and high values 
       midpoint=0.0, #specify midpoint of scale
       size = 5, #text size
       hjust = 0.75,
       nudge_x=-0.1,
       nudge_y=0.1,
       angle = -35,
       label_size = 4,
       legend.size=15,
       layout.exp=2, #expand horizontal axis to avoid variable names being cut,
       family="serif",
       name= "Correlation \ncoefficient\n")+
        theme(text = element_text(size = 20, family="serif"),
        legend.title = element_text(size = 20)#,
        #plot.margin = unit(c(2,1,1,1), "cm")
        )

ggsave("./figures/cor_dist.png", width=10, height=8)

#plot fixed effects of both models
cor_plots<-cowplot::plot_grid(
        cor_HH,
        NULL, #include empty plot to increase space between plots, size specified in rel_widths
        cor_dist,
        rel_widths = c(1, 0.04, 1), #specify width of each plot
        labels = c("A) Household level", "", "B) District level"),
        label_size=22,
        label_x=-0.085, #move label on x-axis
        label_y=0.91, #move label on y-axis
        #labels="AUTO",
        label_fontfamily="serif",
        nrow=1)

cor_plots

ggsave("./figures/cor_plots.png", width=16, height=8)
