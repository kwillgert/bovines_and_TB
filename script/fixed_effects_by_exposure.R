#fixed effects

#1a.  household model for self-reported TB: with bovine ownership as explanatory variable

fixed_effects_HH1a<-(inla_household_bym_binom1 %>% 
                            mutate(mean=round(mean, 2),
                            sd=round(sd, 2),
                            mean_sd=paste(mean, " (",sd,")", sep=""),
                            OR=signif(exp(mean),3),
                            lower=signif(exp(`0.025quant`),3),
                            upper=signif(exp(`0.975quant`),3),
                            CI=paste(round(lower, 2), " - ", round(upper, 2))) %>% 
                            select(Variable,mean, sd, mean_sd, OR,lower,upper, CI))


#1b.  household model for self-reported TB: with bovine density as explanatory variable

fixed_effects_HH1b<-(inla_household_bym_binom2 %>% 
                              mutate(mean=round(mean, 2),
                              sd=round(sd, 2),
                              mean_sd=paste(mean, " (",sd,")", sep=""),
                              OR=signif(exp(mean),3),
                              lower=signif(exp(`0.025quant`),3),
                              upper=signif(exp(`0.975quant`),3),
                              CI=paste(round(lower, 2), " - ", round(upper, 2))) %>% 
                              select(Variable,mean, sd, mean_sd, OR,lower,upper, CI))


#Household models combined
var_colour<-c(rep("black", 7), "#f46d43", "#f46d43", "#2c7bb6")

pHH = ggplot(inla_household_bym_binom1 %>% mutate(exposure="ownership") %>% 
           add_row(inla_household_bym_binom2 %>% mutate(exposure="density")) %>% 
           filter(Variable!='(Intercept)'),
           #reorder variables
         aes(x=factor(Variable, level=c("bovinesyes", "cattle_density.z", "buffalo_density.z",
                                              "h_density.z",
                                              "residence_typerural",
                                              "n_household_members", 
                                              "smokes_indoorsyes", 
                                              "health_schemeyes", 
                                              "wealth_indexmiddle",  
                                              "wealth_indexpoor")),
             y=exp(mean),
             ymin=exp(`0.025quant`), 
             ymax=exp(`0.975quant`),
             colour=exposure)) + 
  geom_pointrange(position=position_dodge(width=0.4)) + 
  scale_color_manual(breaks=c("ownership", "density"), #change order of bovine exposures in legend
    values=c("#2c7bb6","#f46d43"))+
  coord_flip() + 
  scale_x_discrete(limits=rev, #list variables in reverse for consistency
                   labels=c("bovinesyes"="bovines: yes",
                            "cattle_density.z"="cattle density", 
                            "buffalo_density.z"="buffalo density", 
                            "h_density.z"="human density",
                            "residence_typerural"="residence: rural",
                            "n_household_members"="household size", 
                            "smokes_indoorsyes"="smokes indoors: yes", 
                            "health_schemeyes"="health scheme: yes", 
                            "wealth_indexmiddle"="wealth index: middle", 
                            "wealth_indexpoor"="wealth index: low"))+                              
  labs(y='\nOR', 
       colour="Bovine \nexposure") + 
  geom_hline(yintercept=1.0,col='red', size=1, linetype = "dashed") + #add line at OR = 1
  theme(axis.title.y=element_blank(),
        axis.text.y=element_text(color=var_colour))+ #highlight bovine exposure measures in a separate colour
  theme(text=element_text(size=22, family="serif"),
        legend.title=element_text(size=22), 
        legend.text=element_text(size=22),
        axis.text.y=element_text(size=23)) 

#with horizontal legend lines
pHH2 = ggplot(inla_household_bym_binom1 %>% mutate(exposure="ownership") %>% 
               add_row(inla_household_bym_binom2 %>% mutate(exposure="density")) %>% 
               filter(Variable!='(Intercept)'),
             #reorder variables
             aes(y=factor(Variable, level=c("bovinesyes", "cattle_density.z", "buffalo_density.z",
                                            "h_density.z",
                                            "residence_typerural",
                                            "n_household_members", 
                                            "smokes_indoorsyes", 
                                            "health_schemeyes", 
                                            "wealth_indexmiddle",  
                                            "wealth_indexpoor")),
                 x=exp(mean),
                 colour=exposure)) + 
  ggstance::geom_pointrangeh(aes(xmin=exp(`0.025quant`), xmax=exp(`0.975quant`)), #geom_pointrangeh used to have horizontal legend symbols
                             position=position_dodge(width=0.4)) +
  scale_color_manual(breaks=c("ownership", "density"), #change order of bovine exposures in legend
                     values=c("#2c7bb6","#f46d43"))+
  scale_y_discrete(limits=rev, #list variables in reverse #for consistency
                   labels=c("bovinesyes"="bovines: yes",
                            "cattle_density.z"="cattle density", 
                            "buffalo_density.z"="buffalo density", 
                            "h_density.z"="human density",
                            "residence_typerural"="residence: rural",
                            "n_household_members"="household size", 
                            "smokes_indoorsyes"="smokes indoors: yes", 
                            "health_schemeyes"="health scheme: yes", 
                            "wealth_indexmiddle"="wealth index: middle", 
                            "wealth_indexpoor"="wealth index: low"))+                              
  labs(x='\nOR', 
       colour="Bovine \nexposure") + 
  geom_vline(xintercept=1.0,col='red', size=1, linetype = "dashed") + #add line at OR = 1
  theme(axis.title.y=element_blank())+
  theme(text=element_text(size=22, family="serif"),
        legend.title=element_text(size=22), 
        legend.text=element_text(size=22))

#2. district level with officially reported TB and combined district level and aggregated HH predictor variables

#min and max IRR across fixed effects
min_IRR = min(as_tibble(inla_district_notified_bym_NB1$summary.fixed %>% 
                             filter(rownames(inla_district_notified_bym_NB1$summary.fixed) !='(Intercept)') %>%
                             select('0.025quant')) %>% 
                bind_rows(inla_district_notified_bym_NB2$summary.fixed %>% 
                            filter(rownames(inla_district_notified_bym_NB2$summary.fixed) !='(Intercept)') %>%
                            select('0.025quant')) %>% 
                bind_rows(inla_district_households_bym_NB1$summary.fixed %>% 
                             filter(rownames(inla_district_notified_bym_NB1$summary.fixed) !='(Intercept)') %>%
                             select('0.025quant')) %>% 
                bind_rows(inla_district_households_bym_NB2$summary.fixed %>% 
                            filter(rownames(inla_district_notified_bym_NB2$summary.fixed) !='(Intercept)') %>%
                            select('0.025quant')) %>% 
                mutate(`0.025quant`=exp(`0.025quant`)), na.rm=T)
max_IRR = max(as_tibble(inla_district_notified_bym_NB1$summary.fixed %>% 
                           filter(rownames(inla_district_notified_bym_NB1$summary.fixed) !='(Intercept)') %>%
                           select('0.975quant')) %>% 
                bind_rows(inla_district_notified_bym_NB2$summary.fixed %>% 
                            filter(rownames(inla_district_notified_bym_NB2$summary.fixed) !='(Intercept)') %>%
                            select('0.975quant')) %>%
                 bind_rows(inla_district_households_bym_NB1$summary.fixed %>% 
                             filter(rownames(inla_district_notified_bym_NB1$summary.fixed) !='(Intercept)') %>%
                             select('0.975quant')) %>% 
                bind_rows(inla_district_households_bym_NB2$summary.fixed %>% 
                            filter(rownames(inla_district_notified_bym_NB2$summary.fixed) !='(Intercept)') %>%
                            select('0.975quant')) %>% 
                mutate(`0.975quant`=exp(`0.975quant`)), na.rm=T)


var_colour<-c(rep("black", 6), "#f46d43", "#f46d43", "#2c7bb6")

p_distr2 <- ggplot(as_tibble(inla_district_notified_bym_NB1$summary.fixed,rownames='Variable') %>% mutate(exposure="ownership") %>% 
               add_row(as_tibble(inla_district_notified_bym_NB2$summary.fixed,rownames='Variable') %>% mutate(exposure="density")) %>% 
               filter(Variable!='(Intercept)'),
             #reorder variables
             aes(x=factor(Variable, level=c("HH_bovines.z", "cattle_density.z", "buffalo_density.z", 
                                            "h_density.z", "urban_HH.z", "HH_size.z", "HH_smokes_indoors.z", 
                                            "HH_health_scheme.z", "HH_low_wealth_index.z")),
                 y=exp(mean),
                 ymin=exp(`0.025quant`), 
                 ymax=exp(`0.975quant`),
                 colour=exposure)) +
  geom_pointrange(position=position_dodge(width=0.4)) + 
  scale_color_manual(breaks=c("ownership", "density"), #change order of bovine exposures in legend
    values=c("#2c7bb6","#f46d43"))+
  coord_flip() + #variables on y-axis
  scale_x_discrete(limits=rev, #list variables in reverse #for consistency
                   labels=c("HH_bovines.z"="HH bovines",
                            "cattle_density.z"="cattle density", 
                            "buffalo_density.z"="buffalo density", 
                            "h_density.z"="human density", 
                            "urban_HH.z"="HH urban", 
                            "HH_size.z"="HH size", 
                            "HH_smokes_indoors.z"="HH smoking", 
                            "HH_health_scheme.z"="HH health scheme",
                            "HH_low_wealth_index.z"="HH low wealth"))+ 
  labs(y='\nIRR',
       colour="Bovine \nexposure") +
  geom_hline(yintercept=1.0,col='red',size=1, linetype = "dashed") + #add line at IRR= 1
  theme(axis.title.y=element_blank(),
        axis.text.y=element_text(color=var_colour))+ #highlight bovine exposure measures in a separate colour
  theme(text=element_text(size=22, family="serif"), 
        legend.title=element_text(size=22), 
        legend.text=element_text(size=22),
        axis.text.y=element_text(size=23, 
                                 #face="bold"
                                 ))  +
  scale_y_continuous(limits=c(min_IRR, max_IRR)) #add y-axis limits for consistency with other fixed effects plot


#3. district level with aggregated household level of self-reported TB combined with district level predictor variables

p_distr3 <- ggplot(as_tibble(inla_district_households_bym_NB1$summary.fixed,rownames='Variable') %>% mutate(exposure="ownership") %>% 
                     add_row(as_tibble(inla_district_households_bym_NB2$summary.fixed,rownames='Variable') %>% mutate(exposure="density")) %>% 
                     filter(Variable!='(Intercept)'),
                   #reorder variables
                   aes(x=factor(Variable, level=c("HH_bovines.z", "cattle_density.z", "buffalo_density.z", 
                                                  "h_density.z", "urban_HH.z", "HH_size.z", "HH_smokes_indoors.z", 
                                                  "HH_health_scheme.z", "HH_low_wealth_index.z")),
                       y=exp(mean),
                       ymin=exp(`0.025quant`), 
                       ymax=exp(`0.975quant`),
                       colour=exposure)) +
  geom_pointrange(position=position_dodge(width=0.4)) + 
  scale_color_manual(breaks=c("ownership", "density"), #change order of bovine exposures in legend
                     values=c("#2c7bb6","#f46d43"))+
  coord_flip() + #variables on y-axis
  scale_x_discrete(limits=rev, #list variables in reverse #for consistency
                   labels=c("HH_bovines.z"="HH bovines",
                            "cattle_density.z"="cattle density", 
                            "buffalo_density.z"="buffalo density", 
                            "h_density.z"="human density", 
                            "urban_HH.z"="HH urban", 
                            "HH_size.z"="HH size", 
                            "HH_smokes_indoors.z"="HH smoking", 
                            "HH_health_scheme.z"="HH health scheme",
                            "HH_low_wealth_index.z"="HH low wealth"))+ 
  labs(y='\nIRR',
       colour="Bovine \nexposure") +
  geom_hline(yintercept=1.0,col='red',size=1, linetype = "dashed") + #add line at IRR= 1
  theme(axis.title.y=element_blank(),
        axis.text.y=element_text(color=var_colour))+ #highlight bovine exposure measures in a separate colour
  theme(text=element_text(size=22, family="serif"), 
        legend.title=element_text(size=22), 
        legend.text=element_text(size=22),
        axis.text.y=element_text(size=23, #face="bold"
                                 ))  +
  scale_y_continuous(limits=c(min_IRR, max_IRR)) #add y-axis limits for consistency with other fixed effects plot

# extract the legend from one of the plots to assign single legend
legend_fixed_effects <- cowplot::get_legend(
  pHH2 + 
    theme(legend.box.margin = margin(0, 0, 0, 0.5)) # create some space to the left of the legend
)


fixed_effects_all<-cowplot::plot_grid(
  pHH+
    theme(legend.position = "none",
          plot.margin = unit(c(50, 5.5, 5.5, 5.5), "pt")),
  NULL,
  p_distr2+
    theme(legend.position = "none"),
  NULL, #include empty plot to increase space between plots, size specified in rel_widths
  p_distr3+
    theme(legend.position = "none"),
  rel_widths = c(1, 0.03, 1, 0.03, 1), #specify width of each plot
  align='hv', #set plots to the same width and height
  labels = c("1) Self-reported TB: household level", "", "2) Officially notified TB: district level", "", "   3) Self-reported TB: district level"),
  label_size=24,
  label_x=-0.41, #move label on x-axis
  label_y=1.0, #move label on y-axis
  label_fontfamily="serif",
  nrow=1)

# add the legend to the plot. Give it 0.4 of the width of one plot (via rel_widths).
fixed_effects_all<-cowplot::plot_grid(fixed_effects_all, legend_fixed_effects, rel_widths = c(3, .4))

ggsave("./figures/fixed_effect_all.png", width=20, height=7)


#fixed effects district level

fixed_effects_HHdistrict2a<-as_tibble(inla_district_notified_bym_NB1$summary.fixed,rownames='Variable') %>% 
  mutate(mean=round(mean, 2),
         sd=round(sd, 2),
         mean_sd=paste(mean, " (",sd,")", sep=""),
         IRR=signif(exp(mean),3),
         lower=signif(exp(`0.025quant`),3),
         upper=signif(exp(`0.975quant`),3),
         CI=paste(round(lower, 2), " - ", round(upper, 2))) %>% 
  select(Variable,mean, sd, mean_sd, IRR,lower,upper, CI)

fixed_effects_HHdistrict2b<-as_tibble(inla_district_notified_bym_NB2$summary.fixed,rownames='Variable') %>% 
  mutate(mean=round(mean, 2),
         sd=round(sd, 2),
         mean_sd=paste(mean, " (",sd,")", sep=""),
         IRR=signif(exp(mean),3),
         lower=signif(exp(`0.025quant`),3),
         upper=signif(exp(`0.975quant`),3),
         CI=paste(round(lower, 2), " - ", round(upper, 2))) %>% 
  select(Variable,mean, sd, mean_sd, IRR,lower,upper, CI)

fixed_effects_HHdistrict3a<-as_tibble(inla_district_households_bym_NB1$summary.fixed,rownames='Variable') %>% 
  mutate(mean=round(mean, 2),
         sd=round(sd, 2),
         mean_sd=paste(mean, " (",sd,")", sep=""),
         IRR=signif(exp(mean),3),
         lower=signif(exp(`0.025quant`),3),
         upper=signif(exp(`0.975quant`),3),
         CI=paste(round(lower, 2), " - ", round(upper, 2))) %>% 
  select(Variable,mean, sd, mean_sd, IRR,lower,upper, CI)

fixed_effects_HHdistrict3b<-as_tibble(inla_district_households_bym_NB2$summary.fixed,rownames='Variable') %>% 
  mutate(mean=round(mean, 2),
         sd=round(sd, 2),
         mean_sd=paste(mean, " (",sd,")", sep=""),
         IRR=signif(exp(mean),3),
         lower=signif(exp(`0.025quant`),3),
         upper=signif(exp(`0.975quant`),3),
         CI=paste(round(lower, 2), " - ", round(upper, 2))) %>% 
  select(Variable,mean, sd, mean_sd, IRR,lower,upper, CI)

#save fixed effects
write.xlsx(fixed_effects_HHdistrict2a, file="./data/results/fixed_effects.xlsx", sheetName="HHdistrict2a")
write.xlsx(fixed_effects_HHdistrict2b, file="./data/results/fixed_effects.xlsx", sheetName="HHdistrict2b", append=TRUE)
write.xlsx(fixed_effects_HHdistrict3a, "./data/results/fixed_effects.xlsx", sheetName="HHdistrict3a", append=TRUE) 
write.xlsx(fixed_effects_HHdistrict3b, "./data/results/fixed_effects.xlsx", sheetName="HHdistrict3b", append=TRUE) 
write.xlsx(fixed_effects_HH1a, "./data/results/fixed_effects.xlsx", sheetName="HH1a", append=TRUE) 
write.xlsx(fixed_effects_HH1b, "./data/results/fixed_effects.xlsx", sheetName="HH1b", append=TRUE) 
