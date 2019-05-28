library(tidyverse)
library(ggsidekick)

# setwd("/home/brian/Documents/Bycatch/fleetwide/")
# source("/home/brian/Documents/Bycatch/fleetwide/figures/summarize_results.R")
spp = readRDS("wcgop data/spp.rds")
# change all colnames to lower case
spp <- tolower(spp)

# load in the results
#   forgot to distinguish rfxu from rfxu4 in the 'model' column of results... do it here based on filename
files = dir("results")
for(i in 1:length(files)) {
  r = readRDS(paste0("results/",files[i]))
  m = strsplit(files[i],"_")[[1]][2]
  if(m=="rfxu4") r$model = "rfxu4"
  if(i == 1) {
    dat = r
  } else {
    dat = rbind(dat, r)
  }
}

# rename some columns because dplyr can't deal with numbers at start
names(dat)[-c(1:9)] = paste0(c(rep("est_",5), rep("true_",5)), rep(2011:2015,2))
# names(dat)[-c(1:9)] = c(paste0(c(rep("est_",5), rep("true_",5)), rep(2011:2015,2)), "opt_neighbors", "opt_committees")

# calculate summaries across iterations -- percent error statistics. 
# calculate rmse% instead of rmse, https://stats.stackexchange.com/questions/190868/rmse-is-scale-dependent-is-rmse
dat = dat %>% mutate(pe_total = (total_true - total_est)/total_true,
  e_2011 = (est_2011 - true_2011),
  e_2012 = (est_2012 - true_2012),
  e_2013 = (est_2013 - true_2013),
  e_2014 = (est_2014 - true_2014),
  e_2015 = (est_2015 - true_2015),
  pe_2011 = e_2011/true_2011,
  pe_2012 = e_2012/true_2012,
  pe_2013 = e_2013/true_2013,
  pe_2014 = e_2014/true_2014,
  pe_2015 = e_2015/true_2015,
  mean_pe = (pe_2011+pe_2012+pe_2013+pe_2014+pe_2015) / 5,
  mean_dat = (true_2011+true_2012+true_2013+true_2014+true_2015)/5,
  rmse = sqrt((e_2011^2 + e_2012^2 + e_2013^2 + e_2014^2 + e_2015^2)/5) / mean_dat) #%>% 
  # select(model, spatial, effort, sim, pct_trips, species,
  #   pe_total, pe_2011, pe_2012, pe_2013, pe_2014, pe_2015, mean_pe, rmse) 

# create some factors for grouping
dat$spatial = c("", "SP")[as.numeric(dat$spatial)]
dat$effort = c("", "EFF")[as.numeric(dat$effort)]

# rename 
dat = dplyr::mutate(dat, model = fct_recode(model, 
  "RF" = "rftotal", "RF Delta" = "rfdelta", "Ratio" = "ratio", 
  "GAM Delta" = "gamdelta", "GAM" = "gamtotal", "RF Best" = "rfbest", "VAST" = "vast", 
  "RF Cubist" = "rfcubist", "RF Xu2" = "rfxu", "RF Xu4" = "rfxu4", "RF Nonlinear" = "rfnonlinear", "GAM Nonlinear" = "gamnonlinear"))
dat$group = as.factor(paste0(dat$model, " ", dat$spatial, dat$effort, dat$pct_trips))

# rename species
dat = dplyr::mutate(dat, species = fct_recode(species, 
  "Big skate" = "big skate", "Black skate" = "black skate", 
  "Brown cat shark" = "brown cat shark", "California slickhead" = "california slickhead",
  "Dungeness crab" = "dungeness crab", "Grenadier" = "grenadier", 
  "Octopus" = "octopus unid", "Pacific hake" = "pacific hake",
  "Pacific halibut" = "pacific halibut", "Rosethorn rockfish" = "rosethorn rockfish",
  "Slender sole" = "slender sole", "Spiny dogfish shark" = "spiny dogfish shark",
  "Sandpaper skate" = "sandpaper skate", "Spotted ratfish" = "spotted ratfish",
  "Tanner crab" = "tanneri tanner crab"))


# saveRDS(dat, "figures/results_summary.rds") # original (ratio estimator didn't include depth_interval)
saveRDS(dat, "figures/results_summary_depthratio.rds") # revised (ratio estimator uses depth)
