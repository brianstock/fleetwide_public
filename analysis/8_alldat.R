# Brian Stock
# Sept 3 2018
# Fit ratio estimator, RF, and GAM models to full observer dataset
# Produce model summaries + covariate relationships

# source("/home/brian/Documents/Bycatch/fleetwide/analysis/8_alldat.R")

library(dplyr)
library(mgcv)
library(randomForest)
# library(doParallel)

# get filtered data output from 0_filter_dat.r
setwd("/home/brian/Documents/Bycatch/fleetwide")
dat = readRDS("wcgop data/filtered_data_byhaul.rds")
spp = readRDS("wcgop data/spp.rds")
# change all colnames to lower case
names(dat) = tolower(names(dat))
spp <- tolower(spp)

# prep covariates
dat$year = as.factor(dat$year)
dat$depth_interval = factor(dat$depth_interval,levels(dat$depth_interval)[c(2,3,1)])

# get Tweedie p parameters for each species
sp.prof = readRDS("profile/prof.rds")
tweedie_p = 0
for(i in 1:length(spp)) {
  tweedie_p[i] = ifelse(is.null(sp.prof[[i]]$p.max),1.5,sp.prof[[i]]$p.max)
}

# loop over species
# registerDoParallel(cores=4)
# foreach(i=2:length(spp), .packages=c("randomForest","dplyr","mgcv")) %dopar% {
for(i in 1:length(spp)){
	# put current species as 'response' col in dat
	dat$response <- dat[,spp[i]]
	print(paste0("Species: ", i))

	mod.rf = randomForest(response ~ avg_lat + avg_long + I(avg_lat^2) + I(avg_long^2) + year + depth_interval + season + bimonth + I(bimonth^2) + logret, 
		data=dat, ntree=500, do.trace=100, keep.inbag=TRUE, keep.forest=TRUE) # 15 min
	# mod.gam = gam(response ~ s(avg_lat, avg_long, k=50) + season + bimonth + I(bimonth^2) + year + depth_interval + logret, 
	#     data=dat, family=Tweedie(p=tweedie_p[i]), discrete=TRUE, method="REML") # 1 min

	# mod.ratio = dat %>% group_by(year, bimonth, depth_interval) %>% # we didn't stratify by season
	# 	summarize(n = n(), retained = sum(ret), dis = sum(response),
	# 		mean_y = mean(response, na.rm=T),
	# 		var_y = var(response, na.rm=T),
	# 		mean_x = mean(ret, na.rm=T),
	# 		var_x = var(ret, na.rm=T),
	# 		cov_xy = 2*sum((response-mean_x)*(ret-mean_y)) / (mean_x*mean_y), 
	# 		var = (1/length(ret))*((mean_y/mean_x)^2)*(var_y+var_x-cov_xy))

	# save output
	saveRDS(mod.rf, file = paste0("revision/results/alldat_",spp[i],"_RF.rds"))
	# saveRDS(mod.gam, file = paste0("revision/results/alldat_",spp[i],"_GAM.rds"))
	# saveRDS(mod.ratio, file = paste0("revision/results/alldat_",spp[i],"_ratio.rds"))
}

# re-run with factor(Bimonth) and no Season
dat$bimonth <- factor(dat$bimonth)
for(i in 1:length(spp)){
	# put current species as 'response' col in dat
	dat$response <- dat[,spp[i]]
	print(paste0("Species: ", i))

	mod.rf = randomForest(response ~ avg_lat + avg_long + I(avg_lat^2) + I(avg_long^2) + year + depth_interval + bimonth + logret, 
		data=dat, ntree=500, do.trace=100, keep.inbag=TRUE, keep.forest=TRUE) # 15 min
	mod.rf.norep = randomForest(response ~ avg_lat + avg_long + year + depth_interval + bimonth + logret, 
		data=dat, ntree=500, do.trace=100, keep.inbag=TRUE, keep.forest=TRUE) # 15 min
	# mod.gam = gam(response ~ s(avg_lat, avg_long, k=50) + bimonth + year + depth_interval + logret, 
	#     data=dat, family=Tweedie(p=tweedie_p[i]), discrete=TRUE, method="REML") # 1 min

	# save output
	saveRDS(mod.rf, file = paste0("revision/results/alldat_",spp[i],"_RF_facbimonth.rds"))
	saveRDS(mod.rf.norep, file = paste0("revision/results/alldat_",spp[i],"_RF_norep.rds"))
	# saveRDS(mod.gam, file = paste0("revision/results/alldat_",spp[i],"_GAM_facbimonth.rds"))
}

# # re-run with full non-linear flexibility
# dat = readRDS("wcgop data/filtered_data_byhaul_v2.rds")
# names(dat) = tolower(names(dat))
# dat$year <- as.numeric(as.character(dat$year))
# for(i in 1:length(spp)){
# 	dat$response <- dat[,spp[i]]
# 	print(paste0("Species: ", i))

# 	mod.rf = randomForest(response ~ avg_lat + avg_long + year + depth + julian_day + time + gear + logret, 
# 		data=dat, ntree=500, do.trace=100, keep.inbag=TRUE, keep.forest=TRUE) # 15 min
# 	mod.gam = gam(response ~ s(avg_lat, avg_long, k=50) + s(year,k=5) + s(depth,k=5) + s(julian_day,k=5) + s(time,k=5) + gear + logret, 
# 	    data=dat, family=Tweedie(p=tweedie_p[i]), discrete=TRUE, method="REML") # 1 min

# 	# save output
# 	saveRDS(mod.rf, file = paste0("revision/results/alldat_",spp[i],"_RF_nofac.rds"))
# 	saveRDS(mod.gam, file = paste0("revision/results/alldat_",spp[i],"_GAM_nofac.rds"))
# }

# # loop over species
# for(i in 2:length(spp)) {
# 	# put current species as 'response' col in dat
# 	dat$response <- dat[,spp[i]]
# 	print(paste0("Species: ", i))

# 	mod.rf = randomForest(response ~ avg_lat + avg_long + I(avg_lat^2) + I(avg_long^2) + year + depth_interval + season + bimonth + bimonth2 + logret, 
# 		data=dat, ntree=1000, keep.inbag=TRUE, keep.forest=TRUE, do.trace=250) # 15 min
# 	mod.gam = gam(response ~ s(avg_lat, avg_long, k=50) + season + bimonth + bimonth2 + year + depth_interval + logret, 
# 	    data=dat, family=Tweedie(p=tweedie_p[i]), discrete=TRUE, method="REML") # 1 min

# 	mod.ratio = dat %>% group_by(year, bimonth, depth_interval) %>% # we didn't stratify by season
# 		summarize(n = n(), retained = sum(ret), dis = sum(response),
# 			mean_y = mean(response, na.rm=T),
# 			var_y = var(response, na.rm=T),
# 			mean_x = mean(ret, na.rm=T),
# 			var_x = var(ret, na.rm=T),
# 			cov_xy = 2*sum((response-mean_x)*(ret-mean_y)) / (mean_x*mean_y), 
# 			var = (1/length(ret))*((mean_y/mean_x)^2)*(var_y+var_x-cov_xy))

# 	# save output
# 	saveRDS(mod.rf, file = paste0("revision/results/alldat_",spp[i],"_RF.rds"))
# 	saveRDS(mod.gam, file = paste0("revision/results/alldat_",spp[i],"_GAM.rds"))
# 	saveRDS(mod.ratio, file = paste0("revision/results/alldat_",spp[i],"_ratio.rds"))
# }

