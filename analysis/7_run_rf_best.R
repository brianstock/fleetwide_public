library(dplyr)
library(mgcv)
library(randomForest)
library(sp)
library(PBSmapping)

# get filtered data output from 0_filter_dat.r
# setwd("/home/brian/Documents/Bycatch/fleetwide")
# source("/home/brian/Documents/Bycatch/fleetwide/analysis/7_run_rf_best.R")
dat = readRDS("wcgop data/filtered_data_byhaul_v2.rds")
spp = readRDS("wcgop data/spp.rds")
# change all colnames to lower case
names(dat) = tolower(names(dat))
spp <- tolower(spp)

# get test data indices (simulate observing only 20% or 40% of trips)
trips = unique(as.numeric(as.factor(dat$trip_id)))
sampled_trips_20 = readRDS("sampled_trips_20.rds")
sampled_trips_40 = readRDS("sampled_trips_40.rds")
n_sims = 200
pct=c(0.2,0.4)

# create colnames for years: 2011_est, 2012_est, ..., 2011_true, 2012_true, ...
yrs <- names(table(dat$year))
yr.labs <- as.vector(outer(yrs, c("est","true"), FUN = "paste", sep="_"))

# loop over percent observed
for(h in 2:length(pct)) {

  # loop over species
  for(i in 15:length(spp)) {
  # put current species as 'response' col in dat
  dat$response <- dat[,spp[i]]

  # results.sim = save results separately for each species x pct x sim combo
  # results = cumulative results created using rbind(results.sim) in loop
  res.colnames <- c("model","spatial","effort","sim","pct_trips","pct_hauls",
    "species","total_true","total_est",yr.labs)
  results = data.frame(matrix(ncol = length(res.colnames), nrow = 0))
  colnames(results) <- res.colnames

    for(j in 1:n_sims) {
      print(paste0("Species: ",i,", Pct: ",h,", Sim: ", j))

      # collect results for this species, pct, sim 
      results.sim = data.frame(model = rep("rfbest",1),
                          spatial = c("yes"),
                          effort = c("yes"),
                          sim = rep(j,1),
                          pct_trips = pct[h], # percent observed trips (how WCGOP calculates observed %)
                          pct_hauls = NA, # percent observed hauls
                          species = spp[i],
                          total_true = NA, # true total (across all years) catch of species i in unobserved trips
                          total_est = NA) # estimated total (across all years) catch of species i in unobserved trips
      tmp <- data.frame(matrix(NA, ncol = length(yr.labs), nrow = 1))
      colnames(tmp) <- yr.labs
      results.sim <- cbind(results.sim, tmp)

      # sample training / test data
      # n_trips = max(dat$trip)
      # s = sample(seq_len(n_trips), size=round(pct[h]*n_trips,0), replace=FALSE)
      if(h == 1) s = sampled_trips_20[!is.na(sampled_trips_20[,j]),j]
      if(h == 2) s = sampled_trips_40[!is.na(sampled_trips_40[,j]),j]
      training_data = dat[which(dat$trip %in% s),]
      test_data = dat[-which(dat$trip %in% s),]
      
      # results.sim$sum_all[j] = sum(dat$response)
      results.sim$total_true = sum(test_data$response)
      results.sim$pct_hauls = dim(training_data)[1]/dim(dat)[1]

      # calculate true bycatch in test_data by year
      col.2011 <- which(colnames(results.sim)=="2011_true")
      byyear <- group_by(test_data, year) %>% summarize(byyear=sum(response)) %>% pull(byyear)
      for(ii in 1:1) results.sim[ii,col.2011:(col.2011+4)] <- byyear

      # prep covariates
      training_data$year = as.factor(training_data$year)
      training_data$depth_interval = as.factor(training_data$depth_interval)
      test_data$year = as.factor(test_data$year)
      test_data$depth_interval = as.factor(test_data$depth_interval)
      col.2011 <- which(colnames(results.sim)=="2011_est")

      # spatial = yes, effort = yes
      rf = randomForest(response ~ avg_lat + avg_long + I(avg_lat^2) + I(avg_long^2) + year + depth + haul_num + gear + cosday + sinday + costime + sintime + logret, data = training_data, ntree=1000)
      test_data$rf.pred <- predict(rf, newdata = test_data)
      results.sim$total_est[1] = sum(test_data$rf.pred) # calculate total (across years) estimated bycatch, unobserved trips
      results.sim[1,col.2011:(col.2011+4)] <- group_by(test_data, year) %>% summarize(byyear=sum(rf.pred)) %>% pull(byyear)
    
      # append results.sim to results
      results <- rbind(results, results.sim)

# code for calculating Var(pred) from randomForestCI
# rf.pos <- randomForest(x=dat.pos[,covar],y=dat.pos[,sp.pos],mtry=3,ntree=1000,importance=TRUE,do.trace=250,keep.forest=TRUE, keep.inbag=TRUE)
# pred.pos.var <- randomForestInfJack(rf.pos, newdata=predict.grid[,covar])
# pred.pos.var$var.hat
      
    } # end j (sim)
    
    # save output
    saveRDS(results, file = paste0("results/byyear_rfbest_",spp[i],"_",pct[h],".rds"))
  } # end i (species)
  
} # end h (pct)




