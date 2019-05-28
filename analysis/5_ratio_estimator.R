library(dplyr)
library(mgcv)

# get filtered data output from 0_filter_dat.r
# setwd("/home/brian/Documents/Bycatch/fleetwide")
dat = readRDS("wcgop data/filtered_data_byhaul.rds")
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

# loop over species
for(i in 1:length(spp)) {
  # put current species as 'response' col in dat
  dat$response <- dat[,spp[i]]
  # binary variable for presence-absence model
  dat$presence = ifelse(dat$response > 0, 1, 0)
  
  # loop over percent observed
  for(h in 1:length(pct)) {
    # results.sim = save results separately for each species x pct x sim combo
    # results = cumulative results created using rbind(results.sim) in loop
    res.colnames <- c("model","spatial","effort","sim","pct_trips","pct_hauls",
      "species","total_true","total_est",yr.labs)
    results = data.frame(matrix(ncol = length(res.colnames), nrow = 0))
    colnames(results) <- res.colnames
    
    for(j in 1:n_sims) {
      print(paste0("Sim: ", j))
      
      # collect results for this species, pct, sim 
      results.sim = data.frame(model = "ratio",
        spatial = "no",
        effort = "yes",
        sim = j,
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
      results.sim[1,col.2011:(col.2011+4)] <- byyear
      
      # prep covariates
      training_data$year = as.factor(training_data$year)
      training_data$depth_interval = as.factor(training_data$depth_interval)
      test_data$year = as.factor(test_data$year)
      test_data$depth_interval = as.factor(test_data$depth_interval)
      col.2011 <- which(colnames(results.sim)=="2011_est")
      
      # calculate the mean / variance of the training data. Quantities are:
      # - mean and var of bycatch by stratum
      # - mean and var of retained catch by stratum
      train_sum = group_by(training_data, bimonth, year, depth_interval) %>%
        summarize(n = n(), retained = sum(ret), dis = sum(response),
          mean_y = mean(response, na.rm=T),
          var_y = var(response, na.rm=T),
          mean_x = mean(ret, na.rm=T),
          var_x = var(ret, na.rm=T),
          cov_xy = 2*sum((response-mean_x)*(ret-mean_y)) / (mean_x*mean_y), 
          var = (1/length(ret))*((mean_y/mean_x)^2)*(var_y+var_x-cov_xy))
      
      # calculate groundfish retained -- for ratio estimator
      gfr = group_by(test_data, bimonth, year, depth_interval) %>%
        summarize(ntot = n(), gfr = sum(ret))
      
      all_data = left_join(train_sum,gfr)
      
      # expand observed/retained discards to test data based on groundfish retained
      #all_data$test_ratio = (all_data$dis / all_data$ret) * all_data$gfr
      
      # finite population correction factor to variance
      all_data$var = all_data$var * (1 - all_data$n/all_data$ntot)
      all_data$B = (all_data$dis/all_data$retained) * all_data$gfr
      all_data$varB = all_data$var * (all_data$gfr^2)
      
      annual_est = group_by(all_data, year) %>% 
        summarize(total_bycatch = sum(B)) %>% select(total_bycatch)
      results.sim[1, col.2011:(col.2011+4)] = annual_est$total_bycatch
      results.sim[1,"total_est"] = sum(annual_est$total_bycatch)
      # append results.sim to results
      results <- rbind(results, results.sim)
      
    } # end j
    
    # save output
    saveRDS(results, file = paste0("results/byyear_ratio_",spp[i],"_",pct[h],".rds"))
  }
  
}



