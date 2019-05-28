library(dplyr)
library(mgcv)
# library(randomForest)
# library(e1071)
# library(LatticeKrig)
library(sp)
# library(PBSmapping)
# library(randomForestCI)

# get filtered data output from 0_filter_dat.r
# setwd("/home/brian/Documents/Bycatch/fleetwide")
dat = readRDS("wcgop data/filtered_data_byhaul.rds")
spp = readRDS("wcgop data/spp.rds")
# change all colnames to lower case
names(dat) = tolower(names(dat))
spp <- tolower(spp)

# read in likelihood profile of tweedie par
sp.prof = readRDS("profile/prof.rds")
tweedie_p = 0
for(i in 1:length(spp)) {
  tweedie_p[i] = ifelse(is.null(sp.prof[[i]]$p.max),1.5,sp.prof[[i]]$p.max)
}

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
      results.sim = data.frame(model = rep("gamtotal",4),
                          spatial = c("yes","yes","no","no"),
                          effort = c("yes","no","yes","no"),
                          sim = rep(j,4),
                          pct_trips = pct[h], # percent observed trips (how WCGOP calculates observed %)
                          pct_hauls = NA, # percent observed hauls
                          species = spp[i],
                          total_true = NA, # true total (across all years) catch of species i in unobserved trips
                          total_est = NA) # estimated total (across all years) catch of species i in unobserved trips
      tmp <- data.frame(matrix(NA, ncol = length(yr.labs), nrow = 4))
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
      for(ii in 1:4) results.sim[ii,col.2011:(col.2011+4)] <- byyear

      # prep covariates
      training_data$year = as.factor(training_data$year)
      training_data$depth_interval = as.factor(training_data$depth_interval)
      training_data$bimonth = as.factor(training_data$bimonth)
      test_data$year = as.factor(test_data$year)
      test_data$bimonth = as.factor(test_data$bimonth)
      test_data$depth_interval = as.factor(test_data$depth_interval)
      col.2011 <- which(colnames(results.sim)=="2011_est")

      # gam methods
      # https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/smooth.terms.html
      # https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/te.html
      # default k = 5^d = 25 for 2d (k = basis dimension)
      # check if k needs to be larger with gam.check
      # https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/choose.k.html
      # te vs. s
      # Tensor product smooths are especially useful for representing functions of covariates measured in different units, although they are typically not quite as nicely behaved as t.p.r.s. smooths for well scaled covariates. 
      # discrete=TRUE only in 'bam' documentation, not 'gam'...

      # spp = 1, 2, 5, 13, 8, 11
 
      # te(avg_lat,avg_long)
      # runtime 2 secs
      # est/true 71.594, 5482.812, 1.091, 1.702, 0.956, 1.388

      # te(avg_lat,avg_long,k=50) slow, not finished at 15 min
      # te(avg_lat,avg_long,k=35) slow, not finished at 5 min

      # s(avg_lat,avg_long)
      # runtime 10 secs
      # est/true = 1.094, 2.524, 1.177, 1.671, 0.959, 1.341

      # s(avg_lat,avg_long,k=50)
      # runtime 12-15 secs
      # est/true = 1.183, 4.741, 1.141, 1.648, 0.951, 1.200

      # s(avg_lat,avg_long,k=100)
      # runtime 20-35 secs
      # est/true = 1.091, 9.97, 1.174, 1.628, 0.940, 1.164

      # s(avg_lat,avg_long,k=300)
      # runtime 1.0-1.5 mins
      # est/true = 1.117, 7.698, 1.146, 1.459, 0.910, 1.174

      # bam vs. gam
      # bam took longer and did not converge - use gam instead

      # don't calculate or save se.fit - just use var(mean) across sims

      # spatial = yes, effort = yes
      # btime <- Sys.time()
      mod = gam(response ~ s(avg_lat, avg_long, k=50) + bimonth + year + depth_interval + logret, 
              data=training_data, family=Tweedie(p=tweedie_p[i]), discrete=TRUE, method="REML")
      test_data$gam.pred <- predict(mod, newdata = test_data, type="response") # , se.fit = TRUE
      results.sim$total_est[1] = sum(test_data$gam.pred) # calculate total (across years) estimated bycatch, unobserved trips
      results.sim[1,col.2011:(col.2011+4)] <- group_by(test_data, year) %>% summarize(byyear=sum(gam.pred)) %>% pull(byyear)
      # etime <- Sys.time()
      # runtime <- etime - btime
      # runtime
      # results.sim$total_est[1]/results.sim$total_true[1]
      
      # spatial = yes, effort = no
      mod = gam(response ~ s(avg_lat, avg_long, k=50) + bimonth + year + depth_interval, 
              data=training_data, family=Tweedie(p=tweedie_p[i]), discrete=TRUE, method="REML")
      test_data$gam.pred <- predict(mod, newdata = test_data, type="response") # , se.fit = TRUE
      results.sim$total_est[2] = sum(test_data$gam.pred) # calculate total (across years) estimated bycatch, unobserved trips
      results.sim[2,col.2011:(col.2011+4)] <- group_by(test_data, year) %>% summarize(byyear=sum(gam.pred)) %>% pull(byyear)
      
      # spatial = no, effort = yes
      mod = gam(response ~ bimonth + year + depth_interval + logret, 
              data=training_data, family=Tweedie(p=tweedie_p[i]), discrete=TRUE, method="REML")
      test_data$gam.pred <- predict(mod, newdata = test_data, type="response") # , se.fit = TRUE
      results.sim$total_est[3] = sum(test_data$gam.pred) # calculate total (across years) estimated bycatch, unobserved trips
      results.sim[3,col.2011:(col.2011+4)] <- group_by(test_data, year) %>% summarize(byyear=sum(gam.pred)) %>% pull(byyear)
      
      # spatial = no, effort = no
      mod = gam(response ~ bimonth + year + depth_interval, 
              data=training_data, family=Tweedie(p=tweedie_p[i]), discrete=TRUE, method="REML")
      test_data$gam.pred <- predict(mod, newdata = test_data, type="response") # , se.fit = TRUE
      results.sim$total_est[4] = sum(test_data$gam.pred) # calculate total (across years) estimated bycatch, unobserved trips
      results.sim[4,col.2011:(col.2011+4)] <- group_by(test_data, year) %>% summarize(byyear=sum(gam.pred)) %>% pull(byyear)
      
      results <- rbind(results, results.sim)
    } # end j
    
    # save output
    saveRDS(results, file = paste0("results/byyear_gamtotal_",spp[i],"_",pct[h],".rds"))
  }
  
}




