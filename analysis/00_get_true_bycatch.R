# Get true fleet-wide bycatch by year by species
#   saves as RDS for use in .Rmd
# Brian Stock
# Dec 12 2017
library(dplyr)
setwd("/home/brian/Documents/Bycatch/fleetwide")
dat = readRDS("wcgop data/filtered_data_byhaul.rds")
spp = readRDS("wcgop data/spp.rds")
# change all colnames to lower case
names(dat) = tolower(names(dat))
spp <- tolower(spp)

# get test data indices (simulate observing only 20% or 40% of trips)
trips = unique(as.numeric(as.factor(dat$trip_id)))
sampled_trips_20 = readRDS("sampled_trips_20.rds")
# sampled_trips_40 = readRDS("sampled_trips_40.rds")
n.sims = 200
years <- 2011:2015
n.yrs = length(years)

res.colnames <- c("species","year","sim","true_unobs","true_obs","true_total")
results = data.frame(matrix(ncol = length(res.colnames), nrow = 0))
colnames(results) <- res.colnames
# loop over species
for(i in 1:length(spp)) {
	dat$response <- dat[,spp[i]]
    for(j in 1:n.sims) {
      # collect results for this species, pct, sim 
      results.sim = data.frame(species = rep(spp[i], n.yrs),
                          year = years,
                          sim = rep(j, n.yrs),
                          true_unobs = NA, 
                          true_obs = NA, 
                          true_full = NA)
      # tmp <- data.frame(matrix(NA, ncol = length(yr.labs), nrow = 4))
      # colnames(tmp) <- yr.labs
      # results.sim <- cbind(results.sim, tmp)

      s = sampled_trips_20[!is.na(sampled_trips_20[,j]),j]
      # if(h == 2) s = sampled_trips_40[!is.na(sampled_trips_40[,j]),j]
      training_data = dat[which(dat$trip %in% s),]
      test_data = dat[-which(dat$trip %in% s),]
      results.sim$true_unobs <- group_by(test_data, year) %>% summarize(byyear=sum(response)) %>% pull(byyear)
      results.sim$true_obs <- group_by(training_data, year) %>% summarize(byyear=sum(response)) %>% pull(byyear)
      results.sim$true_full <- group_by(dat, year) %>% summarize(byyear=sum(response)) %>% pull(byyear)

      results <- rbind(results, results.sim)
    }
}

saveRDS(results, "wcgop data/true_bycatch_byyear.rds")
