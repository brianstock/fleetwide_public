# Reducing bias in random forest predictions
# Dec 14, 2017

setwd("/home/brian/Documents/Bycatch/fleetwide")
# source("figures/supplement/rf_bias_xu.R")

# Notes:
#  Most trees have bias in the tails (UNDERpredict the upper tail, OVERpredict the lower tail). Why? Terminal nodes for extreme values use the MEAN of the training data in those nodes.
#  (By)catch is almost always right-skewed, so RF will underpredict on average

# What methods exist for bias correction?
#  1. Cubist - 1992-1993 papers by Quinlan, was commercial, made open-source in 2011. R package "Cubist", also implemented in 'caret'
#     1. instead of using mean in terminal nodes, fit linear model
#     2. iterative "model trees", adjust by residual ("committees" in Cubist)
#     3. adjust by nearest-neighbors ("neighbors" in Cubist)
#     'caret' package has option to tune Cubist over values for 'committees' and 'neighbors'
#   Cubist does really well - better than all other models in Kuhn2013 Figs 9.1, 9.2
#   https://cran.r-project.org/web/packages/Cubist/index.html
#   https://www.dropbox.com/s/2vf3swfbk48lfdc/RulesRulesRules.pdf?dl=0

#  2. Xu (2013)
#     fit a second RF using the OOB prediction error as response (or more iterations, but 1 best?)

# Refs:
#  #  Kuhn2013 chapters 5 (bias-variance tradeoff), 8 (Cubist), 9 (comparison Cubist vs. others, Cubist best)
#  https://stats.stackexchange.com/questions/20416/how-should-decision-tree-splits-be-implemented-when-predicting-continuous-variab/20521#comment37572_20521
#  https://redcalx.livejournal.com/140843.html
#  Quinlan, J. (1992). Learning with continuous classes. Proceedings of the 5th Australian Joint Conference On Artificial Intelligence, 343–348.
#  Quinlan, J. (1993). Combining instance-based and model-based learning. Proceedings of the Tenth International Conference on Machine Learning, 236–243.
#  http://www.tandfonline.com/doi/abs/10.1080/02664763.2011.578621
#  http://www.sciencedirect.com/science/article/pii/S122631921500006X
#  https://arxiv.org/pdf/1506.00553.pdf
#  https://andrewsforest.oregonstate.edu/sites/default/files/lter/pubs/pdf/pub4531.pdf

# Implement Xu (2013) pages 40-43
rf_xu = function(formula, test_data, train_data, iter=2) {
  n_test = nrow(test_data)
  n_train = nrow(train_data)
  
  train_oob_pred = matrix(NA, nrow=n_train, ncol=iter+1)
  train_oob_pred[,1] <- train_data$y_k <- train_data$response
  test_pred_k = test_error = as.data.frame(matrix(NA, nrow=n_test, ncol=iter))
  colnames(test_pred) <- paste0("b_",1:iter)
  colnames(test_error) <- paste0("b_",1:iter)
  for(k in 1:iter) {
    rf = randomForest(formula, data = train_data, ntree=1000)
    train_data$y_k <- train_oob_pred[,k+1] <- as.vector(rf$predicted) - train_oob_pred[,k]
    test_pred_k[,k] <- as.vector(predict(rf, newdata=test_data))
    test_error[,k] <- apply(as.matrix(test_pred_k[,1:k]),1,sum) - test_data$response
  }
  test_pred <- apply(as.matrix(test_pred_k[,1:iter]),1,sum)
  return(list(test_pred=test_pred, test_pred_k=test_pred_k, test_error=test_error))
}

library(randomForest)
library(tidyverse)
library(ggsidekick)
setwd("/home/brian/Documents/Bycatch/fleetwide")
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

# i=5 # dungeness crab
# # loop over species
for(i in 1:length(spp)) {
  # put current species as 'response' col in dat
  dat$response <- dat[,spp[i]]

h=1 # only 20% observed
  # # loop over percent observed
  # for(h in 1:length(pct)) {
    # results.sim = save results separately for each species x pct x sim combo
    # results = cumulative results created using rbind(results.sim) in loop
    res.colnames <- c("model","spatial","effort","sim","pct_trips","pct_hauls",
      "species","total_true","total_est",yr.labs)
    results = data.frame(matrix(ncol = length(res.colnames), nrow = 0))
    colnames(results) <- res.colnames

    for(j in 1:n_sims) {
      print(paste0("Species: ",i," Sim: ", j))

      # collect results for this species, pct, sim 
      results.sim = data.frame(model = "rfxu",
                          spatial = "yes",
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
      # results.sim$opt_committees <- results.sim$opt_neighbors <- NA

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
		  modFormula <- paste("response ~ avg_lat + avg_long + I(avg_lat^2) + I(avg_long^2) + year + depth_interval + season + bimonth + bimonth2 + logret")
		  modFormula <- as.formula(modFormula)
      rf <- rf_xu(modFormula, test_data, train_data, iter=2)
		  test_data$rf.pred  <- rf$test_pred

      results.sim$total_est[1] = sum(test_data$rf.pred) # calculate total (across years) estimated bycatch, unobserved trips
      results.sim[1,col.2011:(col.2011+4)] <- group_by(test_data, year) %>% summarize(byyear=sum(rf.pred)) %>% pull(byyear)

      # append results.sim to results
      results <- rbind(results, results.sim)
      
    } # end j
    
    # save output
    saveRDS(results, file = paste0("results/byyear_rfxu_",spp[i],"_",pct[h],".rds"))
    saveRDS(results, file = paste0("/home/brian/Dropbox/bycatch/fleetwide/byyear_rfxu_",spp[i],"_",pct[h],".rds"))
} # end i

# # --------------------------------------------------------------------------------
# # plot RMSE for Dungeness Crab, 20% observed, Space + Effort, Ratio/RF/Cubist
# source("/home/brian/Documents/Bycatch/fleetwide/figures/summarize_results.R")
dat = readRDS("figures/results_summary.rds")
library(dplyr)

# facet by species. model options are including space, effort, both, neither (color)
dat$Covariate = paste(dat$spatial, dat$effort)
dat$Covariate[which(dat$Covariate==" ")] = "Neither"
dat$Covariate[which(dat$Covariate=="SP EFF")] = "Space, Effort"
dat$Covariate[which(dat$Covariate=="SP ")] = "Space"
dat$Covariate[which(dat$Covariate==" EFF")] = "Effort"

dat = filter(dat, pct_trips==0.2) %>% filter(species=="Dungeness crab")

dat.spatial.eff <- dat %>% group_by(species) %>% mutate(medRatio = NA) %>% 
                    filter(model %in% c("RF", "RF Cubist")) %>% filter(Covariate=="Space, Effort")
dat.ratio <- dat %>% filter(model=="Ratio") %>% group_by(species) %>% mutate(medRatio = median(rmse))
dat.plot <- rbind(dat.spatial.eff, dat.ratio)
dat.plot$model <- factor(dat.plot$model, levels = c("Ratio", "RF", "RF Cubist"))
levels(dat.plot$model) <- c("Ratio", "RF Total", "RF Cubist")
# dat.plot$species <- factor(dat.plot$species, levels = c(sp.labs,"AVERAGE"))

# gather annual estimates
dat.plot <- as.data.frame(tidyr::gather(dat.plot, year, pe, c("pe_2011","pe_2012","pe_2013","pe_2014","pe_2015")))
dat.plot$year <- gsub("pe_", "", dat.plot$year)
dat.plot$year <- as.factor(dat.plot$year)

pdf("figures/supplement/fig8_rfcubist_pe.pdf", width=7, height=5)
print(dat.plot %>% #group_by(species, model) %>%
  ggplot(aes(x=year, y=pe, fill = model)) + 
    geom_violin(draw_quantiles = c(0.5)) +
    geom_hline(aes(group = species), yintercept = 0, linetype = 2) +
    facet_wrap(~ species) + 
    theme_sleek() + 
    xlab("Year") + 
    ylab("Percent Error") + 
    coord_cartesian(ylim = c(-0.5,0.5)) +
  scale_fill_manual(name="Model", values=c("grey","#56B4E9",rgb(t(col2rgb("#56B4E9")/1.4), maxColorValue=255))))
dev.off()

pdf("figures/supplement/fig8b_rfcubist_pe_average.pdf", width=7, height=7)
print(dat.plot %>% #group_by(species, model) %>%
  ggplot(aes(x=model, y=pe, fill = model)) + 
    geom_violin(draw_quantiles = c(0.5)) +
    geom_hline(aes(group = species), yintercept = 0, linetype = 2) +
    facet_wrap(~ species) + 
    theme_sleek() + 
    xlab("Year") + 
    ylab("Percent Error") + 
    coord_cartesian(ylim = c(-0.5,0.5)) +
  scale_fill_manual(name="Model", values=c("grey","#56B4E9",rgb(t(col2rgb("#56B4E9")/1.4), maxColorValue=255))))
dev.off()

dat.plot %>% group_by(model) %>% summarize(median.pe=median(pe), median.rmse=median(rmse), mean.pe=mean(pe), mean.rmse=mean(rmse))
#       model    median.pe median.rmse     mean.pe mean.rmse
# 1     Ratio  0.001560456   0.2274851  0.02153090 0.2420192
# 2  RF Total  0.054821925   0.1447881  0.06217114 0.1563674
# 3 RF Cubist  0.043035744   0.1564327  0.06129922 0.1722137

# # ---------------------------------------------------
# # Plot RMSE averaged across years
# filter_dat = dat %>% group_by(model) %>% mutate(rmse = ifelse(rmse > 10, NA, rmse))
# dat.spatial.eff <- filter_dat %>% group_by(species) %>% mutate(medRatio = NA) %>% 
#                     filter(model %in% c("RF", "RF Cubist")) %>% filter(Covariate=="Space, Effort")
# dat.ratio <- filter_dat %>% filter(model=="Ratio") %>% group_by(species) %>% mutate(medRatio = median(rmse))
# dat.plot <- rbind(dat.spatial.eff, dat.ratio)
# dat.plot$model <- factor(dat.plot$model, levels = c("Ratio", "RF", "RF Cubist"))
# levels(dat.plot$model) <- c("Ratio", "RF Total", "RF Cubist")

# pdf("figures/supplement/fig9_rfcubist_rmse.pdf", width=7, height=7)
# print(dat.plot %>% #group_by(species, model) %>%
#   ggplot(aes(model, rmse, fill = model)) + 
#   	geom_violin(draw_quantiles = c(0.5)) +
#   	geom_hline(aes(yintercept = medRatio, group = species), linetype = 2) +
#   	# facet_wrap(~ species) + 
#   	theme_sleek() + 
#   	xlab("Model") + 
#   	ylab("Normalized RMSE") + 
#   	theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
#   	coord_cartesian(ylim = c(0,0.75)) +
# 	scale_fill_manual(name="Model", values=c("grey","#56B4E9",rgb(t(col2rgb("#56B4E9")/1.4), maxColorValue=255))))
# dev.off()

# dat.plot %>% group_by(model) %>% summarize(median.rmse=median(rmse))
# #       model median.rmse
# # 1     Ratio   0.2274851
# # 2  RF Total   0.1447881
# # 3 RF Cubist   0.1564327
