# Reducing bias in random forest predictions
# Dec 14, 2017

setwd("/home/brian/Documents/Bycatch/fleetwide")
# source("figures/supplement/rf_bias.R")

# Notes:
#  Most trees have bias in the tails (UNDERpredict the upper tail, OVERpredict the lower tail). Why? Terminal nodes for extreme values use the MEAN of the training data in those nodes.
# http://www.math.unm.edu/~luyan/research/biasrf.pdf
#  (By)catch is almost always right-skewed, so RF will underpredict on average

# What methods exist for bias correction?
#   Cubist - 1992-1993 papers by Quinlan, was commercial, made open-source in 2011. R package "Cubist", also implemented in 'caret'
#     1. instead of using mean in terminal nodes, fit linear model
#     2. iterative "model trees", adjust by residual ("committees" in Cubist)
#     3. adjust by nearest-neighbors ("neighbors" in Cubist)
#     'caret' package has option to tune Cubist over values for 'committees' and 'neighbors'
#   Cubist does really well - better than all other models in Kuhn2013 Figs 9.1, 9.2
#   https://cran.r-project.org/web/packages/Cubist/index.html
#   https://www.dropbox.com/s/2vf3swfbk48lfdc/RulesRulesRules.pdf?dl=0

# p.182 Kuhn2013
# conditional inference trees, Hothorn et al. (2006).
# unbiased tree-based models for regression, classification, and other scenarios.

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

# Implement Cubist (Kuhn2013 p240)
# another cubist + caret example: https://www.r-bloggers.com/ensemble-learning-with-cubist-model/

# fit Cubist model to dungeness crab
#   i=5 		species 5
#   h=1 		only 20% observed
#   j=1:200 	all simulations
#   			only Effort + Space model
library(caret)
library(Cubist)
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

i=5 # dungeness crab
# # loop over species
# for(i in 1:length(spp)) {
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
      print(paste0("Sim: ", j))

      # collect results for this species, pct, sim 
      results.sim = data.frame(model = "rfcubist",
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
		controlObject <- trainControl(method = "cv", number=5)
		cubistGrid <- expand.grid(.committees = c(5, 10, 50), .neighbors = c(0, 3, 5, 7, 9))
		# b.time <- Sys.time()
		# cbModel <- train(modFormula,
		# 	data = training_data,
		# 	method = "cubist",
		# 	.committees=50,
		# 	.neighbors=5,
		# 	control = cubistControl(unbiased = TRUE),
		# 	trControl = controlObject)
		# e.time <- Sys.time()
		cbModel <- train(modFormula,
			data = training_data,
			method = "cubist",
			tuneGrid = cubistGrid,
			trControl = controlObject,
      control = cubistControl(unbiased = TRUE))
    # e.time <- Sys.time()
		test_data$rf.pred  <- predict(cbModel, test_data)
		# e2.time <- Sys.time()

# # print(cbModel)
# pdf(paste0("figures/supplement/fig10_rfcubist_dcrab_tune",j,".pdf"), width=7, height=5)
# print(ggplot(cbModel) + theme_sleek())
# dev.off()


        results.sim$total_est[1] = sum(test_data$rf.pred) # calculate total (across years) estimated bycatch, unobserved trips
        results.sim[1,col.2011:(col.2011+4)] <- group_by(test_data, year) %>% summarize(byyear=sum(rf.pred)) %>% pull(byyear)
        # results.sim$opt_committees <- cbModel$bestTune$committees
        # results.sim$opt_neighbors <- cbModel$bestTune$neighbors

		# mdl1 <- cubist(x = X1, y = Y1, control = cubistControl(unbiased = TRUE))
		# test <- train(x = X1, y = Y1, "cubist", tuneGrid = expand.grid(.committees = seq(10, 100, 10), .neighbors = 0), trControl = trainControl(method = 'cv'))
		# print(test)
		# mdl2 <- cubist(x = X1, y = Y1, committees = 100, control = cubistControl(unbiased = TRUE,  label = "log_medv", seed = 2015))

      # append results.sim to results
      # print(results.sim %>% select(sim, opt_committees, opt_neighbors))
      results <- rbind(results, results.sim)
      
    } # end j
    
    # save output
    saveRDS(results, file = paste0("results/byyear_rfcubist_",spp[i],"_",pct[h],".rds"))

# --------------------------------------------------------------------------------
# plot RMSE for Dungeness Crab, 20% observed, Space + Effort, Ratio/RF/Cubist
source("/home/brian/Documents/Bycatch/fleetwide/figures/summarize_results.R")
dat = readRDS("figures/results_summary.rds")

# facet by species. model options are including space, effort, both, neither (color)
dat$Covariate = paste(dat$spatial, dat$effort)
dat$Covariate[which(dat$Covariate==" ")] = "Neither"
dat$Covariate[which(dat$Covariate=="SP EFF")] = "Space, Effort"
dat$Covariate[which(dat$Covariate=="SP ")] = "Space"
dat$Covariate[which(dat$Covariate==" EFF")] = "Effort"

dat = filter(dat, pct_trips==0.2) %>% filter(species=="Dungeness crab")

dat.spatial.eff <- dat %>% group_by(species) %>% mutate(medRatio = NA) %>% 
                    filter(model %in% c("RF", "RF Cubist", "RF Xu2", "GAM")) %>% filter(Covariate=="Space, Effort")
dat.ratio <- dat %>% filter(model=="Ratio") %>% group_by(species) %>% mutate(medRatio = median(rmse))
dat.plot <- rbind(dat.spatial.eff, dat.ratio)
dat.plot$model <- factor(dat.plot$model, levels = c("Ratio", "RF", "RF Cubist", "RF Xu2", "GAM"))
levels(dat.plot$model) <- c("Ratio", "RF", "RF Cubist", "RF Xu", "GAM")
# dat.spatial.eff <- dat %>% group_by(species) %>% mutate(medRatio = NA) %>% 
#                     filter(model %in% c("RF", "RF Cubist", "RF Xu2", "RF Xu4")) %>% filter(Covariate=="Space, Effort")
# dat.ratio <- dat %>% filter(model=="Ratio") %>% group_by(species) %>% mutate(medRatio = median(rmse))
# dat.plot <- rbind(dat.spatial.eff, dat.ratio)
# dat.plot$model <- factor(dat.plot$model, levels = c("Ratio", "RF", "RF Cubist", "RF Xu2", "RF Xu4"))
# levels(dat.plot$model) <- c("Ratio", "RF Total", "RF Cubist", "RF Xu2", "RF Xu4")

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
  # scale_fill_manual(name="Model", values=c("grey","#56B4E9",rgb(t(col2rgb("#56B4E9")/1.2), maxColorValue=255),
  #                                                           rgb(t(col2rgb("#56B4E9")/1.4), maxColorValue=255),
  #                                                           rgb(t(col2rgb("#56B4E9")/1.6), maxColorValue=255))))
dev.off()

pdf("figures/supplement/fig8b_rfcubist_pe_average_GAM.pdf", width=7, height=7)
print(dat.plot %>% #group_by(species, model) %>%
  ggplot(aes(x=model, y=pe, fill = model)) + 
    geom_violin(draw_quantiles = c(0.5)) +
    geom_hline(aes(group = species), yintercept = 0, linetype = 2) +
    facet_wrap(~ species) + 
    theme_sleek() + 
    xlab("Model") + 
    ylab("Percent Error") + 
    coord_cartesian(ylim = c(-0.5,0.5)) +
  scale_fill_manual(name="Model", values=c("grey","#56B4E9",rgb(t(col2rgb("#56B4E9")/1.4), maxColorValue=255),
                                                            rgb(t(col2rgb("#56B4E9")/2), maxColorValue=255),
                                                            "#E69F00")))
dev.off()

dat.plot %>% group_by(model) %>% summarize(median.pe=median(pe), median.rmse=median(rmse), mean.pe=mean(pe), mean.rmse=mean(rmse))
#   model     median.pe median.rmse mean.pe mean.rmse
# 1 Ratio      -0.00156       0.227  0.0215     0.242
# 2 RF          0.0548        0.145  0.0622     0.156
# 3 RF Cubist   0.0428        0.157  0.0611     0.173
# 4 RF Xu       1.11          1.15   1.12       1.14 
# 5 GAM         0.0392        0.180  0.0764     0.240


# ---------------------------------------------------
# Plot RMSE averaged across years
filter_dat = dat %>% group_by(model) %>% mutate(rmse = ifelse(rmse > 10, NA, rmse))
dat.spatial.eff <- filter_dat %>% group_by(species) %>% mutate(medRatio = NA) %>% 
                    filter(model %in% c("RF", "RF Cubist", "GAM")) %>% filter(Covariate=="Space, Effort")
dat.ratio <- filter_dat %>% filter(model=="Ratio") %>% group_by(species) %>% mutate(medRatio = median(rmse))
dat.plot <- rbind(dat.spatial.eff, dat.ratio)
dat.plot$model <- factor(dat.plot$model, levels = c("Ratio", "RF", "RF Cubist", "GAM"))
levels(dat.plot$model) <- c("Ratio", "RF Total", "RF Cubist", "GAM")

pdf("figures/supplement/fig9_rfcubist_rmse_GAM.pdf", width=7, height=7)
print(dat.plot %>% #group_by(species, model) %>%
  ggplot(aes(model, rmse, fill = model)) + 
  	geom_violin(draw_quantiles = c(0.5)) +
  	geom_hline(aes(yintercept = medRatio, group = species), linetype = 2) +
  	# facet_wrap(~ species) + 
  	theme_sleek() + 
  	xlab("Model") + 
  	ylab("Normalized RMSE") + 
  	theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  	coord_cartesian(ylim = c(0,0.75)) +
	scale_fill_manual(name="Model", values=c("grey","#56B4E9",rgb(t(col2rgb("#56B4E9")/1.4), maxColorValue=255), "#E69F00")))
dev.off()

dat.plot %>% group_by(model) %>% summarize(median.rmse=median(rmse))
#       model median.rmse
# 1     Ratio   0.2274851
# 2  RF Total   0.1447881
# 3 RF Cubist   0.1564327

# --------------------------------------------------
# Explore RF bias directionality
# https://stats.stackexchange.com/questions/28732/response-distribution-dependent-bias-in-random-forest-regression
library(randomForest)
n = 1000; 
# x1 = rnorm(n, mean = 0, sd = 1)
# x1 = runif(n, 0, 1)
x1 = rexp(n, rate = 1)
response = x1
predictors = data.frame(x1=x1) 
rf = randomForest(x=predictors, y=response)
error = response-predict(rf, predictors)
plot(x1, error)

library(glmmfields)
library(tidyverse)
library(ggsidekick)
set.seed(123)
s = sim_glmmfields(n_knots = 40, n_draws = 1, gp_theta = 0.5, gp_sigma = 0.2,
  mvt=FALSE, n_data_points = 200, sd_obs = 1, obs_error = "gamma")
dat = s$dat
dat$set = sample(c("test", "train"), size = nrow(dat), replace = TRUE)
ggplot(s$dat, aes(y)) + geom_histogram(col="dodgerblue4", fill
 ="dodgerblue4") + theme_sleek() + ylab("Count")

traindat <- filter(dat, set=="train")
rf = randomForest(y ~ lon + lat + I(lon^2) + I(lat^2), data = traindat)
plot(traindat$y, rf$predicted - traindat$y)

y_test = filter(dat, set=="test") %>% select(y)
y_test = y_test$y
test_predict = predict(rf, filter(dat, set=="test"))
test_bias = test_predict - y_test
m = matrix(0, 2, 2)
colnames(m) = c("Estimator", "Bias")
m[,1] = c("Mean bias (test set)", "Median bias (test set)")
m[,2] = c(round(mean(test_bias), 3), round(median(test_bias), 3))
kable(m)

