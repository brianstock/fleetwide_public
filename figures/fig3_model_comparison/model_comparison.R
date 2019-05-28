library(dplyr)
library(ggplot2)
library(ggsidekick)

setwd("/home/brian/Documents/Bycatch/fleetwide")

# created by /figures/summarize_results.R
# dat = readRDS("figures/results_summary.rds")  # old version, ratio estimator without depth_interval
dat = readRDS("figures/results_summary_depthratio.rds") 

# for now we'll only plot the simulations with 20% coverage
dat = filter(dat, pct_trips==0.2)

# facet by species. model options are including space, effort, both, neither (color)
dat$Covariate = paste(dat$spatial, dat$effort)
dat$Covariate[which(dat$Covariate==" ")] = "Neither"
dat$Covariate[which(dat$Covariate=="SP EFF")] = "Space, Effort"
dat$Covariate[which(dat$Covariate=="SP ")] = "Space"
dat$Covariate[which(dat$Covariate==" EFF")] = "Effort"
sp.labs <- levels(dat$species)

# for purposes of visualizing violins, turn observations outside 99% CIs to NAs
filter_dat = group_by(dat, species, model, Covariate) %>% 
  # mutate(rmse = ifelse(rmse > 100, NA, rmse)) %>% 
  mutate(rmse = ifelse(rmse > 10, NA, rmse)) %>% 
  ungroup() %>% 
  group_by(species) %>% 
  mutate(max_rmse = max(rmse,na.rm=T))

pdf("figures/fig3_model_comparison/model_comparison_all.pdf",height=7,width=12)

filter(filter_dat, species %in% c("California slickhead", "Dungeness crab") == FALSE) %>%
group_by(species, model, Covariate) %>%
  ggplot(aes(model, rmse, fill = Covariate, group = factor(paste0(model, Covariate)))) + 
  	# geom_boxplot(outlier.shape = NA) + 
  	geom_violin() +
  	facet_wrap(~ species, scale="free_y") + 
  	theme_sleek() + 
  	xlab("Model") + 
  	ylab("RMSE") + 
  	theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  	coord_cartesian(ylim = c(0,1.5))
dev.off()

# --------------------------------------------------------------
# 1. only plot SPACE + EFFORT for each model (best for each model class, easier to visually compare models)
# 2. move Ratio to left
# 3. color by model class (Ratio/GAM/RF/VAST, Ratio = black to distinguish from spatial models)
# 4. shade/transparency by delta/not-delta
# 5. remove normalized RMSE > 10 and zoom y axis to 1
pdf("figures/fig3_model_comparison/fig3_model_comparison_0.2_space_effort.pdf", width=8, height=7)

filter_dat = group_by(dat, species, model, Covariate) %>% 
  mutate(rmse = ifelse(rmse > 10, NA, rmse)) %>% 
  ungroup() %>% 
  group_by(species) %>% 
  mutate(max_rmse = max(rmse,na.rm=T))
filter_dat$delta <- as.numeric(grepl("Delta", filter_dat$model))
filter_dat$delta <- factor(filter_dat$delta,labels=c("Non-delta","Delta"))

dat.spatial.eff <- filter(filter_dat, Covariate=="Space, Effort") %>% mutate(medRatio = NA) %>%
                    filter(model %in% c("Ratio", "GAM Delta", "GAM", "RF Delta", "RF"))
dat.ratio <- filter(filter_dat, model=="Ratio") %>% group_by(species) %>% mutate(medRatio = median(rmse))
dat.plot <- rbind(dat.spatial.eff, dat.ratio)
dat.plot$model <- factor(dat.plot$model, levels = c("Ratio", "GAM Delta", "GAM", "RF Delta", "RF"))

print(dat.plot %>% #group_by(species, model) %>%
  ggplot(aes(model, rmse, fill = model)) + 
  # ggplot(aes(model, rmse, fill = model, alpha=delta)) + 
  	# geom_boxplot(outlier.shape = NA) + 
  	geom_violin(draw_quantiles = c(0.5)) +
  	geom_hline(aes(yintercept = medRatio, group = species), linetype = 2) +
  	facet_wrap(~ species) + 
  	theme_sleek() + 
  	xlab("Model") + 
  	ylab("Normalized RMSE") + 
  	theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  	coord_cartesian(ylim = c(0,1)) +
	scale_fill_manual(name="Model", values=c("grey","#E69F00","#E69F0080","#56B4E9","#56B4E980")))
	# scale_fill_manual(name="Model",breaks=c("Ratio","GAM","RF"), values=c("#E69F00","#E69F00","grey","#56B4E9","#56B4E9")) +	
	# scale_alpha_discrete(range = c(0.5, 1), guide=FALSE)
dev.off()

# --------------------------------------------------------------
# add "RF Best" and "VAST"
pdf("figures/fig3_model_comparison/fig3_model_comparison_0.2_space_effort_v2.pdf", width=8, height=7)

dat.spatial.eff <- filter(filter_dat, Covariate=="Space, Effort") %>% mutate(medRatio = NA)
dat.ratio <- filter(filter_dat, model=="Ratio") %>% group_by(species) %>% mutate(medRatio = median(rmse))
dat.plot <- rbind(dat.spatial.eff, dat.ratio)
dat.plot$model <- factor(dat.plot$model, levels = c("Ratio", "GAM Delta", "GAM", "VAST","RF Delta", "RF", "RF Best"))

colors <- scales::brewer_pal(type="qual")(5)

print(dat.plot %>% #group_by(species, model) %>%
  ggplot(aes(model, rmse, fill = model)) + 
  # ggplot(aes(model, rmse, fill = model, alpha=delta)) + 
    # geom_boxplot(outlier.shape = NA) + 
    geom_violin(draw_quantiles = c(0.5)) +
    geom_hline(aes(yintercept = medRatio, group = species), linetype = 2) +
    facet_wrap(~ species) + 
    theme_sleek() + 
    xlab("Model") + 
    ylab("Normalized RMSE") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    coord_cartesian(ylim = c(0,1)) +
  # scale_fill_manual(name="Model", values=c("grey","#E69F00","#E69F0080","#56B4E9","#56B4E980")))
  scale_fill_manual(name="Model", values=c(colors[1],colors[2],paste0(colors[2],"80"),colors[3],colors[4],paste0(colors[4],"80"),colors[5])))
  # scale_fill_manual(name="Model",breaks=c("Ratio","GAM","RF"), values=c("#E69F00","#E69F00","grey","#56B4E9","#56B4E9")) +  
  # scale_alpha_discrete(range = c(0.5, 1), guide=FALSE)
dev.off()

# --------------------------------------------------------------
# v3: only Ratio, GAM-Tweedie, and RF (no delta models)
#   - add "AVERAGE" last panel summarizing across all species
pdf("figures/fig3_model_comparison/fig3_model_comparison_0.2_space_effort_v3.pdf", width=6, height=7)

# add "AVERAGE" as a 'species' to put in bottom right panel
sp.labs <- levels(dat$species)
dat$species <- as.character(dat$species)
dat.allspecies <- dat
dat.allspecies$species = "AVERAGE"
dat.all <- rbind(dat, dat.allspecies)
dat.all$species <- factor(dat.all$species, levels = c(sp.labs,"AVERAGE"))

filter_dat = group_by(dat.all, species, model, Covariate) %>% 
  mutate(rmse = ifelse(rmse > 10, NA, rmse)) %>% 
  ungroup() %>% 
  group_by(species) %>% 
  mutate(max_rmse = max(rmse,na.rm=T))
filter_dat$delta <- as.numeric(grepl("Delta", filter_dat$model))
filter_dat$delta <- factor(filter_dat$delta,labels=c("Non-delta","Delta"))

dat.spatial.eff <- filter(filter_dat, Covariate=="Space, Effort") %>% mutate(medRatio = NA) %>%
                    filter(model %in% c("Ratio", "GAM", "RF"))
dat.ratio <- filter(filter_dat, model=="Ratio") %>% group_by(species) %>% mutate(medRatio = median(rmse))
dat.plot <- rbind(dat.spatial.eff, dat.ratio)
dat.plot$model <- factor(dat.plot$model, levels = c("Ratio", "GAM", "RF"))
dat.plot$species <- factor(dat.plot$species, levels = c(sp.labs,"AVERAGE"))

print(dat.plot %>% #group_by(species, model) %>%
  ggplot(aes(model, rmse, fill = model)) + 
  # ggplot(aes(model, rmse, fill = model, alpha=delta)) + 
    # geom_boxplot(outlier.shape = NA) + 
    geom_violin(draw_quantiles = c(0.5)) +
    geom_hline(aes(yintercept = medRatio, group = species), linetype = 2) +
    facet_wrap(~ species) + 
    theme_sleek() + 
    xlab("Model") + 
    ylab("Normalized RMSE") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
          # strip.text.x = element_text(face=c(rep("plain",15),"bold"))) + 
    # strip.text.x = element_text(face="bold")) + 
    coord_cartesian(ylim = c(0,1)) +
  scale_fill_manual(name="Model", values=c("grey","#E69F00","#56B4E9")))
  # scale_fill_manual(name="Model",breaks=c("Ratio","GAM","RF"), values=c("#E69F00","#E69F00","grey","#56B4E9","#56B4E9")) +  
  # scale_alpha_discrete(range = c(0.5, 1), guide=FALSE)
dev.off()

# --------------------------------------------------------------
# v4: Ratio, GAM-Tweedie, RF-Total, and VAST (no effort)
#   - add "AVERAGE" last panel summarizing across all species
pdf("figures/fig3_model_comparison/fig3_model_comparison_0.2_space_effort_v4.pdf", width=7, height=7)

dat.vast <- filter(filter_dat, Covariate=="Space") %>% mutate(medRatio = NA) %>%
                    filter(model %in% c("VAST"))
dat.spatial.eff <- filter(filter_dat, Covariate=="Space, Effort") %>% mutate(medRatio = NA) %>%
                    filter(model %in% c("Ratio", "GAM", "RF"))
dat.ratio <- filter(filter_dat, model=="Ratio") %>% group_by(species) %>% mutate(medRatio = median(rmse))
dat.plot <- rbind(dat.spatial.eff, dat.ratio, dat.vast)
dat.plot$model <- factor(dat.plot$model, levels = c("Ratio", "GAM", "VAST", "RF"))
dat.plot$species <- factor(dat.plot$species, levels = c(sp.labs,"AVERAGE"))

print(dat.plot %>% #group_by(species, model) %>%
  ggplot(aes(model, rmse, fill = model)) + 
  # ggplot(aes(model, rmse, fill = model, alpha=delta)) + 
    # geom_boxplot(outlier.shape = NA) + 
    geom_violin(draw_quantiles = c(0.5)) +
    geom_hline(aes(yintercept = medRatio, group = species), linetype = 2) +
    facet_wrap(~ species) + 
    theme_sleek() + 
    xlab("Model") + 
    ylab("Normalized RMSE") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
          # strip.text.x = element_text(face=c(rep("plain",15),"bold"))) + 
    # strip.text.x = element_text(face="bold")) + 
    coord_cartesian(ylim = c(0,1)) +
  # scale_fill_manual(name="Model", values=c("grey","#E69F00","#56B4E9")))
  scale_fill_manual(name="Model", values=c("grey","#E69F00","#009E73","#56B4E9"))) # color blind friendly palette from https://gist.github.com/ateucher/6543366
  # scale_fill_manual(name="Model",breaks=c("Ratio","GAM","RF"), values=c("#E69F00","#E69F00","grey","#56B4E9","#56B4E9")) +  
  # scale_alpha_discrete(range = c(0.5, 1), guide=FALSE)
dev.off()

# overall stats
library(tidyr)
dat.plot$model <- as.character(dat.plot$model)
tmp <- as.data.frame(dat.plot %>% filter(species=="AVERAGE")) %>% select(model, rmse) %>% 
            group_by(model) %>% mutate(id = row_number()) %>% spread(model, rmse) %>% select(-id) %>%
            mutate(GAM.Ratio=GAM-Ratio, RF.Ratio=RF-Ratio, VAST.Ratio=VAST-Ratio) %>%
            mutate(GAM.Ratio.perc=GAM.Ratio/Ratio, RF.Ratio.perc=RF.Ratio/Ratio, VAST.Ratio.perc=VAST.Ratio/Ratio)
round(apply(tmp,2,median,na.rm=T),2)

# Ratio median(RMSE) = 0.22
# Improvement on ratio estimator (% lower median RMSE):
#   GAM   4%
#   VAST 11%
#   RF   26%
       #      GAM           Ratio              RF            VAST       GAM.Ratio 
       #     0.21            0.22            0.16            0.18           -0.01 
       # RF.Ratio      VAST.Ratio  GAM.Ratio.perc   RF.Ratio.perc VAST.Ratio.perc 
       #    -0.06           -0.02           -0.04           -0.26           -0.11

# --------------------------------------------------------------
# v5: Ratio, GAM-Tweedie, RF-Total, VAST, and Ensemble (= avg of RF, VAST)
#   - add "AVERAGE" last panel summarizing across all species
pdf("figures/fig3_model_comparison/fig3_model_comparison_0.2_space_effort_v5.pdf", width=8, height=7)

# Get ratio, VAST, RF results
dat.vast <- filter(filter_dat, Covariate=="Space") %>% mutate(medRatio = NA) %>%
                    filter(model %in% c("VAST"))
dat.spatial.eff <- filter(filter_dat, Covariate=="Space, Effort") %>% mutate(medRatio = NA) %>%
                    filter(model %in% c("Ratio", "GAM", "RF"))
dat.ratio <- filter(filter_dat, model=="Ratio") %>% group_by(species) %>% mutate(medRatio = median(rmse,na.rm=T))

# Create ensemble = mean(VAST,RF)
dat.ensemble <- dat.vast
err_cols <- c("e_2011","e_2012","e_2013","e_2014","e_2015","pe_2011","pe_2012","pe_2013","pe_2014","pe_2015")
temp_array <- abind::abind(dat.vast[, err_cols], dat.spatial.eff[dat.spatial.eff$model=="RF", err_cols], along=3)
dat.ensemble[, err_cols] <- apply(temp_array, 1:2, mean, na.rm=T)
dat.ensemble$model = "Ensemble"
dat.ensemble$rmse = sqrt((dat.ensemble$e_2011^2 + dat.ensemble$e_2012^2 + dat.ensemble$e_2013^2 + dat.ensemble$e_2014^2 + dat.ensemble$e_2015^2)/5) / dat.ensemble$mean_dat

dat.plot <- rbind(dat.spatial.eff, dat.ratio, dat.vast, dat.ensemble)
dat.plot$model <- factor(dat.plot$model, levels = c("Ratio", "GAM", "VAST", "RF","Ensemble"))
dat.plot$species <- factor(dat.plot$species, levels = c(sp.labs,"AVERAGE"))

print(dat.plot %>% #group_by(species, model) %>%
  ggplot(aes(model, rmse, fill = model)) + 
  # ggplot(aes(model, rmse, fill = model, alpha=delta)) + 
    # geom_boxplot(outlier.shape = NA) + 
    geom_violin(draw_quantiles = c(0.5)) +
    geom_hline(aes(yintercept = medRatio, group = species), linetype = 2) +
    facet_wrap(~ species) + 
    theme_sleek() + 
    xlab("Model") + 
    ylab("Normalized RMSE") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
          # strip.text.x = element_text(face=c(rep("plain",15),"bold"))) + 
    # strip.text.x = element_text(face="bold")) + 
    coord_cartesian(ylim = c(0,1)) +
  # scale_fill_manual(name="Model", values=c("grey","#E69F00","#56B4E9")))
  scale_fill_manual(name="Model", values=c("grey","#E69F00","#009E73","#56B4E9","white"))) # color blind friendly palette from https://gist.github.com/ateucher/6543366
  # scale_fill_manual(name="Model",breaks=c("Ratio","GAM","RF"), values=c("#E69F00","#E69F00","grey","#56B4E9","#56B4E9")) +  
  # scale_alpha_discrete(range = c(0.5, 1), guide=FALSE)
dev.off()

# overall stats
library(tidyr)
dat.plot$model <- as.character(dat.plot$model)
tmp <- as.data.frame(dat.plot %>% filter(species=="AVERAGE")) %>% select(model, rmse) %>% 
            group_by(model) %>% mutate(id = row_number()) %>% spread(model, rmse) %>% select(-id) %>%
            mutate(GAM.Ratio=GAM-Ratio, RF.Ratio=RF-Ratio, VAST.Ratio=VAST-Ratio, Ens.Ratio=Ensemble-Ratio) %>%
            mutate(GAM.Ratio.perc=GAM.Ratio/Ratio, RF.Ratio.perc=RF.Ratio/Ratio, VAST.Ratio.perc=VAST.Ratio/Ratio, Ens.Ratio.perc=Ens.Ratio/Ratio)
round(apply(tmp,2,median,na.rm=T),2)

# Ratio median(RMSE) = 0.22
# Improvement on ratio estimator (% lower median RMSE):
#   GAM   4%
#   VAST 11%
#   RF   26%
#   Ens  33%
  #      Ensemble             GAM           Ratio              RF            VAST 
  #          0.14            0.21            0.22            0.16            0.18 
  #     GAM.Ratio        RF.Ratio      VAST.Ratio       Ens.Ratio  GAM.Ratio.perc 
  #         -0.01           -0.06           -0.02           -0.07           -0.04 
  # RF.Ratio.perc VAST.Ratio.perc  Ens.Ratio.perc 
  #         -0.26           -0.11           -0.33

# --------------------------------------------------------------
# v6: Ratio, RF-Total, and RF nonlinear

# add "AVERAGE" as a 'species' to put in bottom right panel
sp.labs <- levels(dat$species)
dat$species <- as.character(dat$species)
dat.allspecies <- dat
dat.allspecies$species = "AVERAGE"
dat.all <- rbind(dat, dat.allspecies)
dat.all$species <- factor(dat.all$species, levels = c(sp.labs,"AVERAGE"))

filter_dat = group_by(dat.all, species, model, Covariate) %>% 
  mutate(rmse = ifelse(rmse > 10, NA, rmse)) %>% 
  ungroup() %>% 
  group_by(species) %>% 
  mutate(max_rmse = max(rmse,na.rm=T))

# Get results
dat.spatial.eff <- filter(filter_dat, Covariate=="Space, Effort") %>% mutate(medRatio = NA) %>%
                    filter(model %in% c("Ratio", "GAM","GAM Nonlinear","RF", "RF Nonlinear"))
dat.ratio <- filter(filter_dat, model=="Ratio") %>% group_by(species) %>% mutate(medRatio = median(rmse,na.rm=T))

dat.plot <- rbind(dat.spatial.eff, dat.ratio)
dat.plot$model <- factor(dat.plot$model, levels = c("Ratio", "GAM","GAM Nonlinear","RF", "RF Nonlinear"))
dat.plot$species <- factor(dat.plot$species, levels = c(sp.labs,"AVERAGE"))

pdf("figures/fig3_model_comparison/fig3_model_comparison_nonlinear.pdf", width=6, height=7)
print(dat.plot %>% #group_by(species, model) %>%
  ggplot(aes(model, rmse, fill = model)) + 
  # ggplot(aes(model, rmse, fill = model, alpha=delta)) + 
    # geom_boxplot(outlier.shape = NA) + 
    geom_violin(draw_quantiles = c(0.5)) +
    geom_hline(aes(yintercept = medRatio, group = species), linetype = 2) +
    facet_wrap(~ species) + 
    theme_sleek() + 
    xlab("Model") + 
    ylab("Normalized RMSE") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
          # strip.text.x = element_text(face=c(rep("plain",15),"bold"))) + 
    # strip.text.x = element_text(face="bold")) + 
    coord_cartesian(ylim = c(0,1)) +
  # scale_fill_manual(name="Model", values=c("grey","#E69F00","#56B4E9")))
  scale_fill_manual(name="Model", values=c("grey","#E69F00","#E69F0080","#56B4E9","#56B4E980"))) # color blind friendly palette from https://gist.github.com/ateucher/6543366
  # scale_fill_manual(name="Model",breaks=c("Ratio","GAM","RF"), values=c("#E69F00","#E69F00","grey","#56B4E9","#56B4E9")) +  
  # scale_alpha_discrete(range = c(0.5, 1), guide=FALSE)
dev.off()

library(tidyr)
dat.plot$model <- as.character(dat.plot$model)
tmp <- as.data.frame(dat.plot %>% filter(species=="AVERAGE")) %>% select(model, rmse) %>% 
            group_by(model) %>% mutate(id = row_number()) %>% spread(model, rmse) %>% select(-id)
round(apply(tmp,2,quantile, probs=c(0,0.025,0.05,0.25,0.5,0.75,0.95,0.975,1),na.rm=T),2)

#       GAM Delta GAM Nonlinear GAM Total Ratio RF Delta RF Nonlinear RF Total
# 0%         0.02          0.03      0.04  0.03     0.02         0.03     0.02
# 2.5%       0.07          0.07      0.07  0.07     0.05         0.06     0.06
# 5%         0.09          0.08      0.08  0.09     0.06         0.07     0.07
# 25%        0.15          0.13      0.14  0.15     0.10         0.11     0.11
# 50%        0.26          0.19      0.21  0.22     0.14         0.16     0.16
# 75%        0.50          0.31      0.34  0.33     0.22         0.24     0.23
# 95%        1.81          0.66      1.16  0.61     0.46         0.43     0.44
# 97.5%      2.94          1.23      2.98  0.74     0.57         0.49     0.57
# 100%       9.62          9.64      9.75  2.07     1.39         0.89     1.62

# -----------------------------------------------------------------------
# Figure 6 needs to be made as png
library(tidyverse)
library(ggsidekick)

setwd("/home/brian/Documents/Bycatch/fleetwide")
dat = readRDS("figures/results_summary_depthratio.rds")
dat <- filter(dat, !(model %in% c("RF Cubist","RF Xu2","RF Xu4")))
dat$model <- as.character(dat$model)

# for now we'll only plot the simulations with 20% coverage
dat = filter(dat, pct_trips==0.2)

# facet by species. model options are including space, effort, both, neither (color)
dat$Covariate = paste(dat$spatial, dat$effort)
dat$Covariate[which(dat$Covariate==" ")] = "Neither"
dat$Covariate[which(dat$Covariate=="SP EFF")] = "Space, Effort"
dat$Covariate[which(dat$Covariate=="SP ")] = "Space"
dat$Covariate[which(dat$Covariate==" EFF")] = "Effort"

filter_dat = dat %>% 
  group_by(species) %>% 
  mutate(max_rmse = max(rmse,na.rm=T))
filter_dat$delta <- as.numeric(grepl("Delta", filter_dat$model))
filter_dat$delta <- factor(filter_dat$delta,labels=c("Non-delta","Delta"))

dat.spatial.eff <- filter(filter_dat, Covariate=="Space, Effort") %>% 
  filter(model %in% c("Ratio", "GAM Nonlinear", "RF Nonlinear"))
dat.ratio <- filter(filter_dat, model=="Ratio")
dat.plot <- rbind(dat.spatial.eff, dat.ratio)
dat.plot$model <- factor(dat.plot$model, levels = c("Ratio", "GAM Nonlinear", "RF Nonlinear"))

# gather annual estimates for UNOBSERVED bycatch
dat.plot <- as.data.frame(tidyr::gather(dat.plot, year, est_unobs, c("est_2011","est_2012","est_2013","est_2014","est_2015")))
dat.plot$year <- gsub("est_", "", dat.plot$year)
dat.plot$year <- as.factor(dat.plot$year)
dat.plot <- dat.plot %>% select(species, model, sim, year, est_unobs)

dat.true <- readRDS("wcgop data/true_bycatch_byyear.rds")
dat.true <- dplyr::mutate(dat.true, species = fct_recode(species, 
  "Big skate" = "big skate", "Black skate" = "black skate", 
  "Brown cat shark" = "brown cat shark", "California slickhead" = "california slickhead",
  "Dungeness crab" = "dungeness crab", "Grenadier" = "grenadier", 
  "Octopus" = "octopus unid", "Pacific hake" = "pacific hake",
  "Pacific halibut" = "pacific halibut", "Rosethorn rockfish" = "rosethorn rockfish",
  "Slender sole" = "slender sole", "Spiny dogfish shark" = "spiny dogfish shark",
  "Sandpaper skate" = "sandpaper skate", "Spotted ratfish" = "spotted ratfish",
  "Tanner crab" = "tanneri tanner crab"))
# for some reason rosethorn rockfish and sandpaper skate got mixed up in dat.true
dat.true$species <- as.character(dat.true$species)
dat.plot$species <- as.character(dat.plot$species)
sp.labs <- names(table(dat.plot$species))
sp.labs[3] = "Brown catshark"
dat.true$species[dat.true$species=="Brown cat shark"] = "Brown catshark"
dat.plot$species[dat.plot$species=="Brown cat shark"] = "Brown catshark"
dat.true$species <- factor(dat.true$species, levels=sp.labs)
dat.plot$species <- factor(dat.plot$species, levels=sp.labs)

# add true_obs and true_full to dat.plot (same for all models)
dat.plot <- dat.plot[with(dat.plot, order(model, species, year, sim)), ]
dat.true <- dat.true[with(dat.true, order(species, year, sim)), ] 
dat.plot$true_obs = NA
dat.plot$true_full = NA
dat.plot[dat.plot$model=="Ratio", c("true_obs","true_full")] <- dat.true[, c("true_obs","true_full")]
dat.plot[dat.plot$model=="RF Nonlinear", c("true_obs","true_full")] <- dat.true[, c("true_obs","true_full")]
dat.plot[dat.plot$model=="GAM Nonlinear", c("true_obs","true_full")] <- dat.true[, c("true_obs","true_full")]
dat.plot$est_full <- dat.plot$true_obs + dat.plot$est_unobs
dat.plot <- as.data.frame(dat.plot %>% group_by(species) %>% mutate(max_catch=max(true_full)) %>% 
            ungroup() %>% mutate(est_full_max=est_full/max_catch, true_full_max=true_full/max_catch))

cairo_pdf(filename="revision/fig6_model_comparison_byyear_r2.pdf", width=10,height=7,fallback_resolution=600)
# png("revision/fig6_model_comparison_byyear_r1.png",units='in',height=7,width=10,res=600)
# png("figures/fig3_model_comparison/fig6_model_comparison_byyear.png",units='in',height=7,width=10,res=600)
print(dat.plot %>% 
  ggplot(aes(x=year, y=est_full_max, fill = model)) + 
    geom_boxplot(outlier.shape = NA) + 
    geom_errorbar(aes(ymax=true_full_max, ymin=true_full_max, group=interaction(species,year)), linetype=2) +
    facet_wrap(~ species) + 
    theme_sleek() + 
    xlab("Year") + 
    ylab("Relative bycatch (% of max by species)") + 
    coord_cartesian(ylim = c(0,1.2)) +
    scale_fill_manual(name="Model", labels=c("Ratio", "GAM Nonlinear", "RF Nonlinear"), values=c("grey","#E69F00","#56B4E9")))
dev.off()

# -------------------------------------------------------------------
# Revise Figure 5 to 'Brown catshark'
library(tidyverse)
library(ggsidekick)
setwd("/home/brian/Documents/Bycatch/fleetwide")
dat = readRDS("figures/results_summary_depthratio.rds")
dat <- filter(dat, !(model %in% c("RF Cubist","RF Xu2","RF Xu4")))
dat$model <- as.character(dat$model)

# for now we'll only plot the simulations with 20% coverage
dat = filter(dat, pct_trips==0.2)

# facet by species. model options are including space, effort, both, neither (color)
dat$Covariate = paste(dat$spatial, dat$effort)
dat$Covariate[which(dat$Covariate==" ")] = "Neither"
dat$Covariate[which(dat$Covariate=="SP EFF")] = "Space, Effort"
dat$Covariate[which(dat$Covariate=="SP ")] = "Space"
dat$Covariate[which(dat$Covariate==" EFF")] = "Effort"

sp.labs <- levels(dat$species)
sp.labs[3] <- "Brown catshark"
dat$species <- as.character(dat$species)
dat$species[dat$species=="Brown cat shark"] = "Brown catshark"
dat.allspecies <- dat
dat.allspecies$species = "AVERAGE"
dat.all <- rbind(dat, dat.allspecies)
dat.all$species <- factor(dat.all$species, levels = c(sp.labs,"AVERAGE"))

filter_dat = group_by(dat.all, species, model, Covariate) %>% 
  mutate(rmse = ifelse(rmse > 10, NA, rmse)) %>% 
  ungroup() %>% 
  group_by(species) %>% 
  mutate(max_rmse = max(rmse,na.rm=T))
filter_dat$delta <- as.numeric(grepl("Delta", filter_dat$model))
filter_dat$delta <- factor(filter_dat$delta,labels=c("Non-delta","Delta"))

dat.spatial.eff <- filter(filter_dat, Covariate=="Space, Effort") %>% mutate(medRatio = NA) %>%
                    filter(model %in% c("GAM Delta", "GAM", "GAM Nonlinear", "RF Delta", "RF", "RF Nonlinear"))
dat.ratio <- filter(filter_dat, model=="Ratio") %>% group_by(species) %>% mutate(medRatio = median(rmse,na.rm=T))
dat.plot <- rbind(dat.spatial.eff, dat.ratio)
dat.plot$model <- factor(dat.plot$model, levels = c("Ratio", "GAM Delta", "GAM", "GAM Nonlinear","RF Delta", "RF", "RF Nonlinear"))
levels(dat.plot$model) <- c("Ratio", "GAM Delta", "GAM Total", "GAM Nonlinear","RF Delta", "RF Total", "RF Nonlinear")
dat.plot$species <- factor(dat.plot$species, levels = c(sp.labs,"AVERAGE"))

cairo_pdf(filename="revision/fig5_model_comparison_r2.pdf", width=6.7,height=6,fallback_resolution=600)
# png(filename="revision/fig5_model_comparison_r1.png", width=9,height=8,units="in",res=600)
print(dat.plot %>% 
  ggplot(aes(model, rmse, fill = model)) + 
    geom_violin(draw_quantiles = c(0.5)) +
    geom_hline(aes(yintercept = medRatio, group = species), linetype = 2) +
    facet_wrap(~ species) + 
    theme_sleek() + 
    xlab("Model") + 
    ylab("Normalized RMSE") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    coord_cartesian(ylim = c(0,1)) +
  scale_fill_manual(name="Model",
                    values=c("grey","#E69F0080","#E69F00",rgb(t(col2rgb("#E69F00")/1.2), maxColorValue=255),"#56B4E980","#56B4E9", rgb(t(col2rgb("#56B4E9")/1.2), maxColorValue=255))))
dev.off()

