# -----------------------------------------------------------
# Now plot Bycatch by year instead of Percent error
#   have to add the true_obs to est_unobs to get est_fleetwide
library(dplyr)
library(ggplot2)
library(ggsidekick)

setwd("/home/brian/Documents/Bycatch/fleetwide/figures")

# created by /figures/summarize_results.R
dat = readRDS("results_summary.rds")

# for now we'll only plot the simulations with 20% coverage
dat = filter(dat, pct_trips==0.2)

# facet by species. model options are including space, effort, both, neither (color)
dat$Covariate = paste(dat$spatial, dat$effort)
dat$Covariate[which(dat$Covariate==" ")] = "Neither"
dat$Covariate[which(dat$Covariate=="SP EFF")] = "Space, Effort"
dat$Covariate[which(dat$Covariate=="SP ")] = "Space"
dat$Covariate[which(dat$Covariate==" EFF")] = "Effort"

# # add "AVERAGE" as a 'species' to put in bottom right panel
# sp.labs <- levels(dat$species)
# dat$species <- as.character(dat$species)
# dat.allspecies <- dat 
# dat.allspecies <- dat %>% filter(model %in% c("Ratio", "GAM", "RF"))
# dat.allspecies$species = "AVERAGE"
# dat.all <- rbind(dat, dat.allspecies)
# dat.all$species <- factor(dat.all$species, levels = c(sp.labs,"AVERAGE"))

filter_dat = dat %>% 
# filter_dat = dat.all %>% 
  # group_by(species, model, Covariate) %>% 
  # mutate(rmse = ifelse(rmse > 10, NA, rmse)) %>% 
  # ungroup() %>% 
  group_by(species) %>% 
  mutate(max_rmse = max(rmse,na.rm=T))
filter_dat$delta <- as.numeric(grepl("Delta", filter_dat$model))
filter_dat$delta <- factor(filter_dat$delta,labels=c("Non-delta","Delta"))

dat.spatial.eff <- filter(filter_dat, Covariate=="Space, Effort") %>% mutate(medRatio = NA) %>%
                    filter(model %in% c("Ratio", "GAM", "RF"))
dat.ratio <- filter(filter_dat, model=="Ratio") %>% group_by(species) %>% mutate(medRatio = median(rmse))
dat.plot <- rbind(dat.spatial.eff, dat.ratio)
dat.plot$model <- factor(dat.plot$model, levels = c("Ratio", "GAM", "RF"))
# dat.plot$species <- factor(dat.plot$species, levels = c(sp.labs,"AVERAGE"))

# gather annual estimates for UNOBSERVED bycatch
dat.plot <- as.data.frame(tidyr::gather(dat.plot, year, est_unobs, c("est_2011","est_2012","est_2013","est_2014","est_2015")))
dat.plot$year <- gsub("est_", "", dat.plot$year)
dat.plot$year <- as.factor(dat.plot$year)
dat.plot <- dat.plot %>% select(species, model, sim, year, est_unobs)

library(forcats)
dat.true <- readRDS("../wcgop data/true_bycatch_byyear.rds")
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
dat.true$species <- factor(dat.true$species, levels=sp.labs)
dat.plot$species <- factor(dat.plot$species, levels=sp.labs)

# add true_obs and true_full to dat.plot (same for all models)
dat.plot <- dat.plot[with(dat.plot, order(model, species, year, sim)), ]
dat.true <- dat.true[with(dat.true, order(species, year, sim)), ] 
dat.plot$true_obs = NA
dat.plot$true_full = NA
dat.plot[dat.plot$model=="Ratio", c("true_obs","true_full")] <- dat.true[, c("true_obs","true_full")]
dat.plot[dat.plot$model=="RF", c("true_obs","true_full")] <- dat.true[, c("true_obs","true_full")]
dat.plot[dat.plot$model=="GAM", c("true_obs","true_full")] <- dat.true[, c("true_obs","true_full")]
dat.plot$est_full <- dat.plot$true_obs + dat.plot$est_unobs
dat.plot <- as.data.frame(dat.plot %>% group_by(species) %>% mutate(max_catch=max(true_full)) %>% ungroup() %>% 
    mutate(est_full_max=est_full/max_catch, true_full_max=true_full/max_catch))

# v1 boxplots
pdf("fig6_model_comparison_byyear/fig6_model_comparison_byyear_catch.pdf", width=12, height=7)
print(dat.plot %>% 
  ggplot(aes(x=year, y=est_full_max, fill = model)) + 
    geom_boxplot(outlier.shape = NA) + 
    geom_errorbar(aes(ymax=true_full_max, ymin=true_full_max, group=interaction(species,year))) +
    facet_wrap(~ species) + 
    theme_sleek() + 
    xlab("Year") + 
    ylab("Relative bycatch (% of max by species)") + 
    coord_cartesian(ylim = c(0,1.2)) +
    scale_fill_manual(name="Model", values=c("grey","#E69F00","#56B4E9")))
dev.off()

# v2 pointrange
pdf("fig6_model_comparison_byyear/fig6_model_comparison_byyear_catch_v2.pdf", width=10, height=7)
print(dat.plot %>% #group_by(species, model) %>%
  ggplot(aes(x=year, y=est_full_max)) + 
  # ggplot(aes(model, rmse, fill = model, alpha=delta)) + 
    # geom_boxplot(outlier.shape = NA) + 
    stat_summary(aes(color = model, group=interaction(model,species,year)),
               fun.y = median,
               fun.ymin = function(x) quantile(x, 0.1),
               fun.ymax = function(x) quantile(x, 0.9), 
               fatten = 1,
               size=1.5,
               geom = "pointrange",
               position = position_dodge(width = 1)) +
    # geom_violin(draw_quantiles = c(0.5)) +
    geom_errorbar(aes(ymax=true_full_max, ymin=true_full_max, group=interaction(species,year)), linetype=2,color="black") +
    # geom_hline(aes(yintercept = true_full_max, group = year), linetype = 2) +
    # geom_hline(aes(yintercept = true_full_max, group = interaction(species,year)), linetype = 2) +
    facet_wrap(~ species) + 
    theme_sleek() + 
    xlab("Year") + 
    ylab("Relative bycatch (% of max by species)") + 
    coord_cartesian(ylim = c(0,1.2)) +
    scale_color_manual(name="Model", values=c("grey","#E69F00","#56B4E9")))
dev.off()

# v3 colored points with black borders and error bars
pdf("fig6_model_comparison_byyear/fig6_model_comparison_byyear_catch_v3.pdf", width=10, height=7)
print(dat.plot %>% #group_by(species, model) %>%
  ggplot(aes(x=year, y=est_full_max)) + 
    stat_summary(aes(group=interaction(model,species,year)),
               fun.ymin = function(x) quantile(x, 0.1),
               fun.ymax = function(x) quantile(x, 0.9),
               colour="black",
               geom = "linerange",
               position = position_dodge(width = 1)) +  
    stat_summary(aes(fill=model, group=interaction(model,species,year)),
               fun.y = median,
               colour="black", pch=21, size=2.5,
               geom = "point",
               position = position_dodge(width = 1)) +
    # geom_violin(draw_quantiles = c(0.5)) +
    geom_errorbar(aes(ymax=true_full_max, ymin=true_full_max, group=interaction(species,year)), linetype=2,color="black") +
    # geom_hline(aes(yintercept = true_full_max, group = year), linetype = 2) +
    # geom_hline(aes(yintercept = true_full_max, group = interaction(species,year)), linetype = 2) +
    facet_wrap(~ species) + 
    theme_sleek() + 
    xlab("Year") + 
    ylab("Relative bycatch (% of max by species)") + 
    coord_cartesian(ylim = c(0,1.2)) +
    scale_fill_manual(name="Model", values=c("grey","#E69F00","#56B4E9")))
dev.off()

