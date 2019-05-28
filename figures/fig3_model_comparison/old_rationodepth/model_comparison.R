library(dplyr)
library(ggplot2)
library(ggsidekick)

# setwd("/home/brian/Documents/Bycatch/fleetwide")

# created by /figures/summarize_results.R
dat = readRDS("figures/results_summary.rds")

# for now we'll only plot the simulations with 20% coverage
dat = filter(dat, pct_trips==0.2)

# facet by species. model options are including space, effort, both, neither (color)
dat$Covariate = paste(dat$spatial, dat$effort)
dat$Covariate[which(dat$Covariate==" ")] = "Neither"
dat$Covariate[which(dat$Covariate=="SP EFF")] = "Space, Effort"
dat$Covariate[which(dat$Covariate=="SP ")] = "Space"
dat$Covariate[which(dat$Covariate==" EFF")] = "Effort"

pdf("figures/model_comparison.pdf")

# for purposes of visualizing violins, turn observations outside 99% CIs to NAs
filter_dat = group_by(dat, species, model, Covariate) %>% 
  # mutate(rmse = ifelse(rmse > 100, NA, rmse)) %>% 
  mutate(rmse = ifelse(rmse > 10, NA, rmse)) %>% 
  ungroup() %>% 
  group_by(species) %>% 
  mutate(max_rmse = max(rmse,na.rm=T))

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
pdf("fig3_model_comparison/fig3_model_comparison_0.2_space_effort.pdf", width=8, height=7)

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
pdf("fig3_model_comparison/fig3_model_comparison_0.2_space_effort_v2.pdf", width=8, height=7)

filter_dat = group_by(dat, species, model, Covariate) %>% 
  mutate(rmse = ifelse(rmse > 10, NA, rmse)) %>% 
  ungroup() %>% 
  group_by(species) %>% 
  mutate(max_rmse = max(rmse,na.rm=T))
filter_dat$delta <- as.numeric(grepl("Delta", filter_dat$model))
filter_dat$delta <- factor(filter_dat$delta,labels=c("Non-delta","Delta"))

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
#   - add "OVERALL" last panel summarizing across all species
pdf("fig3_model_comparison/fig3_model_comparison_0.2_space_effort_v3.pdf", width=6, height=7)

# add "OVERALL" as a 'species' to put in bottom right panel
sp.labs <- levels(dat$species)
dat$species <- as.character(dat$species)
dat.allspecies <- dat
dat.allspecies$species = "OVERALL"
dat.all <- rbind(dat, dat.allspecies)
dat.all$species <- factor(dat.all$species, levels = c(sp.labs,"OVERALL"))

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
dat.plot$species <- factor(dat.plot$species, levels = c(sp.labs,"OVERALL"))

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

# overall stats
library(tidyr)
dat.plot$model <- as.character(dat.plot$model)
tmp <- as.data.frame(dat.plot %>% filter(species=="OVERALL")) %>% select(model, rmse) %>% 
            group_by(model) %>% mutate(id = row_number()) %>% spread(model, rmse) %>% select(-id) %>%
            mutate(GAM.Ratio=GAM-Ratio, RF.Ratio=RF-Ratio) %>%
            mutate(GAM.Ratio.perc=GAM.Ratio/Ratio, RF.Ratio.perc=RF.Ratio/Ratio)
round(apply(tmp,2,median,na.rm=T),2)

#            GAM          Ratio             RF      GAM.Ratio       RF.Ratio 
#           0.21           0.22           0.16          -0.01          -0.06 
# GAM.Ratio.perc  RF.Ratio.perc 
#          -0.05          -0.27 
