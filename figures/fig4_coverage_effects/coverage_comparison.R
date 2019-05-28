library(dplyr)
library(ggplot2)
library(ggsidekick)

setwd("/home/brian/Documents/Bycatch/fleetwide/")
# dat = readRDS("figures/results_summary.rds") # created by /figures/summarize_results.R
dat = readRDS("figures/results_summary_depthratio.rds") # created by /figures/summarize_results.R

# facet by species. model options are including space, effort, both, neither (color)
dat$Covariate = paste(dat$spatial, dat$effort)
dat$Covariate[which(dat$Covariate==" ")] = "Neither"
dat$Covariate[which(dat$Covariate=="SP EFF")] = "Space, Effort"
dat$Covariate[which(dat$Covariate=="SP ")] = "Space"
dat$Covariate[which(dat$Covariate==" EFF")] = "Effort"

# add "AVERAGE" as a 'species' to put in bottom right panel
sp.labs <- levels(dat$species)
dat$species <- as.character(dat$species)
dat <- dat %>% filter(model %in% c("Ratio", "GAM", "RF","VAST"))
dat.allspecies <- dat %>% filter(model %in% c("Ratio", "RF","VAST"))
dat.allspecies$species = "AVERAGE"
dat.all <- rbind(dat, dat.allspecies)
dat.all$species <- factor(dat.all$species, levels = c(sp.labs,"AVERAGE"))

dat.vast <- filter(dat.all, Covariate=="Space") %>% filter(model=="VAST")
dat.spatial.eff <- filter(dat.all, Covariate=="Space, Effort") %>% filter(model %in% c("GAM","RF"))
dat.ratio <- filter(dat.all, model=="Ratio")
dat.plot <- rbind(dat.spatial.eff, dat.ratio, dat.vast)
dat.plot$model <- factor(dat.plot$model, levels = c("Ratio", "GAM", "VAST", "RF"))

pdf("figures/fig4_coverage_effects/fig4_coverage_comparison_space_effort.pdf", width=9, height=7)

# filter_dat = group_by(dat, species, model, Covariate) %>% 
#   mutate(rmse = ifelse(rmse > 10, NA, rmse)) %>% 
#   ungroup() %>% 
#   group_by(species) %>% 
#   mutate(max_rmse = max(rmse,na.rm=T))
# filter_dat$delta <- as.numeric(grepl("Delta", filter_dat$model))
# filter_dat$delta <- factor(filter_dat$delta,labels=c("Delta","Non-delta"))

print(dat.plot %>% #group_by(species, model) %>%
  ggplot(aes(model, rmse, fill = as.factor(pct_trips))) + 
  # ggplot(aes(model, rmse, fill = model, alpha=delta)) + 
  	# geom_boxplot(outlier.shape = NA) + 
  	geom_violin(draw_quantiles = c(0.5)) +
  	# geom_hline(aes(yintercept = medRatio, group = species), linetype = 2) +
  	facet_wrap(~ species) + 
  	theme_sleek() + 
  	xlab("Model") + 
  	ylab("Normalized RMSE") + 
  	theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  	coord_cartesian(ylim = c(0,1)) +
	  scale_fill_manual(name="Percent Observed", values=c("deepskyblue2","deepskyblue4")))
dev.off()

dat.plot %>% group_by(model, pct_trips) %>% summarize(median.rmse=median(rmse))

# overall stats
library(tidyr)
dat.plot$model <- as.character(dat.plot$model)
tmp <- as.data.frame(dat.plot %>% filter(species=="AVERAGE")) %>% select(model, rmse) %>% 
            group_by(model) %>% mutate(id = row_number()) %>% spread(model, rmse) %>% select(-id) %>%
            mutate(GAM.Ratio=GAM-Ratio, RF.Ratio=RF-Ratio, VAST.Ratio=VAST-Ratio) %>%
            mutate(GAM.Ratio.perc=GAM.Ratio/Ratio, RF.Ratio.perc=RF.Ratio/Ratio, VAST.Ratio.perc=VAST.Ratio/Ratio)
round(apply(tmp,2,median,na.rm=T),2)

# ------------------------------------------------------------------
# filter out a handful of models
# dat = filter(dat, Covariate == "Effort")

# pdf("figures/coverage_comparison.pdf")

# # for purposes of visualizing violins, turn observations outside 99% CIs to NAs
# filter_dat = group_by(dat, species, model, Covariate, pct_trips) %>% 
#   mutate(rmse = ifelse(rmse > 100, NA, rmse)) %>% 
#   ungroup() %>% 
#   group_by(species) %>% 
#   mutate(max_rmse = max(rmse,na.rm=T))

# filter_dat$pct_trips = as.factor(filter_dat$pct_trips)
# filter_dat = rename(filter_dat, Coverage = pct_trips)
# group_by(filter_dat, species, model, Covariate) %>%
#   ggplot(aes(model, rmse, fill = Coverage, group = factor(paste0(model, Coverage)))) + 
#   geom_boxplot(outlier.shape = NA) + facet_wrap(~ species, scale="free_y") + 
#   theme_sleek() + xlab("Model") + ylab("RMSE") + 
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) + coord_cartesian() +
#   scale_fill_manual(values = c("deepskyblue2","deepskyblue4")) + ylim(0,1)

# dev.off()


