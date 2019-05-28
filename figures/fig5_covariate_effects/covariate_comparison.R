library(dplyr)
library(ggplot2)
library(ggsidekick)
library(viridis)

# setwd("/home/brian/Documents/Bycatch/fleetwide/")
dat = readRDS("figures/results_summary.rds") # created by /figures/summarize_results.R

# for now we'll only plot the simulations with 20% coverage
#dat = filter(dat, pct_trips==0.2)

# facet by species. model options are including space, effort, both, neither (color)
dat$Covariate = paste(dat$spatial, dat$effort)
dat$Covariate[which(dat$Covariate==" ")] = "Neither"
dat$Covariate[which(dat$Covariate=="SP EFF")] = "Space + Effort"
dat$Covariate[which(dat$Covariate=="SP ")] = "Space"
dat$Covariate[which(dat$Covariate==" EFF")] = "Effort"

dat = group_by(dat, model, sim, pct_trips, species) %>% 
  mutate(null_rmse = ifelse(length(rmse[which(spatial=="" & effort == "")]) > 0, 
    rmse[which(spatial=="" & effort == "")], NA)) %>% 
  mutate(diff_rmse = rmse - null_rmse) 

# filter out the null model -- not used in comparison
dat = filter(dat, pct_trips == 0.2) %>% 
  filter(diff_rmse != 0 & !is.na(diff_rmse) ) 

pdf("figures/fig5_covariate_effects/fig5_covariate_comparison_0.2.pdf", width=8, height=7)

# # for purposes of visualizing violins, turn observations outside 99% CIs to NAs
# filter_dat = group_by(dat, species, model, Covariate) %>% 
#   mutate(rmse = ifelse(rmse > 100, NA, rmse)) 

group_by(dat, species, model, Covariate) %>%
  ggplot(aes(model, diff_rmse, fill = Covariate, group = factor(paste0(model, Covariate)))) + 
  facet_wrap(~ species) + 
  # facet_wrap(~ species, scale="free_y") + 
  geom_boxplot(outlier.shape = NA) +
  # geom_rect(data=NULL, aes(xmin=2.5, xmax=5, ymin=-Inf, ymax=Inf), fill="lightgrey", alpha=0.01) +
  # geom_violin(draw_quantiles = c(0.5)) +
  theme_sleek() + 
  xlab("Model") + 
  ylab("Difference in normalized RMSE") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  coord_cartesian(ylim = c(-0.5,0.5)) +
  scale_fill_brewer(name="Covariates added", type="qual", palette = 1) +
  # scale_fill_manual(name="Covariates added", values=viridis(3)) +
  # geom_hline(yintercept=0, linetype=3, color="red", alpha=0.3)
  geom_hline(yintercept=0, linetype=2, alpha=0.3)

dev.off()
