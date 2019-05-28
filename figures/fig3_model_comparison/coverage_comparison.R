library(dplyr)
library(ggplot2)
library(ggsidekick)

dat = readRDS("results_summary.rds")

# for now we'll only plot the simulations with 20% coverage
#dat = filter(dat, pct_trips==0.2)

# facet by species. model options are including space, effort, both, neither (color)
dat$Covariate = paste(dat$spatial, dat$effort)
dat$Covariate[which(dat$Covariate==" ")] = "Neither"
dat$Covariate[which(dat$Covariate=="SP EFF")] = "Space, Effort"
dat$Covariate[which(dat$Covariate=="SP ")] = "Space"
dat$Covariate[which(dat$Covariate==" EFF")] = "Effort"

# filter out a handful of models
dat = filter(dat, Covariate == "Neither")

pdf("figures/coverage_comparison.pdf")

# for purposes of visualizing violins, turn observations outside 99% CIs to NAs
filter_dat = group_by(dat, species, model, Covariate, pct_trips) %>% 
  mutate(rmse = ifelse(rmse > 100, NA, rmse)) %>% 
  ungroup() %>% 
  group_by(species) %>% 
  mutate(max_rmse = max(rmse,na.rm=T))

filter_dat$pct_trips = as.factor(filter_dat$pct_trips)
filter_dat = rename(filter_dat, Coverage = pct_trips)
group_by(filter_dat, species, model, Covariate) %>%
  ggplot(aes(model, rmse, fill = Coverage, group = factor(paste0(model, Coverage)))) + 
  geom_boxplot(outlier.shape = NA) + facet_wrap(~ species, scale="free_y") + 
  theme_sleek() + xlab("Model") + ylab("RMSE") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + coord_cartesian() +
  scale_fill_manual(values = c("deepskyblue2","deepskyblue4"))

dev.off()
