library(dplyr)
library(ggplot2)
library(ggsidekick)

setwd("/home/brian/Documents/Bycatch/fleetwide")

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

# add "AVERAGE" as a 'species' to put in bottom right panel
sp.labs <- levels(dat$species)
dat$species <- as.character(dat$species)
# dat.allspecies <- dat 
dat.allspecies <- dat %>% filter(model %in% c("RF","Ratio"))
dat.allspecies$species = "AVERAGE"
dat.all <- rbind(dat, dat.allspecies)
dat.all$species <- factor(dat.all$species, levels = c(sp.labs,"AVERAGE"))

# filter_dat = dat %>% 
filter_dat = dat.all %>% 
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
dat.plot$species <- factor(dat.plot$species, levels = c(sp.labs,"AVERAGE"))

# gather annual estimates
dat.plot <- as.data.frame(tidyr::gather(dat.plot, year, pe, c("pe_2011","pe_2012","pe_2013","pe_2014","pe_2015")))
dat.plot$year <- gsub("pe_", "", dat.plot$year)
dat.plot$year <- as.factor(dat.plot$year)

pdf("fig6_model_comparison_byyear/fig6_model_comparison_byyear.pdf", width=12, height=7)
print(dat.plot %>% #group_by(species, model) %>%
  ggplot(aes(x=year, y=pe, fill = model)) + 
  # ggplot(aes(model, rmse, fill = model, alpha=delta)) + 
    # geom_boxplot(outlier.shape = NA) + 
    geom_violin(draw_quantiles = c(0.5)) +
    geom_hline(aes(group = species), yintercept = 0, linetype = 2) +
    facet_wrap(~ species) + 
    theme_sleek() + 
    xlab("Year") + 
    ylab("Percent Error") + 
    # theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
          # strip.text.x = element_text(face=c(rep("plain",15),"bold"))) + 
    # strip.text.x = element_text(face="bold")) + 
    coord_cartesian(ylim = c(-1,1)) +
  scale_fill_manual(name="Model", values=c("grey","#E69F00","#56B4E9")))
  # scale_fill_manual(name="Model",breaks=c("Ratio","GAM","RF"), values=c("#E69F00","#E69F00","grey","#56B4E9","#56B4E9")) +  
  # scale_alpha_discrete(range = c(0.5, 1), guide=FALSE)
dev.off()

