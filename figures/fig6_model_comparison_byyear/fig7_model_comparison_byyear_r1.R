library(ggplot2)
library(ggsidekick)
library(dplyr)
# devtools::install_github("seananderson/ggsidekick")
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

# Get ratio, VAST, RF results
dat$species <- as.character(dat$species)
# dat.vast <- filter(dat, Covariate=="Space") %>% filter(model %in% c("VAST"))
dat.spatial.eff <- filter(dat, Covariate=="Space, Effort") %>% filter(model %in% c("GAM Nonlinear","RF Nonlinear"))
dat.ratio <- filter(dat, model=="Ratio")

# Create ensemble = mean(VAST,RF)
# dat.ensemble <- dat.spatial.eff
err_cols <- c("e_2011","e_2012","e_2013","e_2014","e_2015","pe_2011","pe_2012","pe_2013","pe_2014","pe_2015")
# temp_array <- abind::abind(dat.vast[, err_cols], dat.spatial.eff[, err_cols], along=3)
# dat.ensemble[, err_cols] <- apply(temp_array, 1:2, mean, na.rm=T)
# dat.ensemble$model = "Ensemble"

# dat.plot <- rbind(dat.ratio, dat.vast, dat.spatial.eff, dat.ensemble)
# dat.plot$model <- factor(dat.plot$model, levels = c("Ratio", "VAST", "RF","Ensemble"))
dat.plot <- rbind(dat.ratio, dat.spatial.eff)
dat.plot$model <- factor(dat.plot$model, levels = c("Ratio","GAM Nonlinear","RF Nonlinear"))
dat.plot$species = "AVERAGE"

# filter_dat = dat %>% 
filter_dat = dat.plot %>% 
  # group_by(species, model, Covariate) %>% 
  # mutate(pe = ifelse(pe > 10, NA, pe)) %>%
  # ungroup() %>% 
  group_by(species) %>% 
  mutate(max_rmse = max(rmse,na.rm=T))
# filter_dat$delta <- as.numeric(grepl("Delta", filter_dat$model))
# filter_dat$delta <- factor(filter_dat$delta,labels=c("Non-delta","Delta"))

# dat.spatial.eff <- filter(filter_dat, Covariate=="Space, Effort") %>% mutate(medRatio = NA) %>%
#                     filter(model %in% c("Ratio", "RF Nonlinear"))
# dat.ratio <- filter(filter_dat, model=="Ratio") %>% group_by(species) %>% mutate(medRatio = median(rmse))
# dat.plot <- rbind(dat.spatial.eff, dat.ratio)
# dat.plot$model <- factor(dat.plot$model, levels = c("Ratio", "RF Nonlinear"))
# dat.plot$species <- factor(dat.plot$species, levels = c(sp.labs,"AVERAGE"))

# gather annual estimates
dat.plot <- as.data.frame(tidyr::gather(filter_dat, year, pe, c("pe_2011","pe_2012","pe_2013","pe_2014","pe_2015")))
dat.plot$year <- gsub("pe_", "", dat.plot$year)
dat.plot$year <- as.factor(dat.plot$year)
dat.plot$pe[dat.plot$pe > 10] <- 10

cairo_pdf(filename="revision/fig7_model_comparison_byyear_r2.pdf", width=6,height=3.75,fallback_resolution=600)
# png("revision/fig7_model_comparison_byyear_r1.png", units='in',width=6, height=3.75,res=600)
print(dat.plot %>% #group_by(species, model) %>%
  ggplot(aes(x=year, y=pe, fill = model)) + 
    geom_violin(draw_quantiles = c(0.5)) +
    geom_hline(yintercept = 0, linetype = 2) +
    theme_sleek() + 
    xlab("Year") + 
    ylab("Percent Error") + 
    coord_cartesian(ylim = c(-0.75, 0.75)) +
    scale_fill_manual(name="Model", labels=c("Ratio", "GAM Nonlinear","RF Nonlinear"),
                    values=c("grey","#E69F00","#56B4E9")))
dev.off()

