# ---------------------------------------------------------------
# Figure with example covariate effect plots for Pacific Hake (i=8)
# save into /home/brian/Documents/Bycatch/fleetwide/figures/fig_marginal_effects/hake_ex_plots

# source("/home/brian/Documents/Bycatch/fleetwide/figures/fig_marginal_effects/fig_marginal_effects_hake.R")

library(tidyverse)
library(visreg)
library(randomForest)
library(forestFloor)
library(mgcv)
source("/home/brian/Documents/Bycatch/fleetwide/figures/fig_marginal_effects/ff_plot_fns_hake.R")

# load data
setwd("/home/brian/Documents/Bycatch/fleetwide")
dat = readRDS("wcgop data/filtered_data_byhaul.rds")
spp = readRDS("wcgop data/spp.rds")
names(dat) = tolower(names(dat))
spp <- tolower(spp)
dat$year = as.factor(dat$year)
dat$depth_interval = factor(dat$depth_interval,levels(dat$depth_interval)[c(2,3,1)])
i=8 # pacific hake
dat$response <- dat[,spp[i]]

# load stratified models
rf.strat = readRDS(paste0("revision/results/alldat_",spp[i],"_RF.rds"))
gam.strat = readRDS(paste0("revision/results/alldat_",spp[i],"_GAM.rds"))

# load nonlinear models
rf.nonlinear = readRDS(paste0("revision/results/alldat_",spp[i],"_RF_nofac.rds"))
gam.nonlinear = readRDS(paste0("revision/results/alldat_",spp[i],"_GAM_nofac.rds"))

# GAM strat
covar <- c("year", "season","bimonth","depth_interval","logret")
for(cov in 1:length(covar)){
	png(paste0("figures/fig_marginal_effects/hake_ex_plots/gam_strat_",covar[cov],".png"), units='in',res=300,width=3,height=3)
	print(visreg(gam.strat, covar[cov],partial=F, rug=F, gg=T) + theme_classic() + ylab("") + xlab("") + theme(axis.text.x = element_blank(), axis.text.y = element_blank()))
	dev.off()
}
png(paste0("figures/fig_marginal_effects/hake_ex_plots/gam_strat_latlon.png"), units='in',res=300,width=1,height=3)
print(visreg2d(gam.strat, "avg_long", "avg_lat", plot.type='gg') + coord_fixed() + xlab("UTM Easting") + ylab("UTM Northing") + theme_classic() + ylab("") + xlab("") + theme(legend.position="none", axis.text.x = element_blank(), axis.text.y = element_blank())	)
dev.off()

# RF strat
covar <- c("year", "season", "bimonth", "depth_interval", "logret", "avg_lat", "avg_long")
ff = forestFloor(rf.fit = rf.strat, X = dat[,covar])
png(paste0("figures/fig_marginal_effects/hake_ex_plots/rf_strat_",covar[1],".png"), units='in',res=300,width=3,height=3)
print(ff_plot_factor(dat, rf.strat, ff, x="year",covar))
dev.off()
png(paste0("figures/fig_marginal_effects/hake_ex_plots/rf_strat_",covar[2],".png"), units='in',res=300,width=3,height=3)
print(ff_plot_factor(dat, rf.strat, ff, x="season",covar))
dev.off()
png(paste0("figures/fig_marginal_effects/hake_ex_plots/rf_strat_",covar[3],".png"), units='in',res=300,width=3,height=3)
print(ff_plot_cont(dat, rf.strat, ff, thecov="bimonth",covar))
dev.off()
png(paste0("figures/fig_marginal_effects/hake_ex_plots/rf_strat_",covar[4],".png"), units='in',res=300,width=3,height=3)
print(ff_plot_factor(dat, rf.strat, ff, x="depth_interval",covar))
dev.off()
png(paste0("figures/fig_marginal_effects/hake_ex_plots/rf_strat_",covar[5],".png"), units='in',res=300,width=3,height=3)
print(ff_plot_cont(dat, rf.strat, ff, thecov="logret",covar))
dev.off()
png(paste0("figures/fig_marginal_effects/hake_ex_plots/rf_strat_latlon.png"), units='in',res=300,width=1,height=3)
print(visreg2d(rf.strat, "avg_long", "avg_lat", trans=log, plot.type='gg') + coord_fixed() + xlab("UTM Easting") + ylab("UTM Northing") + theme_classic() + ylab("") + xlab("") + theme(legend.position="none", axis.text.x = element_blank(), axis.text.y = element_blank())	)
dev.off()

# load data for nonlinear models
dat = readRDS("wcgop data/filtered_data_byhaul_v2.rds")
names(dat) = tolower(names(dat))
dat$year <- as.integer(as.numeric(as.character(dat$year)))
dat$response <- dat[,spp[i]]
covar <- c("year", "julian_day", "time", "depth", "gear", "logret", "avg_lat", "avg_long")

# GAM nonlinear
for(cov in 1:(length(covar)-2)){
	png(paste0("figures/fig_marginal_effects/hake_ex_plots/gam_nonlinear_",covar[cov],".png"), units='in',res=300,width=3,height=3)
	print(visreg(gam.nonlinear, covar[cov],partial=F, rug=F, gg=T) + theme_classic() + ylab("") + xlab("") + theme(axis.text.x = element_blank(), axis.text.y = element_blank()))
	dev.off()
}
png(paste0("figures/fig_marginal_effects/hake_ex_plots/gam_nonlinear_latlon.png"), units='in',res=300,width=1,height=3)
print(visreg2d(gam.nonlinear, "avg_long", "avg_lat", plot.type='gg') + coord_fixed() + xlab("UTM Easting") + ylab("UTM Northing") + theme_classic() + ylab("") + xlab("") + theme(legend.position="none", axis.text.x = element_blank(), axis.text.y = element_blank())	)
dev.off()

# RF nonlinear
ff = forestFloor(rf.fit = rf.nonlinear, X = dat[,covar])
png(paste0("figures/fig_marginal_effects/hake_ex_plots/rf_nonlinear_",covar[1],".png"), units='in',res=300,width=3,height=3)
print(ff_plot_cont(dat, rf.nonlinear, ff, thecov="year",covar))
dev.off()
png(paste0("figures/fig_marginal_effects/hake_ex_plots/rf_nonlinear_",covar[2],".png"), units='in',res=300,width=3,height=3)
print(ff_plot_cont(dat, rf.nonlinear, ff, thecov="julian_day",covar))
dev.off()
png(paste0("figures/fig_marginal_effects/hake_ex_plots/rf_nonlinear_",covar[3],".png"), units='in',res=300,width=3,height=3)
print(ff_plot_cont(dat, rf.nonlinear, ff, thecov="time",covar))
dev.off()
png(paste0("figures/fig_marginal_effects/hake_ex_plots/rf_nonlinear_",covar[4],".png"), units='in',res=300,width=3,height=3)
print(ff_plot_cont(dat, rf.nonlinear, ff, thecov="depth",covar))
dev.off()
png(paste0("figures/fig_marginal_effects/hake_ex_plots/rf_nonlinear_",covar[5],".png"), units='in',res=300,width=3,height=3)
print(ff_plot_factor(dat, rf.nonlinear, ff, x="gear",covar))
dev.off()
png(paste0("figures/fig_marginal_effects/hake_ex_plots/rf_nonlinear_",covar[6],".png"), units='in',res=300,width=3,height=3)
print(ff_plot_cont(dat, rf.nonlinear, ff, thecov="logret",covar))
dev.off()
png(paste0("figures/fig_marginal_effects/hake_ex_plots/rf_nonlinear_latlon.png"), units='in',res=300,width=1,height=3)
print(visreg2d(rf.nonlinear, "avg_long", "avg_lat", trans=log, plot.type='gg') + coord_fixed() + xlab("UTM Easting") + ylab("UTM Northing") + theme_classic() + ylab("") + xlab("") + theme(legend.position="none", axis.text.x = element_blank(), axis.text.y = element_blank())	)
dev.off()

