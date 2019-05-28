# Brian Stock
# Sept 3 2018
# Figure of covariate relationships for Ratio, GAM, RF (by species)

# source("/home/brian/Documents/Bycatch/fleetwide/analysis/8_alldat.R")

library(tidyverse)
library(visreg)
library(randomForest)
library(mgcv)
library(forestFloor)
require(gridExtra)

setwd("/home/brian/Documents/Bycatch/fleetwide")
dat = readRDS("wcgop data/filtered_data_byhaul.rds")
spp = readRDS("wcgop data/spp.rds")
names(dat) = tolower(names(dat))
spp <- tolower(spp)
dat$year = as.factor(dat$year)
# dat$depth_interval = as.factor(dat$depth_interval)
dat$depth_interval = factor(dat$depth_interval,levels(dat$depth_interval)[c(2,3,1)])

# # loop over species
# for(i in 1:length(spp)){
i=1	
	# load output
	dat$response <- dat[,spp[i]]
	mod.rf = readRDS(paste0("revision/results/alldat_",spp[i],"_RF.rds"))
	mod.gam = readRDS(paste0("revision/results/alldat_",spp[i],"_GAM.rds"))
	mod.ratio = readRDS(paste0("revision/results/alldat_",spp[i],"_ratio.rds"))

	# # put depth_interval in order
	# dat$depth_interval = factor(dat$depth_interval,levels(dat$depth_interval)[c(2,3,1)])

broom::tidy(mod.gam, parametric=TRUE) %>% mutate(signif = gtools::stars.pval(p.value))

	dev.new(units='in',width=12,height=3.2)
	layout(matrix(1:5, nrow = 1))
	visreg(mod.gam, c("year","season","bimonth","depth_interval","logret"),partial=F, rug=F, ylim=c(-7,4))
	# visreg(mod.gam, c("year","season","bimonth","depth_interval","logret"),partial=F, rug=F, ylim=range(dat$response), scale='response')

# g <- list(
#   visreg(mod.gam, "year",partial=F, rug=F, gg=T) + coord_cartesian(ylim=c(-6,3)) + theme_classic(),
#   visreg(mod.gam, "season",partial=F, rug=F, gg=T) + coord_cartesian(ylim=c(-6,3)) + theme_classic(),
#   visreg(mod.gam, "bimonth",partial=F, rug=F, gg=T) + coord_cartesian(ylim=c(-6,3)) + theme_classic(),
#   visreg(mod.gam, "depth_interval",partial=F, rug=F,gg=T) + coord_cartesian(ylim=c(-6,3)) + theme_classic(),
#   visreg(mod.gam, "logret",partial=F, rug=F, gg=T) + coord_cartesian(ylim=c(-6,3)) + theme_classic(),
#   visreg2d(mod.gam, "avg_long", "avg_lat", plot.type='gg') + coord_fixed() + xlab("UTM Easting") + ylab("UTM Northing") + theme_classic() + theme(legend.title=element_blank()))
# marrangeGrob(g, nrow=1, ncol=6, top='') #, widths=rep(2.5,6), heights=rep(3,6))

# don't bother keeping y-axis limits the same for each covariate
g <- list(
  visreg(mod.gam, "year",partial=F, rug=F, gg=T) + theme_classic(),
  visreg(mod.gam, "season",partial=F, rug=F, gg=T) + theme_classic(),
  visreg(mod.gam, "bimonth",partial=F, rug=F, gg=T) + theme_classic(),
  visreg(mod.gam, "depth_interval",partial=F, rug=F,gg=T) + theme_classic(),
  visreg(mod.gam, "logret",partial=F, rug=F, gg=T) + theme_classic(),
  visreg2d(mod.gam, "avg_long", "avg_lat", plot.type='gg') + coord_fixed() + xlab("UTM Easting") + ylab("UTM Northing") + theme_classic() + theme(legend.title=element_blank()))
marrangeGrob(g, nrow=1, ncol=6, top='') #, widths=rep(2.5,6), heights=rep(3,6))

# # random forest with visreg
# g <- list(
#   visreg(mod.rf, "year",partial=F, rug=F, gg=T) + theme_classic(),
#   visreg(mod.rf, "season",partial=F, rug=F, gg=T) + theme_classic(),
#   visreg(mod.rf, "bimonth",partial=F, rug=F, gg=T) + theme_classic(),
#   visreg(mod.rf, "depth_interval",partial=F, rug=F, gg=T) + theme_classic(),
#   visreg(mod.rf, "logret",partial=F, rug=F, gg=T) + theme_classic(),
#   visreg2d(mod.rf, "avg_long", "avg_lat", trans=log, plot.type='gg') + coord_fixed() + xlab("UTM Easting") + ylab("UTM Northing") + theme_classic() + theme(legend.title=element_blank()))
# marrangeGrob(g, nrow=1, ncol=6, top='') #, widths=rep(2.5,6), heights=rep(3,6))

# random forest with forestFloor + custom plot function
covar <- c("year", "season", "bimonth", "depth_interval", "logret", "avg_lat", "avg_long")
ff = forestFloor(rf.fit = mod.rf, X = dat[,covar])
g <- list(
  ff_plot_factor(dat,mod.rf,ff,x="year",covar),
  ff_plot_factor(dat,mod.rf,ff,x="season",covar),
  ff_plot_cont(dat,mod.rf,ff,thecov="bimonth",covar),
  ff_plot_factor(dat,mod.rf,ff,x="depth_interval",covar),
  ff_plot_cont(dat,mod.rf,ff,thecov="logret",covar),
  visreg2d(mod.rf, "avg_long", "avg_lat", trans=log, plot.type='gg') + coord_fixed() + xlab("UTM Easting") + ylab("UTM Northing") + theme_classic() + theme(legend.title=element_blank()))
marrangeGrob(g, nrow=1, ncol=6, top='') #, widths=rep(2.5,6), heights=rep(3,6))

	visreg(mod.gam,"depth_interval",partial=F,rug=F)
	v = visreg(mod.gam,"depth_interval",by="year",plot=F,partial=F,rug=F)
	v1=subset(v,year %in% 2011:2013)
	plot(v1, layout=c(3,1), partial=F,rug=F)

	visreg(mod.gam,"depth_interval",by="year",scale="response",partial=F,rug=F,layout=c(5,1))
	dev.new()
	visreg(mod.rf,"depth_interval",by="year",partial=F,rug=F,layout=c(5,1))

	visreg(mod.gam,"bimonth2",partial=F,rug=F,scale="response")
	dev.new()
	visreg(mod.rf,"bimonth2",partial=F,rug=F)

	dev.new(units='in',width=3.861540,height=8.547594)
	visreg2d(mod.gam, "avg_long", "avg_lat", asp=1, zlim=c(-16,11))
	dev.new(units='in',width=3.861540,height=8.547594)
	visreg2d(mod.rf, "avg_long", "avg_lat", asp=1, trans=log, zlim=c(-16,11))

# }

dev.new(units='in',width=3.861540,height=8.547594)
visreg2d(mod.rf.norep, "avg_long", "avg_lat", asp=1, trans=log, zlim=c(-16,11))
dev.new(units='in',width=3.861540,height=8.547594)
visreg2d(mod.rf, "avg_long", "avg_lat", asp=1, trans=log, zlim=c(-16,11))

