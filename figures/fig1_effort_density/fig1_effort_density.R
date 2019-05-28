# Figure 1
# WCGOP effort
# KDE of haul density using KernSmooth::bkde2D

library(KernSmooth)
library(fields)
library(PBSmapping)
library(RColorBrewer)
# library(INLA)
library(sp)

# -------------------------------------------------------
# 1a WCGOP effort
# -------------------------------------------------------
setwd("/home/brian/Documents/Bycatch/fleetwide")
dat <- readRDS("wcgop data/filtered_data_byhaul.rds")

# convert to km
dat$avg_long <- dat$avg_long*1000
dat$avg_lat <- dat$avg_lat*1000

minX = min(dat$avg_long)
maxX = max(dat$avg_long) + 50 # extend to see a bit more coastline
minY = min(dat$avg_lat)
maxY = max(dat$avg_lat)
n.pts.x <- round(maxX-minX-50)*10
n.pts.y <- round(maxY-minY)*10

# re-create 'fit' if it doesn't already exist
if(!file.exists("figures/fig1_effort_density/bkde2D_fit.rds")){
   fit <- bkde2D(x=cbind(dat$avg_long, dat$avg_lat), 
   	bandwidth=c(5, 5),
   	gridsize=c(n.pts.x, n.pts.y),
   	range.x=list(c(minX,maxX-50),c(minY,maxY)), truncate=TRUE)
   saveRDS(fit, "revision/fig1_effort_density/bkde2D_fit.rds")
} else {
   fit <- readRDS("figures/fig1_effort_density/bkde2D_fit.rds")
}
# image(fit$x1, fit$x2, fit$fhat, col = spec200)
# image(fit$x1, fit$x2, log(fit$fhat), col = spec200)

source("figures/fig1_effort_density/image.scale.R")
data(nepacLL) # loads the 'nepacLL' dataset from the PBSmapping package
attr(nepacLL,"zone")="11" # tell it we're in zone 10
nepacUTM <- convUL(nepacLL) # convert lat/long to UTM (km)

rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
spec200 <- rf(200)

# png(filename="figures/fig1_effort_density/fig1_effort_density.png",width=3.2,height=7,units="in",res=600)
# # dev.new(width=3.2, height=7)
# # layout(matrix(c(1,2), nrow=1, ncol=2), widths=c(3,2.75), heights=c(7))
# plotMap(nepacUTM, xlim=c(minX,maxX),ylim=c(minY,maxY),
#    col='grey',main="",plt = c(0.03, 0.97, 0.08, 0.95),
#    cex.axis=1.5, cex.lab=1.5)
# # title("WCGOP Effort",line=1)
# rect(minX, minY, maxX, maxY, density = 20, col='grey')
# rect(minX, minY, maxX, maxY, density = 20, col='grey', angle=135)
# image(fit$x1,fit$x2,fit$fhat, col = spec200, add=T)#, add=T, breaks = seq(-0.6,1.15,length.out=201))
# lev = levels(as.factor(nepacUTM$PID))
# for(i in 1:length(lev)) {
#    indx = which(nepacUTM$PID == lev[i])
#    polygon(nepacUTM$X[indx], nepacUTM$Y[indx], col = "grey")
# }
# minP <- min(fit$fhat, na.rm=T)
# maxP <- max(fit$fhat, na.rm=T)
# f <- pretty(seq(minP,maxP,length.out=4))
# image.plot(smallplot=c(.8,.85,0.08,0.95),col=spec200,zlim=c(minP,maxP),
#    legend.only=TRUE, legend.args=list(text=expression(paste("x 10"^"-5" ~ "/ km"^"2"))),
#    axis.args=list(at=f, labels=f*100000))
# dev.off()

# setEPS()
cairo_pdf(filename="revision/fig1_effort_density_r2.pdf",width=3.34646,height=7.32,fallback_resolution=600)
# png(filename="revision/fig1_effort_density_r1.png",width=3.2,height=7,units="in",res=600)
# bitmap("revision/fig1_effort_density_r2.tiff", height=7.32, width=3.34646, units='in', type="tifflzw", res=600)
# dev.new(width=3.2, height=7)
# layout(matrix(c(1,2), nrow=1, ncol=2), widths=c(3,2.75), heights=c(7))
plotMap(nepacUTM, xlim=c(minX,maxX),ylim=c(minY,maxY),
   col='grey',main="",plt = c(0.03, 0.97, 0.08, 0.95),
   cex.axis=1.5, cex.lab=1.5, xaxt = "n", yaxt='n', ylab=expression(paste("UTM Northing (km"^"–3",")")))
# title("WCGOP Effort",line=1)
axis(1, at=c(-200,-100,0), labels=c("–200","–100","0"))
axis(2, at=c(4600,4800,5000,5200,5400), labels=c(4.6,4.8,"5.0",5.2,5.4), las=1)
rect(minX, minY, maxX, maxY, density = 20, col='grey')
rect(minX, minY, maxX, maxY, density = 20, col='grey', angle=135)
image(fit$x1,fit$x2,fit$fhat, col = spec200, add=T)#, add=T, breaks = seq(-0.6,1.15,length.out=201))
lev = levels(as.factor(nepacUTM$PID))
for(i in 1:length(lev)) {
   indx = which(nepacUTM$PID == lev[i])
   polygon(nepacUTM$X[indx], nepacUTM$Y[indx], col = "grey")
}
minP <- min(fit$fhat, na.rm=T)
maxP <- max(fit$fhat, na.rm=T)
f <- pretty(seq(minP,maxP,length.out=4))
image.plot(smallplot=c(.8,.85,0.08,0.95),col=spec200,zlim=c(minP,maxP),
   legend.only=TRUE, legend.args=list(text=expression(paste("x 10"^"–5"," km"^"–2"))),
   axis.args=list(at=f, labels=f*100000))
dev.off()

# dev.print(tiff,"/home/brian/Dropbox/bycatch/manuscript/figures/fig1_effort_catch/WCGOP_effort_spec200.tiff", compression="lzw",bg="white",res=400, height=7, width=7, units="in")

# text(cities[2,3]+5,cities[2,4]-10,"CB",cex=0.9)
# text(cities[1,3]+15,cities[1,4]+30,"EU",cex=0.9)
# text(cities[3,3]+15,cities[3,4],"NP",cex=0.9) 
# text(425,5220,"GH",cex=0.9) 
# par(mai = c(0.35,0,0.35,0.15))
# image.scale(exp(runif(10000, -0.6,1.15)), col = tim.colors(200), breaks = exp(seq(-0.6,1.15,length.out=201)), axis.pos=4)

# # ------------------------------------------------------------------
# # 1b WCGOP target catch (GFR)
# # -----------------------------------------------------------

# # fields::Tps crashes R, even on 32GB desktop
# # try sampling 1/2 of locations
# frac <- 2
# n.dat <- dim(dat)[1]
# plot.id <- sample(x=1:n.dat, size=floor(n.dat/frac), replace=F)
# plot.id <- plot.id[which(dat$GFR[plot.id]>0)]

# # # old: fit un-logged GFR
# # GFR.spline <- Tps(data.frame(dat$LONG[plot.id], dat$LAT[plot.id]), dat$GFR[plot.id])
# # new.grid <- predictSurface(GFR.spline, nx = 1000, ny = 6000)

# # better to fit spline on log(GFR)
# GFR.spline.log <- Tps(data.frame(dat$LONG[plot.id], dat$LAT[plot.id]), log(dat$GFR[plot.id]))
# new.grid.log <- predictSurface(GFR.spline.log, nx = 1000, ny = 6000)

# # the lower half of the scale is unused because it's covered by land
# # turn onLand points to NA so the scale covers full range over ocean points
# library(sp)
# # new.grid.log$onLand = matrix(0,nrow=length(new.grid.log$x),ncol=length(new.grid.log$y))
# new.grid.log$onLand = rep(0,length(new.grid.log$z))
# # new.grid.log$coords = matrix(NA,nrow=length(new.grid.log$x),ncol=length(new.grid.log$y))
# new.grid.log$coords <- as.matrix(expand.grid(new.grid.log$x, new.grid.log$y))

# polygons = unique(nepacLL$PID)
# for(i in 1:length(polygons)) {
#  indx = which(nepacLL$PID == polygons[i])
#  new.grid.log$onLand = new.grid.log$onLand + point.in.polygon(new.grid.log$coords[,1], new.grid.log$coords[,2], nepacLL$X[indx], nepacLL$Y[indx], mode.checked=FALSE)
# }
# # new.grid.log$z[which(new.grid.log$onLand > 0)] <- NA
# new.grid.log$z[which(new.grid.log$onLand > 0)] <- NA

# minP <- min(new.grid.log$z,na.rm=TRUE)
# maxP <- max(new.grid.log$z,na.rm=TRUE)

# # # old: fit Tps using all data
# # GFR.spline <- Tps(data.frame(dat$LONG, dat$LAT), dat$GFR)
# # new.grid <- predictSurface(GFR.spline, nx = 2000, ny = 12000)
# # minP <- min(new.grid$z,na.rm=TRUE)
# # maxP <- max(new.grid$z,na.rm=TRUE)

# plotMap(nepacLL, xlim=c(minX,maxX),ylim=c(minY,maxY),
#    col='grey',main="",plt = c(0, 0.97, 0.08, 0.95),
#    cex.axis=1.5, cex.lab=1.5, yaxt = "n", ylab="")
# title("WCGOP Target Density")
# rect(minX, minY, maxX, maxY, density = 20, col='grey')
# rect(minX, minY, maxX, maxY, density = 20, col='grey', angle=135)
# # axis(side=1, at=c(-125,-123,-121), labels=TRUE,cex.axis=1.5, cex.lab=1.5)

# image(new.grid.log,col=spec200,add=T, breaks = seq(minP,maxP,length.out=201))
# lev = levels(as.factor(nepacLL$PID))
# for(i in 1:length(lev)) {
#    indx = which(nepacLL$PID == lev[i])
#    polygon(nepacLL$X[indx], nepacLL$Y[indx], col = "grey")
# }
# image.plot(smallplot=c(.82,.85,0.08,0.95),col=spec200,zlim=c(round(minP,2),round(maxP,2)),legend.only=TRUE,legend.shrink=0.3)
# # dev.print(tiff,"/home/brian/Dropbox/bycatch/manuscript/figures/fig1_effort_catch/fig_1_effort_catch_WCGOP_tps.tiff", compression="lzw",bg="white",res=400, height=7, width=5.75, units="in")
# # dev.copy2pdf(file="/home/brian/Dropbox/bycatch/manuscript/figures/fig1_effort_catch/fig_1_effort_catch_WCGOP_tps.pdf", height=7, width=5.75)
# dev.print(png,"/home/brian/Dropbox/bycatch/manuscript/figures/fig1_effort_catch/fig_1_effort_catch_WCGOP_tps.png", res=400, height=7, width=5.75, units="in")

# save(list=c("fit","GFR.spline.log","new.grid.log"),file="/home/brian/Documents/Bycatch/WCGOP/output/fig1_tps.RData")

