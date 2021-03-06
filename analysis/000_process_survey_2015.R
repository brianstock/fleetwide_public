# Process West Coast Groundfish Trawl Survey data
# Brian Stock
# 7.6.16

# Want to get data frame where each row is a unique haul, with the following columns:
#   - HAUL_ID: haul ID as in HAUL$Trawl.Id
#   - YEAR: year
#   - DATE: HAUL$Trawl.Date
#   - TIME: HAUL$Trawl.Start.Time
#   - LAT
#   - LONG
#   - DEPTH
#   - EFFORT1: area swept
#   - EFFORT2: haul duration
#   - BOT_TEMP: bottom temperature (at gear)
#   - YEYE: yelloweye rockfish catch weight in kg (species=="Sebastes ruberrimus", 0 for all other hauls)
#   - DBRK: darkblotched rockfish catch weight in kg (species=="Sebastes crameri", 0 for all other hauls)
#   - PHLB: Pacific halibut catch weight in kg (species=="Hippoglossus stenolepis", 0 for all other hauls)
#   - SST: sea-surface temperature - have to get online

library(dplyr)
library(ncdf4)
# library(date)

setwd("/home/brian/Documents/Bycatch/fleetwide") 				# linux
HAUL <- read.csv("wcann_hauls_20150722.csv",header=TRUE)
FISH <- read.csv("wcann_fish_20150722.csv",header=TRUE)

# For some reason, the HAUL dataset is duplicated - remove duplicate rows
hauls <- unique(HAUL$Trawl.Id)
n.hauls <- length(hauls)
HAUL <- HAUL[1:n.hauls,]

# Create data frame where each row is a unique haul
#   Center/de-mean the covariates (subtract mean from each observation)
na_vec <- rep(NA,n.hauls)
zero_vec <- rep(0,n.hauls)
dat <- data.frame(HAUL_ID=na_vec,YEAR=na_vec,DATE=as.Date(na_vec),TIME=na_vec,LAT=na_vec,LONG=na_vec,DEPTH=na_vec,EFFORT1=na_vec,EFFORT2=na_vec,BOT_TEMP=na_vec,SST=na_vec,ARTH=zero_vec,DBRK=zero_vec,PHLB=zero_vec)
dat$HAUL_ID <- HAUL$Trawl.Id
dat$DATE <- as.Date(as.character(HAUL$Trawl.Date),format = "%m/%d/%Y")
# dat$DATE <- as.Date(as.character(HAUL$Trawl.Date),format = "%m/%d/%y")
dat$YEAR <- format(dat$DATE,"%Y")
dat$TIME <- as.POSIXct(as.character(HAUL$Trawl.Start.Time), format = "%m/%d/%y %H:%M", tz="US/Pacific")
attr(dat$TIME, "tzone") <- "US/Pacific"
dat$LAT <- HAUL$Best.Latitude..dd.
dat$LONG <- HAUL$Best.Longitude..dd.
dat$DEPTH <- HAUL$Best.Depth..m.
dat$EFFORT1 <- HAUL$Area.Swept.by.the.Net..hectares.
dat$EFFORT2 <- HAUL$Trawl.Duration..min.
dat$BOT_TEMP <- HAUL$Temperature.At.the.Gear..degs.C. - mean(HAUL$Temperature.At.the.Gear..degs.C.,na.rm=TRUE)
for(i in 1:n.hauls){
	cur_haul <- dplyr::filter(FISH,Trawl.Id==dat$HAUL_ID[i])
	if("Atheresthes stomias" %in% cur_haul$Species) dat$ARTH[i] <- as.numeric(filter(cur_haul,Species=="Atheresthes stomias") %>% select(Haul.Weight..kg.))
	if("Sebastes crameri" %in% cur_haul$Species) dat$DBRK[i] <- as.numeric(filter(cur_haul,Species=="Sebastes crameri") %>% select(Haul.Weight..kg.))
	if("Hippoglossus stenolepis" %in% cur_haul$Species) dat$PHLB[i] <- as.numeric(filter(cur_haul,Species=="Hippoglossus stenolepis") %>% select(Haul.Weight..kg.))
}

# Only want 2011-2013
dat$YEAR <- as.numeric(dat$YEAR)
dat <- dat %>% filter(YEAR > 2010 & YEAR < 2014)

# Read in "get_SST" function
get_SST <- function(dat){
  for(i in 1:dim(dat)[1]){ # for each row i (dim(dat)[1] gets n.rows)
    this.yr = dat$YEAR[i]
    # nc = open.ncdf(paste("/home/brian/Documents/Bycatch/WCGOP/data/sst.day.anom.",this.yr,".v2.nc",sep=""))
    nc = ncdf4::nc_open(paste("/home/brian/Documents/Bycatch/WCGOP/data/sst.day.anom.",this.yr,".v2.nc",sep=""))
    ncdates = nc$dim$time$vals 							# gets vector of dates of the current year
    ncdates = as.Date(ncdates,origin = '1800-1-1')    	# formats date vector
    date1a = which.min(abs(dat$DATE[i] - ncdates)) 		# finds the day of the calendar year for this haul (e.g. 01/04/year = 4, and 02/01/year = 32)
    all.lat = nc$dim$lat$vals 							
    lat1a = which.min(abs(dat$LAT[i] - all.lat))		# index of haul's LAT
    all.lon = nc$dim$lon$vals
    lon1a = which.min(abs(((180+dat$LONG[i])+180) - all.lon)) # index of haul's LONG
    	
    this.lon = 360+dat$LONG[i] 							# haul LONG
    this.lat = dat$LAT[i] 								# haul LAT
    lat.hi = which(all.lat > dat$LAT[i])[1] 			# index of LAT *just above* haul LAT
    lat.lo = lat.hi - 1 								# index of LAT *just below* haul LAT
    lon.hi = which(all.lon > (360+dat$LONG[i]))[1] 		# index of LONG *just above* haul LONG
    lon.lo = lon.hi - 1 								# index of LONG *just below* haul LONG

	# w00 = (all.lon[lon.hi] - this.lon)*(all.lat[lat.hi] - this.lat)
	# w10 = (this.lon - all.lon[lon.lo])*(all.lat[lat.hi] - this.lat)
	# w01 = (all.lon[lon.hi] - this.lon)*(this.lat - all.lat[lat.lo])    	
 	# w11 = (this.lon - all.lon[lon.lo])*(this.lat - all.lat[lat.lo])
     
    # get the SST anomolies from the ncdf object
    # start = X,Y,time (anom object is 3-D)
    # count = how many points to read in each dim
    # sstfield grabs the SST anomolies for all lat/long points on the date of the haul
    sstfield = ncdf4::ncvar_get(nc, "anom", start=c(1,1,date1a), count=c(length(all.lon),length(all.lat),1))
    sst00 = sstfield[lon.lo,lat.lo]
    sst01 = sstfield[lon.lo,lat.hi]        
    sst10 = sstfield[lon.hi,lat.lo]
    sst11 = sstfield[lon.hi,lat.hi]
   
    if(is.na(sst00)) sst00 = sst10
    if(is.na(sst10)) sst10 = sst00
    if(is.na(sst01)) sst01 = sst11
    if(is.na(sst11)) sst11 = sst01
    
    # This math makes sense if you draw it out (see notes)       
    # We first do linear interpolation in the x-direction. This yields
    fR1 = (all.lon[lon.hi]-this.lon)/(all.lon[lon.hi]-all.lon[lon.lo])*sst00 + (this.lon-all.lon[lon.lo])/(all.lon[lon.hi]-all.lon[lon.lo])*sst10
	fR2 = (all.lon[lon.hi]-this.lon)/(all.lon[lon.hi]-all.lon[lon.lo])*sst01 + (this.lon-all.lon[lon.lo])/(all.lon[lon.hi]-all.lon[lon.lo])*sst11
	# Next do interpolation of these values in Y-direction. This yields, 
	sst.interp = (all.lat[lat.hi]-this.lat)/(all.lat[lat.hi]-all.lat[lat.lo])*fR1 + (this.lat-all.lat[lat.lo])/(all.lat[lat.hi]-all.lat[lat.lo])*fR2
	print(paste(i,sst.interp,sep="  "))
	dat$SST[i] = sst.interp
    ncdf4::nc_close(nc)
  } # end for loop over haul points
  # print(head(dat))
  return(dat)
} # end function get_SST

dat$SST <- 0
dat <- get_SST(dat)
# delete any records where SST is NA (71 of 6453)
dat <- dat[-which(is.na(dat$SST)),]

# Check for NAs in all dat columns
col.na <- function(vec){ return(length(which(is.na(vec))))} # returns number of NAs in a vector
apply(dat,2,col.na)
# bottom/gear temp has NAs - remove them
dat <- dat[-which(is.na(dat$BOT_TEMP)),]
dat <- dat[-which(is.na(dat$ARTH)),]

# get new number of hauls
n.hauls <- dim(dat)[1]

# # Log-transform covariates on large scales (DEPTH)
# dat$logDEPTH <- log(dat$DEPTH)

# # Center/de-mean each covariate (for easier model fitting)
# demean <- function(vec){ return(vec - mean(vec))}
# dat[,c(7:11,15)] <- apply(dat[,c(7:11,15)],2,demean)

# # Add rockdist, rocksize, and inRCA covariates (from Blake)
# library(tidyr)
# rca <- read.csv("/home/brian/Documents/Bycatch/WCGOP/data/rca_boundaries.csv",header=TRUE)

# years <- sort(as.numeric(levels(as.factor(rca$Year))),decreasing=TRUE)
# get_n_bins <- function(yr) {a <- rca %>% dplyr::filter(Year==yr) %>% dplyr::select(Lat.low) %>% dim; return(a[1])}
# n.bins <- sapply(years,get_n_bins)
# LAT.bins <- NULL
# for(yr in 1:length(n.bins)){ LAT.bins <- c(LAT.bins,n.bins[yr]:1) }
# rca.new <- rca %>% mutate(LAT.bin=LAT.bins) %>% gather(Month,Close,Jan:Dec)
# close.lohi <- matrix(as.numeric(unlist(strsplit(rca.new$Close,"-"))), ncol=2, byrow=TRUE)
# rca.new <- rca.new %>% mutate(close.low=close.lohi[,1],close.high=close.lohi[,2])

# blake <- read.table("survey_points_with_attributes.txt",header=TRUE,sep=",")
# checkRCA <- dplyr::filter(blake,Fath_categ!="250+") # only could be in an RCA if depth < 250 fm
# checkRCA <- dplyr::filter(checkRCA,Year!=2002) # no RCA closures in 2002

# # test <- checkRCA[1:1000,]
# blake$Month <- format(as.Date(blake$Date),"%b")
# blake$inRCA = 0 # add "inRCA" covariate (0 if not, 1 if yes)
# blake$bin = 0
# for(j in 1:nrow(checkRCA)){
#     i <- checkRCA$Master_id[j]
#     breaks <- c(55,rca %>% dplyr::filter(Year==blake$Year[i]) %>% dplyr::select(Lat.low) %>% unlist)
#     blake$bin[i] <- cut(blake$Lat[i],breaks=breaks,labels=1:(length(breaks)-1))
#     low <- rca.new %>% dplyr::filter(Year==blake$Year[i],Month==blake$Month[i],LAT.bin==blake$bin[i]) %>% dplyr::select(close.low)
#     high <- rca.new %>% dplyr::filter(Year==blake$Year[i],Month==blake$Month[i],LAT.bin==blake$bin[i]) %>% dplyr::select(close.high)
#     if(abs(blake$Ngdc_fath[i]) < high & abs(blake$Ngdc_fath[i]) > low) blake$inRCA[i] = 1
# }
# # # Check inRCA is working
# # pos <- filter(test,inRCA==1) %>% select(Year,Month,Lat,Ngdc_fath,bin)
# # rca.new %>% filter(Year==pos$Year[i],Month==pos$Month[i],LAT.bin==pos$bin[i])

# inRCA_summary <- blake %>% group_by(Year) %>% summarise(count=n(),inRCA=sum(inRCA)) %>% mutate(propRCA=inRCA/count)

# pdf("/home/brian/Documents/Bycatch/WCGOP/figures/survey_inRCA.pdf")
# plot(inRCA_summary$Year,inRCA_summary$inRCA,type='o',xlab="Year",ylab="Number of tows in RCAs")
# dev.off()

# pdf("/home/brian/Documents/Bycatch/WCGOP/figures/survey_propRCA.pdf")
# plot(inRCA_summary$Year,inRCA_summary$propRCA,type='o',xlab="Year",ylab="Percent of tows in RCAs")
# dev.off()

# # # Make sure blake$LAT == out$LAT etc.
# # identical(blake$Lat,dat$LAT)
# # identical(blake$Long,dat$LONG)
# # identical(blake$Year,dat$YEAR)

# # Copy covariates to 'out' and save workspace
# dat$inRCA <- blake$inRCA
# dat$ROCKDIST <- blake$Rockdist_m
# dat$ROCKSIZE <- blake$Rocksizeha

# # data are loaded and "cleaned" = all original variables are good to go, no NAs (not transformed, centered)
# save.image("survey_cleaned.RData")

# # Now "process" the data - transform, center, make ready for model fitting
# # Original untransformed, uncentered data in ALL CAPS, e.g. DEPTH
# # Transformed, ready to fit data in lower case, e.g. logDEPTH, logDEPTH2
# setwd("/home/brian/Documents/Bycatch/WCGOP/data")
# load("survey_cleaned.RData")

# Log- or Sqrt-transform covariates on large scales
dat$logDEPTH <- log(dat$DEPTH)
# dat$sqrtROCKDIST <- sqrt(dat$ROCKDIST)
# dat$logROCKSIZE <- log(dat$ROCKSIZE)
dat$logEFFORT1 <- log(dat$EFFORT1)
dat$logEFFORT2 <- log(dat$EFFORT2)

# Center/de-mean each covariate (for easier model fitting)
dat$sst <- dat$SST # - min(dat$SST)
dat$bot_temp <- dat$BOT_TEMP
demean <- function(vec){ return(vec - mean(vec))}
dat[,15:19] <- apply(dat[,15:19],2,demean)

# Create squared covariates
dat$sst2 <- dat$sst^2
dat$logDEPTH2 <- dat$logDEPTH^2
dat$bot_temp2 <- dat$BOT_TEMP^2

# Get vessel from HAUL
dat$VESSEL <- NA
for(i in 1:n.hauls){
   dat$VESSEL[i] <- as.character(HAUL$Vessel[which(HAUL$Trawl.Id==dat$HAUL_ID[i])][1])
}
dat$VESSEL <- as.factor(dat$VESSEL)

# Data are ready to fit
save(dat,file="/home/brian/Documents/Bycatch/fleetwide/survey_processed.RData")

# Non-zero catch rates
length(which(dat$PHLB!=0))/n.hauls # 0.08
length(which(dat$DBRK!=0))/n.hauls # 0.18
length(which(dat$ARTH!=0))/n.hauls # 0.36
