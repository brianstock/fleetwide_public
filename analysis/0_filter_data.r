library(dplyr)
library(sp)
library(PBSmapping)

# setwd("/home/brian/Documents/Bycatch/fleetwide")
d = readRDS("wcgop data/Ward_request.rds")

d = filter(d, AREA == "NORTH") %>% 
  filter(HAUL_DURATION > 0)

# Delete all pts on land
# takes 8 min to run, so save output to avoid re-running in future
if(file.exists("wcgop data/haulids_on_land.rds")){
	hauls_to_delete = readRDS("wcgop data/haulids_on_land.rds")
} else {
  # filter out pts on land
  data("nepacLLhigh")
  d$pip = 0
  #for(i in 1:length(unique(nepacLLhigh$PID))) {
  for(i in 2:2) {
    indx = which(nepacLLhigh$PID == unique(nepacLLhigh$PID)[i])
    d$pip = d$pip + point.in.polygon(d$AVG_LONG, d$AVG_LAT, nepacLLhigh$X[indx], nepacLLhigh$Y[indx])
  }
  hauls_to_delete = d$HAUL_ID[which(d$pip==1)]
  saveRDS(d$HAUL_ID[which(d$pip==1)], file="wcgop data/haulids_on_land.rds")
}
d = d[-which(d$HAUL_ID%in%hauls_to_delete),]

# convert Lat/Lon to UTM north/east
# still labeled as lat and long...
coordinates(d) = c("AVG_LONG","AVG_LAT")
proj4string(d) <- CRS("+proj=longlat +datum=WGS84")  ## for example
d = as.data.frame(spTransform(d, CRS(paste("+proj=utm +zone=10"," ellps=WGS84",sep=''))))
d$AVG_LONG = d$AVG_LONG / 1000000
d$AVG_LAT = d$AVG_LAT / 1000000

d$COMMON_NAME = as.character(d$COMMON_NAME)
d$PACFIN_SPID = as.character(d$PACFIN_SPID)

# lump grenadiers together
d$COMMON_NAME[grep("Grenadi", d$COMMON_NAME)] = "Grenadier"
d$PACFIN_SPID[which(d$COMMON_NAME=="Grenadier")] = "GRDR"

# lump long- and short-spine thornyheads together
d$COMMON_NAME[grep("Thorny", d$COMMON_NAME)] = "Thornyhead"
d$PACFIN_SPID[which(d$COMMON_NAME=="Thornyhead")] = "THDS"
d$TARGET[which(d$TARGET%in%c("SSPN","LSPN"))] = "THDS"

d$resp = d$DIS_LBS
d$resp[which(is.na(d$resp))] = 0
d$zero = ifelse(d$resp==0, "zero", "pos")
d$bin = ifelse(d$resp > 0, 1, 0)
d$pos = ifelse(d$resp > 0, d$resp, NA)

saveRDS(d, "wcgop data/filtered_data.rds")

# ----------------------------------------------------------------
# d is not by haul
# create filtered_data_byhaul.rds to collapse d by species 
# use this for Fig1 and quick data exploration)

# v2 has additional covariates saved (filtered_data_byhaul_v2.rds)

# setwd("/home/brian/Documents/Bycatch/fleetwide")
d <- readRDS("wcgop data/filtered_data.rds")

# species that are exclusively discarded
sp.dis = c("Big Skate", "Black Skate", "Brown Cat Shark", "California Slickhead", 
	"Dungeness Crab", "Grenadier", "Octopus Unid", "Pacific Hake", "Pacific Halibut", 
	"Sandpaper Skate", "Rosethorn Rockfish", "Slender Sole", "Spiny Dogfish Shark", 
	"Spotted Ratfish", "Tanneri Tanner Crab")
# species that are exclusively retained
sp.ret = c("Canary Rockfish", "Pacific Cod", 
  "Pacific Ocean Perch", "Petrale Sole", 
  "Sablefish", "Yellowtail Rockfish", 
  "darkblotched rockfish", "cowcod rockfish", 
  "Dover sole", "Thornyheads", "Sablefish")
# species that are sometimes retained, sometimes discarded
sp.both = c("Arrowtooth")

# for now, only use species that are 100% discarded
spp = sp.dis
n.spp <- length(spp)

# calculate haul level covariates
dat = group_by(d, HAUL_ID) %>% 
  summarize(avg_lat = AVG_LAT[1], avg_long = AVG_LONG[1], 
    year = DYEAR[1], depth_interval = depth_interval[1], 
    bimonth = bimonth[1], bimonth2 = bimonth[1]^2, 
    trip_id = TRIP_ID[1], season=season[1],
    haul_dur = HAUL_DURATION[1],
    ret = sum(RET_LBS, na.rm=T),
    depth = AVG_DEPTH[1],
    haul_num = HAUL_NUM[1],
    gear = GEAR[1],
    time = UP_TIME[1],
    date = as.Date(paste(UP_YEAR[1],UP_MONTH[1],UP_DAY[1],sep="-"))
    )
dat$trip = as.numeric(as.factor(dat$trip_id))
dat$logret = log(dat$ret + 1)  
dat$year = as.factor(dat$year)
dat$depth_interval = as.factor(dat$depth_interval)
dat$loghaul_dur = log(dat$haul_dur)
dat$julian_day = as.numeric(format(as.Date(dat$date,format="%Y-%m-%d"), "%j"))
dat$cosDAY <- cos(2*pi*dat$julian_day/365)
dat$sinDAY <- sin(2*pi*dat$julian_day/365)
dat$cosTIME <- cos(2*pi*dat$time/24)
dat$sinTIME <- sin(2*pi*dat$time/24)
dat$gear <- as.factor(dat$gear)

# This block removes a few 100 rows of redundant locations, associated with data entry errors
dat = dat %>% 
  mutate(coords = paste0(avg_lat, avg_long)) %>% 
  group_by(coords) %>% 
  mutate(indx = seq(1, n())) %>% 
  filter(indx==1) %>% 
  ungroup %>% 
  select(-indx,-coords)

# for each haul, get catch of each species, add as column to dat
# how do some hauls have NA for discards + retained... see cur_haul for i=1
#   seems fixed now with Eric's code block above
# in i=1000, many species missing common name but have SPID
#   don't worry about it, Jason advised current setup using 'common_name'
hauls <- unique(dat$HAUL_ID)
n.hauls <- length(hauls)
dat_bycatch <- matrix(NA, nrow=n.hauls, ncol=n.spp)
for(i in 1:n.hauls){
	cur_haul <- d[which(d$HAUL_ID==hauls[i]),] # get all species records for current haul
	for(sp in 1:n.spp){
		dat_bycatch[i,sp] <- sum(cur_haul$resp[cur_haul$COMMON_NAME==spp[sp]], na.rm=T) 
	}
}
# dat_bycatch[which(is.na(dat_bycatch))] = 0 # not necessary
dat_bycatch <- as.data.frame(dat_bycatch)
colnames(dat_bycatch) <- spp
dat <- cbind(dat, dat_bycatch)

# calculate bycatch rate by species
percent_pos <- function(x) return(sum(x>0)/length(x))
perc.pos <- round(apply(dat_bycatch,2,percent_pos),3)
write.table(perc.pos,"/home/brian/Documents/Bycatch/fleetwide/analysis/bycatch_rates_spp.txt",
	quote=FALSE, col.names=FALSE)

saveRDS(dat, "wcgop data/filtered_data_byhaul_v2.rds")
# saveRDS(dat, "wcgop data/filtered_data_byhaul.rds")
saveRDS(spp, "wcgop data/spp.rds")

# give test data to Jim for VAST code
# i=3 # species 3 catch with colname 'response'
# test_dat <- head(dat,20)
# write.csv(test_dat,file="wcgop data/test_dat.csv",quote=FALSE,row.names=FALSE)

# # give test data to predict to Jim
# dat = readRDS("/home/brian/Documents/Bycatch/fleetwide/wcgop data/filtered_data_byhaul.rds")[21:25,]
# dat[1:2,'bimonth'] = 2

# # Rename to expected format
# Data_Geostat_pred = data.frame('Lat'=dat[,'avg_lat'], 
#                           'Lon'=dat[,'avg_long'], 
#                           'AreaSwept_km2'=dat[,'haul_dur'],
#                           'Vessel'="none", 
#                           'Catch_KG'=NA, 
#                           'Year'=dat[,'year'])

# # show Data_Geostat
# head(Data_Geostat_pred)

# response = sum(resp[common_name==spp[i]], na.rm=T) 
# dat$response[which(is.na(dat$response))] = 0
