# Create summary table by species
# Brian Stock
# Dec 8 2017
library(dplyr)
library(tidyr)
setwd("/home/brian/Documents/Bycatch/fleetwide")
dat = readRDS("wcgop data/filtered_data_byhaul.rds")
spp = readRDS("wcgop data/spp.rds")
names(dat) = tolower(names(dat))
spp <- tolower(spp)

# ------------------------------------------------
# by year 2011-2015
byyear.catch <- dat %>%
        gather(species, bycatch, spp) %>%
        group_by(species, year) %>%
        summarize(tot.bycatch=round(sum(bycatch)/2204.62,1), n.hauls=n(), n.pos=sum(bycatch>0)) %>%
        mutate(bycatch.rate=100*round(n.pos/n.hauls,3)) %>%
        select(species, year, tot.bycatch) %>%
        spread(year, tot.bycatch) %>%
        setNames( c("Species", rep("Catch (mt)",5)))
        #setNames( c("Species", paste0(2011:2015,"_bycatch")))

byyear.rate <- dat %>%
        gather(species, bycatch, spp) %>%
        group_by(species, year) %>%
        summarize(tot.bycatch=round(sum(bycatch)), n.hauls=n(), n.pos=sum(bycatch>0)) %>%
        mutate(bycatch.rate=100*round(n.pos/n.hauls,3)) %>%
        select(species, year, bycatch.rate) %>%
        spread(year, bycatch.rate) %>%
        setNames( c("Species", rep("% Hauls",5)))
        #setNames( c("Species", paste0(2011:2015,"_rate")))
byyear <- cbind(byyear.catch[,1:2], byyear.rate[,2], byyear.catch[,3], byyear.rate[,3], byyear.catch[,4], byyear.rate[,4], byyear.catch[,5], byyear.rate[,5], byyear.catch[,6], byyear.rate[,6])

# Redo species labels, from dat
df = readRDS("figures/results_summary.rds")
sp.labs <- levels(df$species)

byyear[,1] <- sp.labs
saveRDS(byyear, "figures/table1_summary_byspecies/byyear.rds")

# ------------------------------------------------------
# collapsed across all years 2011-2015
allyears <- dat %>%
        gather(species, bycatch, spp) %>%
        group_by(species) %>%
        summarize(tot.bycatch=round(sum(bycatch)/2204.62,1), n.hauls=n(), n.pos=sum(bycatch>0)) %>%
        mutate(bycatch.rate=100*round(n.pos/n.hauls,3)) %>%
        select(species, tot.bycatch, bycatch.rate) %>%
        setNames( c("Species", "Catch (mt)", "% Hauls"))
allyears[,1] <- sp.labs
allyears <- as.data.frame(allyears)
saveRDS(allyears, "figures/table1_summary_byspecies/allyears.rds")


