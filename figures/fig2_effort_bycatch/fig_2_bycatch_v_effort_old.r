library(ggplot2)
library(ggsidekick)
library(dplyr)
# devtools::install_github("seananderson/ggsidekick")
# setwd("/home/brian/Documents/Bycatch/fleetwide")
d = readRDS("wcgop data/filtered_data.rds")

#d = dplyr::filter(d, COMMON_NAME %in% sp.dis)

# definition of bycatch in le trawl = discards. So we're trying to predict discards. 
# total retained in 
# rebuilding stocks = good example because even if fishermen want to keep them, they can't. 
g = group_by(d, species) %>%
  summarize(ret = sum(RET_LBS, na.rm=T), 
    dis = sum(DIS_LBS, na.rm=T),
    dur = max(HAUL_DURATION,na.rm=T))


# plot discards versus effort
g = group_by(d, HAUL_ID) %>%
  summarize(ret = max(RET_LBS, na.rm=T), 
    dis = max(DIS_LBS[which(species == "Darkblotched Rockfish")]),
    dur = max(HAUL_DURATION,na.rm=T))
g$ret[which(is.na(g$ret))] = 0
g$dis[which(is.na(g$dis))] = 0


g1 = ggplot(g, aes(x = log(ret), y = log(dis))) + 
      geom_point(alpha = 0.4, color = "darkblue") + 
      ggtitle("Darkblotched rockfish") + 
      xlab("Ln (retained)") + 
      ylab("Ln (discards)") + 
      geom_smooth() + 
      xlim(c(5, 9.5)) + 
      theme_sleek()
g2 = ggplot(g, aes(x = dur, y = log(dis))) + 
      geom_point(alpha = 0.4, color ="darkblue") + 
      ggtitle("Darkblotched rockfish") + 
      xlab("Duration (hours)") + 
      ylab("Ln (discards)") + 
      geom_smooth() + 
      xlim(c(0, 10)) + 
      theme_sleek()

png(filename="figures/fig2_effort_bycatch/fig2_effort_bycatch_old.png", width=7,height=7,units="in",res=300)
gridExtra::grid.arrange(g1, g2, ncol=1)
dev.off()
