# Is model performance correlated to bycatch rate?
#   x-axis: bycatch rate
#   y-axis: dRMSE (Ratio-RF)
library(tidyverse)
library(ggsidekick)
library(ggrepel)
setwd("/home/brian/Documents/Bycatch/fleetwide")
dat = readRDS("figures/results_summary.rds")

# for now we'll only plot the simulations with 20% coverage
dat$Covariate = paste(dat$spatial, dat$effort)
ratiodat = filter(dat, pct_trips==0.2, model == "Ratio")
rfdat = filter(dat, pct_trips==0.2, model == "RF", Covariate=="SP EFF")
dat = rbind(ratiodat, rfdat)

# facet by species. model options are including space, effort, both, neither (color)
dat$mean_bias = NA
dat$mean_var = NA
for(i in 1:nrow(dat)) {
  z = c( (dat$true_2011[i] - dat$est_2011[i])/dat$true_2011[i], (dat$true_2012[i] - dat$est_2012[i])/dat$true_2012[i],
    (dat$true_2013[i] - dat$est_2013[i])/dat$true_2013[i], (dat$true_2014[i] - dat$est_2014[i])/dat$true_2014[i],
    (dat$true_2015[i] - dat$est_2015[i])/dat$true_2015[i])
dat$mean_bias[i] = mean(z)
dat$mean_var[i] = sqrt(var(z))
}

group_by(dat, species, model) %>% 
  summarize(mean_rmse=mean(rmse), mean_bias=mean(-mean_bias), mean_var=mean(mean_var))

df <- group_by(dat, species, model) %>% 
  summarize(mean_rmse=mean(rmse)) %>%
  spread(model, mean_rmse) %>%
  mutate(rmse_ratio_RF = 100*(Ratio-RF)/Ratio)

# now get bycatch rate by species (avg all years)
dat = readRDS("wcgop data/filtered_data_byhaul.rds")
spp = readRDS("wcgop data/spp.rds")
names(dat) = tolower(names(dat))
spp <- tolower(spp)
allyears <- dat %>%
        gather(species, bycatch, spp) %>%
        group_by(species) %>%
        summarize(tot.bycatch=round(sum(bycatch)/2204.62,1), n.hauls=n(), n.pos=sum(bycatch>0)) %>%
        mutate(bycatch.rate=100*round(n.pos/n.hauls,3)) %>%
        select(species, tot.bycatch, bycatch.rate) %>%
        setNames( c("Species", "Catch (mt)", "pct_hauls"))
df <- as.data.frame(df)
df$pct_hauls <- as.vector(as.data.frame(allyears[,3])[,1])
df$species <- as.character(df$species)
df$species[3] <- "Brown catshark"

cairo_pdf(filename="revision/fig8_bycatch_rate_rmse_r2.pdf", width=7,height=7,fallback_resolution=600)
# png(filename="revision/fig8_bycatch_rate_rmse_r1.png", width=7,height=7,units="in",res=600)
# png(filename="figures/supplement/fig10_bycatch_rate_rmse.png", width=7,height=7,units="in",res=300)
ggplot(df, aes(x=pct_hauls, y=rmse_ratio_RF)) +
  xlab("Bycatch rate (% of hauls)") + 
  ylab(expression(paste("Improvement in RMSE  ", frac((RMSE[Ratio] - RMSE[RF]), RMSE[Ratio])))) + 
  scale_y_continuous(expand=c(0,0), limits=c(0,50)) +
  scale_x_continuous(expand=c(0,0), limits=c(0,60)) +
  geom_point() +
  geom_smooth(method=lm) +
  theme_sleek() +
  geom_text_repel(aes(label=species), size=3.5, show.legend = FALSE) +
  theme(plot.title = element_text(size = 16),
    axis.text.x= element_text(size = 12), # tick labels
    axis.text.y=element_text(size = 12),
    axis.title=element_text(size=16), # axes labels
    panel.border = element_blank(),
    axis.line = element_line(colour = "black"))
dev.off()


