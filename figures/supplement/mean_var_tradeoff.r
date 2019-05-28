library(dplyr)
library(ggplot2)
library(ggsidekick)
library(ggrepel)
# created by /figures/summarize_results.R
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

g = group_by(dat, species, model) %>% 
  summarize(m = mean(c(pe_2011, pe_2012, pe_2013, pe_2014, pe_2015)), 
    s = sd(c(pe_2011, pe_2012, pe_2013, pe_2014, pe_2015)))
g$species <- as.character(g$species)
g$species[g$species=="Brown cat shark"] = "Brown catshark"
sp.labs <- names(table(g$species))
sp.labs[3] = "Brown catshark"
g$species <- factor(g$species, levels=sp.labs)

# create RMSE isolines
rmse <- seq(0.03,0.5,length.out=7)
bias <- seq(0.005,0.2,length.out=100)
res.colnames <- c("rmse","bias","sd")
df.iso = data.frame(matrix(ncol = length(res.colnames), nrow = 0))
colnames(df.iso) <- res.colnames
for(i in 1:length(rmse)){
  df.iso <- rbind(df.iso, data.frame(rmse=rmse[i], bias=bias, sd=sqrt(rmse[i]^2 - bias^2)))
}

# png(filename="figures/supplement/fig7_tradeoffs_v2.png", width=7,height=7,units="in",res=300)
# png(filename="revision/fig9_tradeoffs_r1.png", width=7,height=7,units="in",res=600)
cairo_pdf(filename="revision/fig9_tradeoffs_r2.pdf", width=6.7,height=6.7,fallback_resolution=600)
# filter out species with tradeoffs
dat.plot <- group_by(g, species) %>%
  mutate(keepm = ifelse(s[model=="RF"] < s[model=="Ratio"] & m[model=="RF"] < m[model=="Ratio"], 1, 0)) %>%
  filter(keepm == 1) %>% select(-keepm)

ggplot(g, aes(x=abs(m), y=s)) +
  xlab("Absolute bias of bycatch estimates") + ylab("Standard deviation of bycatch estimates") + 
  scale_fill_manual(name = "Model", values=c("grey","#56B4E9")) + 
  # coord_cartesian(xlim = c(0,0.5), ylim=c(0.,0.5)) +
  scale_y_continuous(expand=c(0,0), limits=c(0,0.5)) +
  scale_x_continuous(expand=c(0,0), limits=c(0,0.2)) +
  geom_line(data=df.iso, aes(x=bias,y=sd,group=rmse), linetype=2, colour="grey") +
  geom_line(aes(group=species), color="grey70") + 
  # geom_point(size=4, alpha=0.4) +
  # geom_point(size=4) + 
  stat_summary(aes(fill=model),
             fun.y = median,
             colour="black", pch=21, size=4,
             geom = "point") +   
  geom_text_repel(aes(label=species), size=3, show.legend = FALSE) +
  theme_sleek() +
  theme(plot.title = element_text(size = 16),
    axis.text.x= element_text(size = 12), # tick labels
    axis.text.y=element_text(size = 12),
    axis.title=element_text(size=16), # axes labels
    panel.border = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.text=element_text(size=16), 
    legend.title=element_blank(), 
    legend.position = c(0.85, 0.9),
    legend.key.size = unit(1.2, 'lines'))
dev.off()

