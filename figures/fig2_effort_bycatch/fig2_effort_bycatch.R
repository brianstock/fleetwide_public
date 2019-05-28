library(ggplot2)
library(ggsidekick)
library(dplyr)
# devtools::install_github("seananderson/ggsidekick")
setwd("/home/brian/Documents/Bycatch/fleetwide")
g = readRDS("wcgop data/filtered_data_byhaul.rds")
spp = readRDS("wcgop data/spp.rds")

# Need long format data frame with "species" as a column
g.long <- tidyr::gather(data=g,
                        key=Species,        # name of new column - what are the columns you're gathering?
                        value=Bycatch.kg,   # measurement title - what are the numbers?
                        spp,                # names of columns to gather
                        factor_key=TRUE)    # treat new key column (species) as factor
# remove 0 bycatch for lm fits, can't take log(0)
g.long[g.long$Bycatch.kg==0,"Bycatch.kg"] = NA
# remove 0 target for lm fits, can't take log(0)
g.long[g.long$ret==0,"ret"] = NA

# Redo species labels, from dat
dat = readRDS("figures/results_summary.rds")
sp.labs <- levels(dat$species)
sp.labs[3] <- "Brown catshark"
g.long$Species <- plyr::mapvalues(g.long$Species, from=levels(g.long$Species), to=sp.labs)

# -----------------------------------------------------------------------------
# multipanel plot of log-log linear regressions by species
# http://vis.supstat.com/2013/04/mathematical-annotation-in-r/
# https://stackoverflow.com/questions/17022553/adding-r2-on-graph-with-facets

# effort = target catch
# fit linear models + extract slopes
lm_eqn_target = function(df){
    m = lm(log(Bycatch.kg) ~ logret, df);
    eq <- substitute(paste(beta," = ",b), 
                     list(b = sprintf("%.2f", round(summary(m)$coefficients[2,1], digits = 2))))
    as.character(as.expression(eq));  
}
eqns <- by(g.long, g.long$Species, lm_eqn_target)
df2 <- data.frame(eq = unclass(eqns), Species = names(eqns))
# calculate the intercept for the y=x line for each species (should be mean_y - mean_x)
calc_intercept = function(df){
  # int <- mean(log(df$Bycatch.kg), na.rm=T) - mean(df$logret, na.rm=T)
  int <- mean(log(df$Bycatch.kg), na.rm=T) - mean(log(df$ret), na.rm=T)
  return(int);
}
df2$int <- by(g.long, g.long$Species, calc_intercept)

cairo_pdf(filename="revision/fig4_effort_bycatch_target_nozeros_r2.pdf", width=6.7,height=6.7,fallback_resolution=600)
# png(filename="revision/fig2_effort_bycatch_target_nozeros_r1.png", width=7,height=7,units="in",res=600)
# png(filename="figures/fig2_effort_bycatch/fig2_effort_bycatch_target_nozeros.png", width=7,height=7,units="in",res=300)
# ggplot(g.long, aes(x = logret, y = log(Bycatch.kg))) + 
ggplot(g.long, aes(x = log(ret), y = log(Bycatch.kg))) + 
      geom_point(alpha = 0.4) + 
      ggtitle("Effort = Log(Target Catch)") + 
      xlab("Log(Target Catch)") + 
      # ggtitle("Effort = Log(Target Catch + 1)") + 
      # xlab("Log(Target Catch + 1)") + 
      ylab("Log(Bycatch)") + 
      geom_smooth(method=lm) + 
      facet_wrap(~Species) +
      geom_abline(data=df2, aes(intercept = int, slope = 1), linetype=2) +
      geom_text(data = df2, aes(x = 2.35, y = 10, label = eq, family = "serif"), parse = TRUE) +
      theme_sleek()
dev.off()

# -----------------------------------------------------------------------------
# effort = haul duration
# fit linear models + extract slopes
lm_eqn_hauldur = function(df){
    m = lm(log(Bycatch.kg) ~ loghaul_dur, df);
    eq <- substitute(paste(beta," = ",b), 
                     list(b = sprintf("%.2f", round(summary(m)$coefficients[2,1], digits = 2))))
    as.character(as.expression(eq));  
}
eqns <- by(g.long, g.long$Species, lm_eqn_hauldur)
df2 <- data.frame(eq = unclass(eqns), Species = names(eqns))
# calculate the intercept for the y=x line for each species (should be mean_y - mean_x)
calc_intercept = function(df){
  int <- mean(log(df$Bycatch.kg), na.rm=T) - mean(df$loghaul_dur, na.rm=T)
  return(int);
}
df2$int <- by(g.long, g.long$Species, calc_intercept)

png(filename="revision/fig2_effort_bycatch_hauldur_r1.png", width=7,height=7,units="in",res=600)
# png(filename="figures/fig2_effort_bycatch/fig2_effort_bycatch_hauldur.png", width=7,height=7,units="in",res=300)
ggplot(g.long, aes(x = loghaul_dur, y = log(Bycatch.kg))) + 
      geom_point(alpha = 0.4) + 
      ggtitle("Effort = Log(Haul Duration)") + 
      xlab("Log(Haul Duration)") + 
      ylab("Log(Bycatch)") + 
      geom_smooth(method=lm) + 
      geom_abline(data=df2, aes(intercept = int, slope = 1), linetype=2) +
      facet_wrap(~Species) +
      geom_text(data = df2, aes(x = -2.5, y = 10, label = eq, family = "serif"), parse = TRUE) +
      theme_sleek()
dev.off()

# -----------------------------------------------------------
# Histograms of slopes using both effort metrics
df2 <- data.frame(Species = spp)
get_slope_ret = function(df){
  # m = lm(log(Bycatch.kg) ~ logret, df);
  m = lm(log(Bycatch.kg) ~ log(ret), df);
  b = summary(m)$coefficients[2,1]
  return(b);
}
df2$ret <- by(g.long, g.long$Species, get_slope_ret)

get_slope_haul = function(df){
  m = lm(log(Bycatch.kg) ~ loghaul_dur, df);
  b = summary(m)$coefficients[2,1]
  return(b);
}
df2$haul <- by(g.long, g.long$Species, get_slope_haul)

df2.long <- tidyr::gather(data=df2,
                        key=Metric,        # name of new column - what are the columns you're gathering?
                        value=b,   # measurement title - what are the numbers?
                        ret:haul,                # names of columns to gather
                        factor_key=TRUE)    # treat new key column (species) as factor
levels(df2.long$Metric) <- c("Target catch", "Haul duration")

png(filename="figures/fig2_effort_bycatch/fig2_effort_bycatch_slopes.png", width=7,height=7,units="in",res=300)
ggplot(df2.long, aes(x=b, fill=Metric)) +
      geom_histogram(binwidth=0.2, position="dodge") +
      ggtitle("") + 
      # ggtitle("Log-Log linear model slope terms") + 
      xlab("Log-log linear model slope term") + 
      ylab("Frequency") +       
      geom_vline(linetype=2, xintercept=1) +
      scale_fill_manual(name="",values=c("#636363","#bdbdbd")) + # labels=c("Target Catch","Haul Duration")
      scale_y_continuous(expand=c(0,0), limits=c(0,7.5)) +
      theme_sleek() +
      # coord_cartesian(ylim=c(0, 7.5)) +
      theme(plot.title = element_text(size = 16),
          axis.text.x= element_text(size = 14), # tick labels
          axis.text.y=element_text(size = 14),
          axis.title=element_text(size=16), # axes labels
          panel.border = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.text=element_text(size=16), 
          legend.title=element_blank(), 
          legend.position = c(0.85, 0.9),
          legend.key.size = unit(1.2, 'lines'))         # add vertical space between legend keys     
dev.off()

