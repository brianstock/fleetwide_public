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

# fit a hierarchical model in glmmTMB
#library(glmmTMB)
#fit_1 = glmmTMB(log(Bycatch.kg) ~ logret + (logret | Species), data = g.long)
#r_1 = rnorm(10000, summary(fit_1)$coefficients$cond[2,1], attr(summary(fit_1)$varcor$cond$Species, "stddev")[2])
# fit same model but with log(haul duration) as predictor
#fit_2 = glmmTMB(log(Bycatch.kg) ~ loghaul_dur + (loghaul_dur | Species), data = g.long)
#r_2 = rnorm(10000, summary(fit_2)$coefficients$cond[2,1], attr(summary(fit_2)$varcor$cond$Species, "stddev")[2])


# make a density plot in ggplot2
#df = data.frame("est" = c(r_1, r_2), "Predictor" = c(rep("Ln (target catch)", length(r_1)), rep("Ln (haul duration)", length(r_1))))
#png(filename="figures/fig2_effort_bycatch/fig2_effort_bycatch_hierarchical.png", width=7,height=7,units="in",res=300)
#ggplot(df, aes(est, fill = Predictor, color = Predictor)) + 
#  geom_density(alpha = 0.3) +
#  xlab("Effect on Ln (bycatch)") + 
#  ylab("Density") + theme_sleek() +
#  geom_vline(aes(xintercept = 0), color ="grey30", linetype=2)
#dev.off()

##### Fixed effects version

# also do the linear model -- fixed effect version
library(broom)
coef3 = g.long %>% group_by(Species) %>% do(tidy(lm(log(Bycatch.kg) ~ logret, data=.)))
coef3$Predictor = "Log(Target Catch)"
coef4 = g.long %>% group_by(Species) %>% do(tidy(lm(log(Bycatch.kg) ~ loghaul_dur, data=.)))
coef4$Predictor = "Log(Haul Duration)"
coefs = rbind(coef3, coef4) %>% filter(term != "(Intercept)") %>% 
  select(-term, -statistic, -p.value)

# png(filename="figures/fig2_effort_bycatch/fig2_effort_bycatch_caterpillar.png", width=7,height=4,units="in",res=600)
# png(filename="revision/fig2_effort_bycatch_caterpillar_r1.png", width=7,height=4,units="in",res=600)
cairo_pdf(filename="revision/fig3_effort_bycatch_caterpillar_r2.pdf", width=6.69291,height=3.82452,fallback_resolution=600)
ggplot(coefs, aes(x = Species, y = estimate)) + 
  coord_flip() + 
  geom_point(color="darkblue", size=2, alpha=0.3) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=0, colour="darkblue") + 
  geom_hline(aes(yintercept = 1), linetype = 2, alpha=0.5) + 
  facet_wrap(~ Predictor) + 
  # facet_wrap(~ Predictor, scale="free_x") + 
  xlab("Species") + 
  ylab("Estimate") + theme(axis.text=element_text(size=11),
    axis.title=element_text(size=12),
    strip.text.x = element_text(size = 12)) +
  theme_sleek()
dev.off()







###### Random effects version

library(lme4)
fit_3 = lmer(log(Bycatch.kg) ~ logret + (logret | Species), data = g.long)
fit_4 = lmer(log(Bycatch.kg) ~ loghaul_dur + (loghaul_dur | Species), data = g.long)


randoms<-ranef(fit_3, postVar = TRUE)
qq <- attr(ranef(fit_3, condVar = TRUE)[[1]], "postVar")
rand.interc<-randoms$Batch
df<-data.frame(Intercepts=randoms$Batch[,1],
  sd.interc=2*sqrt(qq[,,1:dim(qq)[3]]),
  lev.names=rownames(rand.interc))
df$lev.names<-factor(df$lev.names,levels=df$lev.names[order(df$Intercepts)])

coef3 = filter(ggCaterpillar(ranef(fit_3, condVar = TRUE))[[1]], ind != "intercept adjustment")
coef3$Predictor = "Ln (target species)"
coef4 = filter(ggCaterpillar(ranef(fit_4, condVar = TRUE))[[1]], ind != "intercept adjustment")
coef4$Predictor = "Ln (haul duration)"
coefs = rbind(coef3, coef4) %>% 
  select(-nQQ, -ind)

png(filename="figures/fig2_effort_bycatch/fig2_effort_bycatch_caterpillar.png", width=7,height=7,units="in",res=300)
ggplot(coefs, aes(x = ID, y = y)) + coord_flip() + geom_point(color="darkblue", size=2, alpha=0.3) +
  geom_errorbar(aes(ymin=y-ci, ymax=y+ci), width=0, colour="darkblue") + 
  geom_hline(aes(yintercept = 0), color = "red", linetype = 3) + 
  theme_sleek()+ 
  facet_wrap(~ Predictor) + 
  xlab("Species") + 
  ylab("Estimate")

dev.off()

# from http://stackoverflow.com/questions/13847936/in-r-plotting-random-effects-from-lmer-lme4-package-using-qqmath-or-dotplot
# re = object of class ranef.mer
ggCaterpillar <- function(re, detailedFacetLabs = TRUE) {
  f <- function(x, nm = "ranef plot") {
    pv   <- attr(x, "postVar")
    cols <- 1:(dim(pv)[1])
    se   <- unlist(lapply(cols, function(i) sqrt(pv[i, i, ])))
    ord  <- unlist(lapply(x, order)) + rep((0:(ncol(x) - 1)) * nrow(x), each=nrow(x))
    pDf  <- data.frame(y=unlist(x)[ord],
      ci=1.96*se[ord],
      nQQ=rep(stats::qnorm(stats::ppoints(nrow(x))), ncol(x)),
      ID=factor(rep(rownames(x), ncol(x))[ord], levels=rownames(x)[ord]),
      ind=gl(ncol(x), nrow(x), labels=names(x)))
    
    if(detailedFacetLabs){
      pDf$ind <- ifelse(grepl("(Intercept)", pDf$ind), "intercept adjustment", paste0("slope adj: ", pDf$ind))
    }
    
    return(pDf)
  }
  
  #   lapply(re, f) # original
  lapply(seq_along(re), function(y, n, i) { f(y[[i]], n[[i]]) }, y=re, n=names(re)) # adds plot names
}


png(filename="figures/fig2_effort_bycatch/fig2_effort_bycatch_caterpillar.png", width=7,height=7,units="in",res=300)
ggplot(coefs, aes(x = ID, y = y)) + coord_flip() + geom_point(color="darkblue", size=2, alpha=0.3) +
  geom_errorbar(aes(ymin=y-ci, ymax=y+ci), width=0, colour="darkblue") + 
  geom_hline(aes(yintercept = 0), color = "red", linetype = 3) + 
  theme_sleek()+ 
  facet_wrap(~ Predictor) + 
  xlab("Species") + 
  ylab("Estimate")

dev.off()

