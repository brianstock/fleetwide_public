# Built on Jim Thorson's example script for VAST
#   spatio-temporal analysis of single-species catch-rate
# Oct 25, 2017

# source("/home/brian/Documents/Bycatch/fleetwide/analysis/6_run_vast_delta.R")

# This tutorial will walk through a simple example of how to use `VAST` for estimating 
# single-species abundance indices, distribution shifts, and range expansion.

# # Installation
# # To install TMB on a windows machine, we need to first install Rtools
# #   https://cran.r-project.org/bin/windows/Rtools/
# # During the installation, please select the option to have Rtools included in your system path.  
# # On other operating systems, it is not necessary to install Rtools.  We then install `VAST`  
# devtools::install_github("james-thorson/VAST") 
# devtools::install_github("james-thorson/utilities")
library(TMB)
library(VAST)
library(dplyr)
# INLA:::inla.dynload.workaround() # necessary on Brian's laptop since it has old dependencies

## Further information
# If you have further questions after reading this tutorial, please explore the 
# [GitHub repo](https://github.com/james-thorson/VAST/#description) mainpage, wiki, and glossary.  
# Also please explore the R help files, e.g., `?Data_Fn` for explanation of data inputs, or 
# `?Param_Fn` for explanation of parameters.  

## Related tools
# Related tools for spatio-temporal fisheries analysis are currently housed at 
# [www.FishStats.org](http://www.FishStats.org).  These include 
#   [SpatialDeltaGLMM](https://github.com/nwfsc-assess/geostatistical_delta-GLMM/#description), 
#     a single-species antecedent of VAST, and 
# [www.FishViz.org](http://www.FishViz.org), a tool for visualizing single-species results using worldwide. 
# `VAST` and `SpatialDeltaGLMM` both use continuous integration to confirm that they give 
# identical estimates when applied to single-species data.  

## How to cite VAST
# `VAST` has involved many publications for developing individual features.  
# If using `VAST`, please browse the [GitHub list](https://github.com/james-thorson/VAST/#description-of-package) of papers

# # Settings
# First chose an example data set for this script, as archived with package
Data_Set = "Custom"

# Next use latest version for CPP code
Version = "VAST_v2_8_0"

# ## Spatial settings
# The following settings define the spatial resolution for the model, 
# and whether to use a grid or mesh approximation
Method = c("Grid", "Mesh", "Spherical_mesh")[2] # use Mesh
grid_size_km = 25
n_x = c(100, 250, 500, 1000, 2000)[4] # Number of stations
# n_x = 15
Kmeans_Config = list( "randomseed"=1, "nstart"=100, "iter.max"=1e3 )    

# ## Model settings
# The following settings define whether to include spatial and spatio-temporal variation, 
# whether its autocorrelated, and whether there's overdispersion
#
# We want a constant spatial field with year intercepts:
#   Omega1 = spatial binomial (yes)
#   Epsilon1 = spatiotemporal binomial (no)
#   Omega2 = spatial positive (yes)
#   Epsilon2 = spatiotemporal positive (no)
FieldConfig = c("Omega1"=1, "Epsilon1"=0, "Omega2"=1, "Epsilon2"=0)

# Structure on parameters among years for intercepts (Beta) and spatiotemporal effects (Epsilon)
#   0: each year as fixed effect
#   1: each year as random following IID distribution
#   2: each year as random following a random walk
#   3: constant among years (i.e. SHARED model, no intercept by year)
#   4: each year as random following AR1 process
RhoConfig = c("Beta1"=0, "Beta2"=0, "Epsilon1"=0, "Epsilon2"=0) 

# Delta1 and Delta2 control random variation in catchability among a grouping variable (tows or vessels)
#   i.e. include random effect of vessel / trip
OverdispersionConfig = c("Delta1"=0, "Delta2"=0)

# conventional delta model (could try Tweedie)
# ObsModel[2] controls link functions
#   =0 binomial: logit link, positive: log link
# ObsModel[1] controls distribution for positive component (see ?Data_Fn for options)
#   =2 gamma
ObsModel = c(2,0)  

# ## Potential outputs
# Additional post-hoc calculations
Options =  c("SD_site_density"=0, 
            "SD_site_logdensity"=0, 
            "Calculate_Range"=0, 
            "Calculate_evenness"=0, 
            "Calculate_effective_area"=0, 
            "Calculate_Cov_SE"=0, 
            'Calculate_Synchrony'=0, 
            'Calculate_Coherence'=0)

# ## Set the location for saving files and settings
setwd('/home/brian/Documents/Bycatch/fleetwide/analysis')
DateFile = paste0(getwd(),"/",Sys.Date())
dir.create(DateFile)
Record = ThorsonUtilities::bundlelist( c("Data_Set","Version","Method","grid_size_km","n_x","FieldConfig","RhoConfig","OverdispersionConfig","ObsModel","Kmeans_Config") )
save( Record, file=file.path(DateFile,"Record.RData"))
capture.output( Record, file=file.path(DateFile,"Record.txt"))

# --------------------------------------------------------------------
# get filtered data output from 0_filter_dat.r
# setwd("/home/brian/Documents/Bycatch/fleetwide")
dat = readRDS("/home/brian/Documents/Bycatch/fleetwide/wcgop data/filtered_data_byhaul.rds")
spp = readRDS("/home/brian/Documents/Bycatch/fleetwide/wcgop data/spp.rds")
# change all colnames to lower case
names(dat) = tolower(names(dat))
spp <- tolower(spp)

# get test data indices (simulate observing only 20% or 40% of trips)
trips = unique(as.numeric(as.factor(dat$trip_id)))
sampled_trips_20 = readRDS("/home/brian/Documents/Bycatch/fleetwide/sampled_trips_20.rds")
sampled_trips_40 = readRDS("/home/brian/Documents/Bycatch/fleetwide/sampled_trips_40.rds")
n_sims = 200
pct=c(0.2,0.4)

# create colnames for years: 2011_est, 2012_est, ..., 2011_true, 2012_true, ...
yrs <- names(table(dat$year))
yr.labs <- as.vector(outer(yrs, c("est","true"), FUN = "paste", sep="_"))

## testing
# i=9
# h=1
# j=1

# loop over percent observed
for(h in 1:length(pct)) {

  # loop over species
  for(i in 3:length(spp)) {
    # put current species as 'response' col in dat
    dat$response <- dat[,spp[i]]

    # results.sim = save results separately for each species x pct x sim combo
    # results = cumulative results created using rbind(results.sim) in loop
    res.colnames <- c("model","spatial","effort","sim","pct_trips","pct_hauls",
      "species","total_true","total_est",yr.labs)
    results = data.frame(matrix(ncol = length(res.colnames), nrow = 0))
    colnames(results) <- res.colnames

    for(j in 1:n_sims) {
      print(paste0("Sim: ", j))
      # collect results for this species, pct, sim 
      results.sim = data.frame(model = rep("vast",2),
                          spatial = c("yes","yes"),
                          effort = c("yes","no"),
                          sim = rep(j,2),
                          pct_trips = pct[h], # percent observed trips (how WCGOP calculates observed %)
                          pct_hauls = NA, # percent observed hauls
                          species = spp[i],
                          total_true = NA, # true total (across all years) catch of species i in unobserved trips
                          total_est = NA) # estimated total (across all years) catch of species i in unobserved trips
      tmp <- data.frame(matrix(NA, ncol = length(yr.labs), nrow = 2))
      colnames(tmp) <- yr.labs
      results.sim <- cbind(results.sim, tmp)

      # sample training / test data
      # n_trips = max(dat$trip)
      # s = sample(seq_len(n_trips), size=round(pct[h]*n_trips,0), replace=FALSE)
      if(h == 1) s = sampled_trips_20[!is.na(sampled_trips_20[,j]),j]
      if(h == 2) s = sampled_trips_40[!is.na(sampled_trips_40[,j]),j]
      training_data = dat[which(dat$trip %in% s),]
      test_data = dat[-which(dat$trip %in% s),]
      
      # results.sim$sum_all[j] = sum(dat$response)
      results.sim$total_true = sum(test_data$response)
      results.sim$pct_hauls = dim(training_data)[1]/dim(dat)[1]

      # calculate true bycatch in test_data by year
      col.2011 <- which(colnames(results.sim)=="2011_true")
      byyear <- group_by(test_data, year) %>% summarize(byyear=sum(response)) %>% pull(byyear)
      for(ii in 1:2) results.sim[ii,col.2011:(col.2011+4)] <- byyear

      # prep covariates
      training_data$year = as.factor(training_data$year)
      training_data$depth_interval = as.factor(training_data$depth_interval)
      test_data$year = as.factor(test_data$year)
      test_data$depth_interval = as.factor(test_data$depth_interval)
      col.2011 <- which(colnames(results.sim)=="2011_est")

      # verify the datasets have the same factor levels for year and depth
      same_factor = FALSE
      if(length(unique(training_data$year)) == length(unique(test_data$year)) &&
         length(unique(training_data$depth_interval[which(training_data$response>0)])) == length(unique(test_data$depth_interval[which(training_data$response>0)])) &&
         length(unique(training_data$depth_interval[which(training_data$response>0)])) == length(unique(test_data$depth_interval[which(training_data$response>0)]))) same_factor = TRUE
      
      if(same_factor) { # fit models
        training_data$predTF <- 0
        test_data$predTF <- 1
        Data_Stack = rbind(training_data, test_data)
        Data_Stack$ret_plus1 <- Data_Stack$ret + 1 # going to take log, so can't have 0s
        Data_Stack$year <- as.numeric(as.character(Data_Stack$year))

        Data_Geostat = data.frame('Lat'=Data_Stack[,'avg_lat'], 
                          'Lon'=Data_Stack[,'avg_long'], 
                          'AreaSwept_km2'=Data_Stack[,'ret_plus1'], # effort, natural scale (not log)
                          'Vessel'="none", 
                          'Catch_KG'=Data_Stack[,'response'], 
                          'Year'=Data_Stack[,'year'],
                          'predTF'=Data_Stack[,'predTF'],
                          'season'=Data_Stack[,'season'],
                          'bimonth'=Data_Stack[,'bimonth'],
                          'bimonth2'=Data_Stack[,'bimonth2'],
                          'depth_interval'=Data_Stack[,'depth_interval'])

        # setup covariates (catchability)
        Q_ik = NULL
        Q_ik = cbind( Q_ik, ThorsonUtilities::vector_to_design_matrix(Data_Stack[,'depth_interval'])[,-1] )
        Q_ik = cbind( Q_ik, Data_Stack[,'bimonth'])
        Q_ik = cbind( Q_ik, Data_Stack[,'bimonth2'])
        Q_ik = cbind( Q_ik, ThorsonUtilities::vector_to_design_matrix(Data_Stack[,'season'])[,-1] )

        Region = "Other" 
        # Define any potential stratification of results, and settings specific to any case-study data set
        # Jim's code converts Lat/Lon to UTM, so we need to feed it raw lat/lon, not transformed
        strata.limits = data.frame('STRATA'="All_areas")
        Extrapolation_List = SpatialDeltaGLMM::Prepare_Extrapolation_Data_Fn( Region=Region, 
                              strata.limits=strata.limits, 
                              observations_LL=Data_Geostat[,c('Lat','Lon')], 
                              maximum_distance_from_sample=15 ) #

        # ## Derived objects for spatio-temporal estimation
        # And we finally generate the information used for conducting spatio-temporal parameter estimation, 
        Spatial_List = SpatialDeltaGLMM::Spatial_Information_Fn( grid_size_km=grid_size_km, n_x=n_x, Method=Method, Lon=Data_Geostat[,'Lon'], Lat=Data_Geostat[,'Lat'], Extrapolation_List=Extrapolation_List, randomseed=Kmeans_Config[["randomseed"]], nstart=Kmeans_Config[["nstart"]], iter.max=Kmeans_Config[["iter.max"]], DirPath=DateFile, Save_Results=FALSE )
        # # Add knots to Data_Geostat
        Data_Geostat = cbind( Data_Geostat, "knot_i"=Spatial_List$knot_i )

        # # Build and run model 
        TmbData = Data_Fn("Version"=Version, "FieldConfig"=FieldConfig, "OverdispersionConfig"=OverdispersionConfig, 
                          "RhoConfig"=RhoConfig, "ObsModel"=ObsModel, 
                          "c_i"=rep(0,nrow(Data_Geostat)), 
                          "b_i"=Data_Geostat[,'Catch_KG'], # response / bycatch
                          "a_i"=Data_Geostat[,'AreaSwept_km2'], # effort -- not areaswept, but logret
                          "v_i"=as.numeric(Data_Geostat[,'Vessel'])-1, # all 'none', turns into all 0s
                          "s_i"=Data_Geostat[,'knot_i']-1, # we specify # knots above, here # datapoints < #knots, so each has own knot
                          "t_i"=Data_Geostat[,'Year'],
                          "PredTF_i"=Data_Geostat[,'predTF'],
                          "a_xl"=Spatial_List$a_xl, 
                          "MeshList"=Spatial_List$MeshList, "GridList"=Spatial_List$GridList, 
                          "Method"=Spatial_List$Method, "Options"=Options, "Q_ik"=Q_ik )
        TmbList = Build_TMB_Fn("TmbData"=TmbData, "RunDir"=DateFile, "Version"=Version, "RhoConfig"=RhoConfig, 
                              "loc_x"=Spatial_List$loc_x, "Method"=Method, 
                              # "TmbDir"="/home/brian/R/x86_64-pc-linux-gnu-library/3.4/VAST/executables")
                              "TmbDir"="/home/brian/R/x86_64-redhat-linux-gnu-library/3.4/VAST/executables")
        Obj = TmbList[["Obj"]]
        Opt = TMBhelper::Optimize( obj=Obj, lower=TmbList[["Lower"]], upper=TmbList[["Upper"]], 
              getsd=TRUE, savedir=DateFile, bias.correct=FALSE, newtonsteps=1 )
        Report = Obj$report()

        # get predictions
        pred.ind <- which(Data_Stack$predTF==1)
        test_data$predicted <- Report$R1_i[pred.ind] * Report$R2_i[pred.ind]
        results.sim$total_est[1] = sum(test_data$predicted)
        results.sim[1,col.2011:(col.2011+4)] <- group_by(test_data, year) %>% summarize(byyear=sum(predicted)) %>% pull(byyear)

        # ------------------------------------------------------------------
        # spatial = yes, effort = no

        # re-run setting area_swept (a_i) = 1 for all observations (i.e. no effort offset)
        TmbData = Data_Fn("Version"=Version, "FieldConfig"=FieldConfig, "OverdispersionConfig"=OverdispersionConfig, 
                          "RhoConfig"=RhoConfig, "ObsModel"=ObsModel, 
                          "c_i"=rep(0,nrow(Data_Geostat)), 
                          "b_i"=Data_Geostat[,'Catch_KG'], # response / bycatch
                          "a_i"=rep(1,nrow(Data_Geostat)), # areaswept = 1 for all i
                          "v_i"=as.numeric(Data_Geostat[,'Vessel'])-1, # all 'none', turns into all 0s
                          "s_i"=Data_Geostat[,'knot_i']-1, # we specify # knots above, here # datapoints < #knots, so each has own knot
                          "t_i"=Data_Geostat[,'Year'],
                          "PredTF_i"=Data_Geostat[,'predTF'],
                          "a_xl"=Spatial_List$a_xl, 
                          "MeshList"=Spatial_List$MeshList, "GridList"=Spatial_List$GridList, 
                          "Method"=Spatial_List$Method, "Options"=Options, "Q_ik"=Q_ik )
        TmbList = Build_TMB_Fn("TmbData"=TmbData, "RunDir"=DateFile, "Version"=Version, "RhoConfig"=RhoConfig, 
                              "loc_x"=Spatial_List$loc_x, "Method"=Method, 
                              # "TmbDir"="/home/brian/R/x86_64-pc-linux-gnu-library/3.4/VAST/executables")
                              "TmbDir"="/home/brian/R/x86_64-redhat-linux-gnu-library/3.4/VAST/executables")                              
        Obj = TmbList[["Obj"]]
        Opt = TMBhelper::Optimize( obj=Obj, lower=TmbList[["Lower"]], upper=TmbList[["Upper"]], 
              getsd=TRUE, savedir=DateFile, bias.correct=FALSE, newtonsteps=1 )
        Report = Obj$report()

        # get predictions
        pred.ind <- which(Data_Stack$predTF==1)
        test_data$predicted <- Report$R1_i[pred.ind] * Report$R2_i[pred.ind]
        results.sim$total_est[2] = sum(test_data$predicted)
        results.sim[2,col.2011:(col.2011+4)] <- group_by(test_data, year) %>% summarize(byyear=sum(predicted)) %>% pull(byyear)

      } # end if(same_factor) {
    # append results.sim to results
    results <- rbind(results, results.sim)
    
    } # end j (sim)
    
    # save output
    saveRDS(results, file = paste0("/home/brian/Documents/Bycatch/fleetwide/results/byyear_vast_",spp[i],"_",pct[h],".rds"))
    saveRDS(results, file = paste0("/home/brian/Dropbox/bycatch/fleetwide/byyear_vast_",spp[i],"_",pct[h],".rds"))
  } # end i (species)
  
} # end h (pct)

# # # Diagnostic plots
# SpatialDeltaGLMM::Plot_data_and_knots(Extrapolation_List=Extrapolation_List, Spatial_List=Spatial_List, Data_Geostat=Data_Geostat, PlotDir=file.path(DateFile) )
# Opt$diagnostics[,c('Param','Lower','MLE','Upper','final_gradient')]
# Enc_prob = SpatialDeltaGLMM::Check_encounter_prob( Report=Report, Data_Geostat=Data_Geostat, DirName=DateFile)
# Q = SpatialDeltaGLMM::QQ_Fn( TmbData=TmbData, Report=Report, FileName_PP=file.path(DateFile,"Posterior_Predictive.jpg"),
#           FileName_Phist=file.path(DateFile,"Posterior_Predictive-Histogram.jpg"), 
#           FileName_QQ=file.path(DateFile,"Q-Q_plot.jpg"), FileName_Qhist=file.path(DateFile,"Q-Q_hist.jpg"))

# # plot predicted vs observed
# Data_Stack$predicted <- Report$R1_i * Report$R2_i
# plot(log(Data_Stack$predicted+1), log(Data_Stack$response+1))
# abline(1:10,1:10)
# cor(log(Data_Stack$predicted+1), log(Data_Stack$response+1), method="pearson")

# MapDetails_List = SpatialDeltaGLMM::MapDetails_Fn( "Region"=Region, "NN_Extrap"=Spatial_List$PolygonList$NN_Extrap, "Extrapolation_List"=Extrapolation_List )
# Year_Set = seq(min(Data_Geostat[,'Year']),max(Data_Geostat[,'Year']))
# Years2Include = which( Year_Set %in% sort(unique(Data_Geostat[,'Year'])))
# SpatialDeltaGLMM:::plot_residuals(Lat_i=Data_Geostat[,'Lat'], Lon_i=Data_Geostat[,'Lon'], TmbData=TmbData, Report=Report, Q=Q, savedir=DateFile, MappingDetails=MapDetails_List[["MappingDetails"]], PlotDF=MapDetails_List[["PlotDF"]], MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName=DateFile, Year_Set=Year_Set, Years2Include=Years2Include, Rotate=MapDetails_List[["Rotate"]], Cex=MapDetails_List[["Cex"]], Legend=MapDetails_List[["Legend"]], zone=MapDetails_List[["Zone"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), cex=1.8)

# SpatialDeltaGLMM::PlotAniso_Fn( FileName=file.path(DateFile,"Aniso.png"), Report=Report, TmbData=TmbData )
# Dens_xt = SpatialDeltaGLMM::PlotResultsOnMap_Fn(plot_set=c(3), MappingDetails=MapDetails_List[["MappingDetails"]], Report=Report, Sdreport=Opt$SD, PlotDF=MapDetails_List[["PlotDF"]], MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName=DateFile, Year_Set=Year_Set, Years2Include=Years2Include, Rotate=MapDetails_List[["Rotate"]], Cex=MapDetails_List[["Cex"]], Legend=MapDetails_List[["Legend"]], zone=MapDetails_List[["Zone"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), cex=1.8, plot_legend_fig=FALSE)
# Dens_DF = cbind( "Density"=as.vector(Dens_xt), "Year"=Year_Set[col(Dens_xt)], "E_km"=Spatial_List$MeshList$loc_x[row(Dens_xt),'E_km'], "N_km"=Spatial_List$MeshList$loc_x[row(Dens_xt),'N_km'] )
# head(Dens_DF)

# Index = SpatialDeltaGLMM::PlotIndex_Fn( DirName=DateFile, TmbData=TmbData, Sdreport=Opt[["SD"]], Year_Set=Year_Set, Years2Include=Years2Include, use_biascorr=TRUE )
# Index$Table[,c("Year","Fleet","Estimate_metric_tons","SD_log","SD_mt")]

# SpatialDeltaGLMM::Plot_range_shifts(Report=Report, TmbData=TmbData, Sdreport=Opt[["SD"]], Znames=colnames(TmbData$Z_xm), PlotDir=DateFile, Year_Set=Year_Set)


