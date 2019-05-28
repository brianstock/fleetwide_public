# Built on Jim Thorson's example script for VAST
#   spatio-temporal analysis of single-species catch-rate
# Oct 25, 2017

# Need to change for real data
#  103 setwd
#  117 location of data file (CSV)
#  119 delete Data_Orig[1:5,'bimonth'] = 2
#  124 change effort to exp(logret) instead of haul_dur
#  156 need raw lat/lon, not transformed (Jim's code converts Lat/Lon to UTM)
#  184 location of cpp files (VAST folder in R library)

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
INLA:::inla.dynload.workaround() # necessary on Brian's laptop since it has old dependencies

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
# n_x = c(100, 250, 500, 1000, 2000)[1] # Number of stations
n_x = 15
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
Options =  c("SD_site_density"=1, 
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

# # Prepare the data
# ## Data-frame for catch-rate data
# Depending upon the `Data_Set` chosen, we load archived data sets that are distributed with the package. 
# Each archived data set is then reformatted to create a data-frame `Data_Geostat` with a 
# standardized set of columns. For a new data set, the user is responsible for formatting `Data_Geostat` 
# appropriately to match this format.  
# We show the first six rows of `Data_Geostat` given that Data_Set = `Data_Set`.  
Data_Orig = read.csv( "/home/brian/Documents/Bycatch/fleetwide/wcgop data/test_dat.csv" )[1:15,]
# # Edit bimonth to have more than one level
Data_Orig[1:5,'bimonth'] = 2

# Add data/locations to predict
Data_Orig$pred <- 0
Data_Pred = read.csv( "/home/brian/Documents/Bycatch/fleetwide/wcgop data/test_dat.csv" )[16:20,]
Data_Pred$pred <- 1
Data_Stack <- rbind(Data_Orig, Data_Pred)

# Rename to expected format
Data_Geostat = data.frame('Lat'=Data_Stack[,'avg_lat'], 
                          'Lon'=Data_Stack[,'avg_long'], 
                          'AreaSwept_km2'=Data_Stack[,'haul_dur'],
                          'Vessel'="none", 
                          'Catch_KG'=Data_Stack[,'response'], 
                          'Year'=Data_Stack[,'year'],
                          'pred'=Data_Stack[,'pred'])

# I imagine that `haul_dur` is in log-space, so I'm rescaling here 
Data_Geostat[,'AreaSwept_km2'] = exp(Data_Stack[,'haul_dur'])
# show Data_Geostat
head(Data_Geostat)

# Make "catchability covariate" matrix Q_ik, which is technically a cheat for 
# density covariates, e.g., depth_interval, but because we only care about predicting 
# data (not extrapolating to areas with new values of density covariates), it has the intended effect
Q_ik = NULL
# Q_ik = cbind( Q_ik, ThorsonUtilities::vector_to_design_matrix(Data_Orig[,'depth_interval'])[,-1] )
# Q_ik = cbind( Q_ik, ThorsonUtilities::vector_to_design_matrix(Data_Orig[,'bimonth'])[,-1] )

## Extrapolation grid
# We also generate the extrapolation grid appropriate for a given region.  
# ## Derived objects
# Depending on the case study, we define a `Region` used when extrapolating or plotting density estimates.  
# If its a different data set, it will define `Region="Other"`, and this is a recognized level 
# for all uses of `Region` (which attempts to define reasonable settings based on the location of sampling).  
# For example `Data_Set="Iceland_cod"` has no associated meta-data for the region, so it uses `Region="Other"`

# We aren't using a standard survey region, so use 'Other'
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
# ## Build model
# To estimate parameters, we first build a list of data-inputs used for parameter estimation.  
# `Data_Fn` has some simple checks for buggy inputs, but also please read the help file `?Data_Fn`.  
TmbData = Data_Fn("Version"=Version, "FieldConfig"=FieldConfig, "OverdispersionConfig"=OverdispersionConfig, 
                  "RhoConfig"=RhoConfig, "ObsModel"=ObsModel, 
                  "c_i"=rep(0,nrow(Data_Geostat)), 
                  "b_i"=Data_Geostat[,'Catch_KG'], # response / bycatch
                  "a_i"=Data_Geostat[,'AreaSwept_km2'], # effort -- not areaswept, but logret
                  "v_i"=as.numeric(Data_Geostat[,'Vessel'])-1, # all 'none', turns into all 0s
                  "s_i"=Data_Geostat[,'knot_i']-1, # we specify # knots above, here # datapoints < #knots, so each has own knot
                  "t_i"=Data_Geostat[,'Year'],
                  "PredTF_i"=Data_Geostat[,'pred'],
                  "a_xl"=Spatial_List$a_xl, 
                  "MeshList"=Spatial_List$MeshList, "GridList"=Spatial_List$GridList, 
                  "Method"=Spatial_List$Method, "Options"=Options, "Q_ik"=Q_ik )

# We then build the TMB object.
TmbList = Build_TMB_Fn("TmbData"=TmbData, "RunDir"=DateFile, "Version"=Version, "RhoConfig"=RhoConfig, 
                      "loc_x"=Spatial_List$loc_x, "Method"=Method, 
                      "TmbDir"="/home/brian/R/x86_64-pc-linux-gnu-library/3.4/VAST/executables")
Obj = TmbList[["Obj"]]
# Q_Config=TRUE; CovConfig=TRUE; Method="Mesh"; ConvergeTol=1; Use_REML=FALSE; loc_x=Spatial_List$loc_x; Parameters="generate"; Random="generate"; Map="generate";  DiagnosticDir=NULL; TmbDir="C:\\Users\\James.Thorson\\Desktop\\Project_git\\VAST\\inst\\executables\\"; RunDir=DateFile

# ## Estimate fixed effects and predict random effects
# Next, we use a gradient-based nonlinear minimizer to identify maximum likelihood estimates for fixed-effects
Opt = TMBhelper::Optimize( obj=Obj, lower=TmbList[["Lower"]], upper=TmbList[["Upper"]], 
      getsd=TRUE, savedir=DateFile, bias.correct=FALSE, newtonsteps=1 )

# Finally, we bundle and save output
Report = Obj$report()
Save = list("Opt"=Opt, "Report"=Report, "ParHat"=Obj$env$parList(Opt$par), "TmbData"=TmbData)
save(Save, file=file.path(DateFile,"Save.RData"))

# # Diagnostic plots
# We first apply a set of standard model diagnostics to confirm that the model is reasonable 
# ## Plot data
# It is always good practice to conduct exploratory analysis of data.  
# Here, I visualize the spatial distribution of data.  
# Spatio-temporal models involve the assumption that the probability of sampling a 
# given location is statistically independent of the probability distribution for the response at that location. 
# So if sampling "follows" changes in density, then the model is probably not appropriate!
SpatialDeltaGLMM::Plot_data_and_knots(Extrapolation_List=Extrapolation_List, Spatial_List=Spatial_List, Data_Geostat=Data_Geostat, PlotDir=file.path(DateFile) )

# ## Convergence - confirm that:
#  (1) no parameter is hitting an upper or lower bound and 
#  (2) the final gradient for each fixed-effect is close to zero. For explanation of parameters, please see `?Data_Fn`.
Opt$diagnostics[,c('Param','Lower','MLE','Upper','final_gradient')]

# ## Diagnostics for encounter-probability component
# Next, we check whether observed encounter frequencies for either low or high probability samples 
# are within the 95% predictive interval for predicted encounter probability
Enc_prob = SpatialDeltaGLMM::Check_encounter_prob( Report=Report, Data_Geostat=Data_Geostat, DirName=DateFile)

# ## Diagnostics for positive-catch-rate component
# We can visualize fit to residuals of catch-rates given encounters using a Q-Q plot.  
# A good Q-Q plot will have residuals along the one-to-one line.  
Q = SpatialDeltaGLMM::QQ_Fn( TmbData=TmbData, Report=Report, FileName_PP=file.path(DateFile,"Posterior_Predictive.jpg"),
          FileName_Phist=file.path(DateFile,"Posterior_Predictive-Histogram.jpg"), 
          FileName_QQ=file.path(DateFile,"Q-Q_plot.jpg"), FileName_Qhist=file.path(DateFile,"Q-Q_hist.jpg"))

# ## Diagnostics for plotting residuals on a map
# First define years to plot and generate plotting inputs.
# useful plots by first determining which years to plot (`Years2Include`), and labels for each plotted year (`Year_Set`)
# # Get region-specific settings for plots
MapDetails_List = SpatialDeltaGLMM::MapDetails_Fn( "Region"=Region, "NN_Extrap"=Spatial_List$PolygonList$NN_Extrap, "Extrapolation_List"=Extrapolation_List )
# Decide which years to plot                                                   
Year_Set = seq(min(Data_Geostat[,'Year']),max(Data_Geostat[,'Year']))
Years2Include = which( Year_Set %in% sort(unique(Data_Geostat[,'Year'])))
# We then plot Pearson residuals.  If there are visible patterns (areas with consistently 
# positive or negative residuals accross or within years) then this is an indication of the model 
# "overshrinking" results towards the intercept, and model results should then be treated with caution.  
SpatialDeltaGLMM:::plot_residuals(Lat_i=Data_Geostat[,'Lat'], Lon_i=Data_Geostat[,'Lon'], TmbData=TmbData, Report=Report, Q=Q, savedir=DateFile, MappingDetails=MapDetails_List[["MappingDetails"]], PlotDF=MapDetails_List[["PlotDF"]], MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName=DateFile, Year_Set=Year_Set, Years2Include=Years2Include, Rotate=MapDetails_List[["Rotate"]], Cex=MapDetails_List[["Cex"]], Legend=MapDetails_List[["Legend"]], zone=MapDetails_List[["Zone"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), cex=1.8)

# ## Model selection
# To select among models, we recommend using the Akaike Information Criterion, AIC, via `Opt$AIC=` ``r Opt$AIC``. 

# ## Direction of "geometric anisotropy"
# We can visualize which direction has faster or slower decorrelation (termed "geometric anisotropy")
SpatialDeltaGLMM::PlotAniso_Fn( FileName=file.path(DateFile,"Aniso.png"), Report=Report, TmbData=TmbData )

# ## Density surface for each year
# We can visualize many types of output from the model.  Here I only show predicted density, 
# but other options are obtained via other integers passed to `plot_set` as described in `?PlotResultsOnMap_Fn`
Dens_xt = SpatialDeltaGLMM::PlotResultsOnMap_Fn(plot_set=c(3), MappingDetails=MapDetails_List[["MappingDetails"]], Report=Report, Sdreport=Opt$SD, PlotDF=MapDetails_List[["PlotDF"]], MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName=DateFile, Year_Set=Year_Set, Years2Include=Years2Include, Rotate=MapDetails_List[["Rotate"]], Cex=MapDetails_List[["Cex"]], Legend=MapDetails_List[["Legend"]], zone=MapDetails_List[["Zone"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), cex=1.8, plot_legend_fig=FALSE)

# Extract density predictions at different locations
# This is output in UTM using zone `r Extrapolation_List$zone-ifelse(Extrapolation_List$flip_around_dateline,30,0)`
Dens_DF = cbind( "Density"=as.vector(Dens_xt), "Year"=Year_Set[col(Dens_xt)], "E_km"=Spatial_List$MeshList$loc_x[row(Dens_xt),'E_km'], "N_km"=Spatial_List$MeshList$loc_x[row(Dens_xt),'N_km'] )
head(Dens_DF)

# ## Index of abundance
# The index of abundance is generally most useful for stock assessment models.
Index = SpatialDeltaGLMM::PlotIndex_Fn( DirName=DateFile, TmbData=TmbData, Sdreport=Opt[["SD"]], Year_Set=Year_Set, Years2Include=Years2Include, use_biascorr=TRUE )
Index$Table[,c("Year","Fleet","Estimate_metric_tons","SD_log","SD_mt")]

# ## Center of gravity and range expansion/contraction
# We can detect shifts in distribution or range expansion/contraction.  
SpatialDeltaGLMM::Plot_range_shifts(Report=Report, TmbData=TmbData, Sdreport=Opt[["SD"]], Znames=colnames(TmbData$Z_xm), PlotDir=DateFile, Year_Set=Year_Set)


