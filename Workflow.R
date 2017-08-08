# Simplify the testing by using a fixed mesh placement (obtained from 2-year data) 
# then estimate nodal values using 2-year data (in sliding-window fasion),
# comparing with density obtained pixel-based conterpart (regular grid of the same size of mesh), 
# as well as beat-wise/district-wise long-term and short-term averages

setwd("/Users/xiaomuliu/CrimeProject/RegionEval/")
# Load daily crime data
source("LoadDailyCrimeData.R")
# Detrending
source("Detrend.R")
## Load weather data
source("LoadWeatherData.R")

## Detrended residual prediction
source("EvalConfig.R")
source("MovingWinEval.R")

#source("TimeSeriesEval.R")

# Allocate daily predications to mesh nodes

source("SetupGrid.R")

## Crime density and its corresponding mesh
source("IMfunction.R")
source("MeshGenFunction.R")
source("PixGenFunction.R")
source("MeshReconFunction.R")
source("PixReconFunction.R")

source("MeshConfig.R")

source("PredRateMap.R")

## Assessment
# Integrate (sum) the rate inside a police beat/district, which give the expected number of crime in that beat/district for that day
# Then get the actual count, evaluate MSE, ...  Compare with the prediction (a) using the long-term count mean;
# (b) using the short-term count mean in that beat/district
source("SetupEvalPanelData.R")
# Baseline methods: 
source("Baseline.R")

source("EvalPredResults.R")
source("PlotEval.R")
