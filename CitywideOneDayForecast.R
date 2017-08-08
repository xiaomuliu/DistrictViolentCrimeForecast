setwd("/Users/xiaomuliu/CrimeProject/SpatioTemporalModeling/TimeSeriesMapping/")
load("MatchedViolentCrimeData_portal.RData")

# convert to daily data
CrimeData.day <- aggregate(INC_CNT~DATEOCC+YEAR+MONTH+DOW+HOLIDAY,data=CrimeData, FUN=sum, na.rm=TRUE)
CrimeData.day <- CrimeData.day[order(CrimeData.day$DATEOCC),]
CrimeData.day$DOW <- factor(CrimeData.day$DOW, levels=c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"))

# Detrending
library(MASS)
source("TimeSeriesFunction.R")

#Smooth out holiday cases
CrimeData.day$INC_CNT_s <- SmoothHoliday(CrimeData.day)

trendLen <- 730
CrimeData.buffer <- CrimeData.day[1:trendLen,]
CrimeData.nonbuffer <- CrimeData.day[(trendLen+1):nrow(CrimeData.day),]
CrimeData.nonbuffer$TStrend <- rep(NA,nrow(CrimeData.nonbuffer))
CrimeData.nonbuffer$TSdetrendRes <- rep(NA,nrow(CrimeData.nonbuffer))

Trend <- PredictTrend(CrimeData.day,trendLen,nlfit="IRLS") 
CrimeData.nonbuffer$TStrend <- Trend
CrimeData.nonbuffer$TSdetrendRes <- CrimeData.nonbuffer$INC_CNT_s-CrimeData.nonbuffer$TStrend  

## Load weather data
source("WeatherDataFunctions.R")
WeatherFilePath <- "/Users/xiaomuliu/CrimeProject/SpatioTemporalModeling/TimeSeriesMapping/Data/WeatherData/"
startDate <- "01/01/2001"
endDate <- "12/31/2014"
filename.daily <- paste(WeatherFilePath,'WeatherData_Daily_',as.character(as.Date(startDate, "%m/%d/%Y")),
                        '_',as.character(as.Date(endDate, "%m/%d/%Y")),'.csv',sep='')
WeatherData.daily <- read.csv(filename.daily)
WeatherData.daily$Date <- as.Date(WeatherData.daily$Date)
WeatherData.daily_diff <- DailyWeatherDiff(WeatherData.daily)

WeatherData.nonbuffer <- WeatherData.daily[(trendLen+1):nrow(WeatherData.daily),]
WeatherDataDiff.nonbuffer <- WeatherData.daily_diff[(trendLen-1):nrow(WeatherData.daily_diff),] 


## Time series regression
source("HolidayChart.R")
require(glmnet)
require(dummies)
require(doMC)
registerDoMC(cores=4)

glm <- "gaussian"
varSet <- c("DOW","weather","weatherdiff","timelag")
standardize <- "minmax"
Windowing <- TRUE
# nlambda <- 20
lambdaSeq <- 2^seq(-5,0.5,by=0.5)
parallel <- TRUE

date.pred <- as.Date("2012-07-15")
Ntrain <- 365*12
winSize <- 90
winNum <- 12

CrimeData.pred <- data.frame(DATEOCC=date.pred,TStrend=NA,TSresPred=NA,HolidayCorrection=NA)
CrimeData.pred$TStrend <- CrimeData.nonbuffer$TStrend[match(date.pred,CrimeData.nonbuffer$DATEOCC)]

# match crime data and weather data by date
DateRange <- c(CrimeData.nonbuffer$DATEOCC[1],date.pred-1)

CrimeData.hist <- subset(CrimeData.nonbuffer, DATEOCC>=DateRange[1]&DATEOCC<=DateRange[2])
WeatherData.hist <- subset(WeatherData.nonbuffer, Date>=DateRange[1]&Date<=DateRange[2])
WeatherDataDiff.hist <- subset(WeatherDataDiff.nonbuffer, Date>=DateRange[1]&Date<=DateRange[2])

selectData <- VariableSet(varSet,CrimeData.hist,WeatherData.hist,WeatherDataDiff.hist,glm)
X <- selectData$X
y <- selectData$y
CrimeData.hist2 <- selectData$crimeData

# pinpoint the training time range
startDate.train <- date.pred-Ntrain
endDate.train <- date.pred-1
dateSeq.train <- seq.Date(startDate.train,endDate.train,by=1)

if (Windowing){
  dateWindow <- HistDateWindows(dateSeq.train,date.pred,windowSize=winSize,windowNum=winNum,interval=365.25,dir="backward")
  idx.tr <- CrimeData.hist2$DATEOCC %in% dateWindow$histDates
}else{
  # use all training data
  idx.tr <- CrimeData.hist2$DATEOCC %in% dateSeq.train
}

X.train_raw <- X[idx.tr,]
y.train <- y[idx.tr]

CrimeData.test <- subset(CrimeData.nonbuffer, DATEOCC>=date.pred-7&DATEOCC<=date.pred)
WeatherData.test <- subset(WeatherData.nonbuffer, Date==date.pred)
WeatherDataDiff.test <- subset(WeatherDataDiff.nonbuffer, Date==date.pred)
selectData <- VariableSet2(varSet,CrimeData.test,WeatherData.test,WeatherDataDiff.test)
X.test_raw <- selectData

scaling.train <- Standardization(X.train_raw,X.train_raw,standardize,varSet,glm)    
scaling.test <- Standardization(X.train_raw,X.test_raw,standardize,varSet,glm)
X.train <- scaling.train$scaledData
X.test <- scaling.test$scaledData
scalingflag <- scaling.test$flag

#   cvfit <- cv.glmnet(as.matrix(X.train),as.vector(y.train),family=glm,standardize=scalingflag,nlambda=nlambda,parallel=parallel)       
cvfit <- cv.glmnet(as.matrix(X.train),as.vector(y.train),family=glm,standardize=scalingflag,lambda=lambdaSeq,parallel=parallel)   
fit.lasso <- glmnet(as.matrix(X.train),as.vector(y.train),family=glm,lambda=cvfit$lambda.min,standardize=scalingflag) 

y_hat.test <- predict(fit.lasso,newx=as.matrix(X.test),type="response")    

CrimeData.pred$TSresPred[CrimeData.pred$DATEOCC==date.pred] <- y_hat.test

# Compenstate holiday cases
H_indicator<- holidays(date.pred)
if (H_indicator != 0){
  if (H_indicator==1 & format(date.pred,"%m-%d")!="01-01"){
    # Do not compensate when the new year observation is not on Jan 1st
    Correction <- 0
  }
  else{
    # "back-predict" past few years' holidays
    idx.holiday <- (CrimeData.hist2$HOLIDAY == H_indicator)
    X.holiday_raw <- X[idx.holiday,]
    y.holiday <- CrimeData.hist2$INC_CNT[idx.holiday]-CrimeData.hist2$TStrend[idx.holiday]
    
    scaling.holiday<- Standardization(X.train_raw,X.holiday_raw,standardize,varSet,glm)
    X.holiday <- scaling.holiday$scaledData
    
    y_hat.holiday <- predict(fit.lasso,newx=as.matrix(X.holiday),type="response")
    Correction <- mean(y.holiday-y_hat.holiday)
  }    
}else{
  # non-holiday days: no correction
  Correction <- 0
}  

CrimeData.pred$HolidayCorrection[CrimeData.pred$DATEOCC==date.pred] <- Correction

CrimeData.pred <- within(CrimeData.pred, TSpred <- TSresPred+TStrend+HolidayCorrection)