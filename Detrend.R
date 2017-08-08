source("FitTrendFunction.R")
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