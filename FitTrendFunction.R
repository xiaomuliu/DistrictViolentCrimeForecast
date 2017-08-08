SmoothHoliday <- function(Data){
  # soomth out the number of crimes in holidays 
  IC <- Data$INC_CNT
  Hidx <- which(Data$HOLIDAY!=0)
  
  for (i in 1:length(Hidx)){
    if(Hidx[i]==1) {
      # the first datum
      IC[Hidx[i]] <- round(1/2*(IC[Hidx[i]+1]+IC[Hidx[i]+2]))
    }
    else if(Hidx[i]==nrow(Data)){
      # the last datum
      IC[Hidx[i]] <- round(1/2*(IC[Hidx[i]-1]+IC[Hidx[i]-2]))
    }
    else if(i<length(Hidx) & Hidx[i+1]-Hidx[i]==1){
      # holidays in weekend;observations on the nearest weekday
      IC[Hidx[i]] <- round(1/2*(IC[Hidx[i]-1]+IC[Hidx[i]+2]))
    } 
    else if(i>1 & Hidx[i]-Hidx[i-1]==1){
      IC[Hidx[i]] <- round(1/2*(IC[Hidx[i]-2]+IC[Hidx[i]+1])) 
    }  
    else{
      IC[Hidx[i]] <- round(1/2*(IC[Hidx[i]-1]+IC[Hidx[i]+1]))
    }
  }
  return(IC)
}

library(MASS)
PredictTrend <- function(Data,trendLen,nlfit){
  # Predict long-term trends
  updateLen <- nrow(Data)-trendLen
  trend <- rep(NA,updateLen)
  
  if (nlfit=="WLS"){
    for (j in 1:updateLen){
      Data.sub <- Data[j:(j+trendLen-1),]
      Data.sub$TIMEIDX <- 1:trendLen
      fit.trend <- nls(INC_CNT_s~b0+b1*TIMEIDX+b2*sin(2*pi*TIMEIDX/365.25)+b3*cos(2*pi*TIMEIDX/365.25), 
                       data=Data.sub)
      trend[j] <- fitted.values(fit.trend)[trendLen] # Let the last fitted value to be prediction of the trend of next day
    }   
  }
  else if (nlfit=="IRLS"){
    for (j in 1:updateLen){
      Data.sub <- Data[j:(j+trendLen-1),]
      Data.sub$TIMEIDX <- 1:trendLen
      fit.trend <- rlm(INC_CNT_s~TIMEIDX+sin(2*pi*TIMEIDX/365.25)+cos(2*pi*TIMEIDX/365.25),
                       data=Data.sub, psi=psi.bisquare)
      trend[j] <- fitted.values(fit.trend)[trendLen] # Let the last fitted value to be prediction of the trend of next day
    }   
  }
  
  return(trend)
}