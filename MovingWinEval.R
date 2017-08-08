source("HolidayChart.R")
source("VarSelFunction.R")
CrimeData.eval <- subset(CrimeData.nonbuffer,DATEOCC %in% dateSeq.eval)
CrimeData.eval$TSresPred <- rep(NA,nrow(CrimeData.eval))
CrimeData.eval$HolidayCorrection <- rep(NA,nrow(CrimeData.eval))

# match crime data and weather data by date
selectData <- VariableSet(varSet,CrimeData.nonbuffer,WeatherData.nonbuffer,WeatherDataDiff.nonbuffer,glm)
X <- selectData$X
y <- selectData$y
CrimeData.nonbuffer2 <- selectData$crimeData

for (i in 1:length(dateSeq.eval)){
  
  # pinpoint the training time range
  d <- dateSeq.eval[i]
  startDate.train <- d-HistSpan
  endDate.train <- d-1
  dateSeq.train <- seq.Date(startDate.train,endDate.train,by=1)
  
  if (Windowing){
    dateWindow <- HistDateWindows(dateSeq.train,d,windowSize=winSize,windowNum=winNum,interval=365.25,dir="backward")
    idx.tr <- CrimeData.nonbuffer2$DATEOCC %in% dateWindow$histDates
  }else{
    # use all training data
    idx.tr <- CrimeData.nonbuffer2$DATEOCC %in% dateSeq.train
  }
  
  idx.te <- CrimeData.nonbuffer2$DATEOCC %in% d 
  X.train_raw <- X[idx.tr,]
  y.train <- y[idx.tr]
  X.test_raw <- X[idx.te,]
  y.test <- y[idx.te]
  
  scaling.train <- Standardization(X.train_raw,X.train_raw,standardize,varSet,glm)    
  scaling.test <- Standardization(X.train_raw,X.test_raw,standardize,varSet,glm)
  X.train <- scaling.train$scaledData
  X.test <- scaling.test$scaledData
  scalingflag <- scaling.test$flag
  
  #   cvfit <- cv.glmnet(as.matrix(X.train),as.vector(y.train),family=glm,standardize=scalingflag,nlambda=nlambda,parallel=parallel)       
  cvfit <- cv.glmnet(as.matrix(X.train),as.vector(y.train),family=glm,standardize=scalingflag,lambda=lambdaSeq,parallel=parallel)   
  fit.lasso <- glmnet(as.matrix(X.train),as.vector(y.train),family=glm,lambda=cvfit$lambda.min,standardize=scalingflag) 
  
  y_hat.test <- predict(fit.lasso,newx=as.matrix(X.test),type="response")
  
  CrimeData.eval$TSresPred[CrimeData.eval$DATEOCC==d] <- y_hat.test
  
  # Compenstate holiday cases
  H_indicator <- CrimeData.eval$HOLIDAY[CrimeData.eval$DATEOCC==d]
  if (H_indicator != 0){
    if (H_indicator==1 & format(d,"%m-%d")!="01-01"){
      # Do not compensate when the new year observation is not on Jan 1st
      Correction <- 0
    }
    else{
      # "back-predict" past few years' holidays
      idx.holiday <- (CrimeData.nonbuffer2$HOLIDAY == H_indicator)
      X.holiday_raw <- X[idx.holiday,]
      y.holiday <- CrimeData.nonbuffer2$INC_CNT[idx.holiday]-CrimeData.nonbuffer2$TStrend[idx.holiday]
      
      scaling.holiday<- Standardization(X.train_raw,X.holiday_raw,standardize,varSet,glm)
      X.holiday <- scaling.holiday$scaledData
      
      y_hat.holiday <- predict(fit.lasso,newx=as.matrix(X.holiday),type="response")
      Correction <- mean(y.holiday-y_hat.holiday)
    }    
  }
  else{
    # non-holiday days: no correction
    Correction <- 0
  }  
  
  CrimeData.eval$HolidayCorrection[CrimeData.eval$DATEOCC==d] <- Correction
}

CrimeData.eval <- within(CrimeData.eval, TSpred <- TSresPred+TStrend+HolidayCorrection)
CrimeData.eval <- within(CrimeData.eval, TSerr <- INC_CNT-TSpred)