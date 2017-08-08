VariableSet <- function(varSet,crimeData,weatherData,weatherDiffData,glm){
  # Match the crime and/or weather variables according to variables to be selected and dates 
  # Output
  # -X: selected input variables
  # -y: dependent variable
  
  # Truncate datasets if their data ranges don't match
  d1 <- range(crimeData$DATEOCC)
  d2 <- range(weatherData$Date)
  d3 <- range(weatherDiffData$Date)
  if (!( all(d1 %in% d2) & all(d1 %in% d3) & all(d2 %in% d3) )){
    startDate <- max(d1[1],d2[1],d3[1])
    endDate <- min(d1[2],d2[2],d3[2])
    crimeData <- subset(crimeData,DATEOCC>=startDate&DATEOCC<=endDate)
    weatherData <- subset(weatherData,Date>=startDate&Date<=endDate)
    weatherDiffData <- subset(weatherDiffData,Date>=startDate&Date<=endDate)
  }
  
  # create a dummy variable 
  # In order to use function "dummy", 
  # all possible values of the factor levels must be present.
  if (any("DOW" %in% varSet)){
    DOW <- factor(c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"),levels=c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"))
    DOW.dummy <- dummy(DOW)
    DOW.dummy <- DOW.dummy[,-1] # Sunday as the reference level
    DOW.coded <- matrix(NA,nrow=length(crimeData$DOW),ncol=ncol(DOW.dummy)) 
    for (i in 1:length(crimeData$DOW)){
      DOW.coded[i,] <- DOW.dummy[as.numeric(crimeData$DOW[i]),]
    }
    DOW.coded <- as.data.frame(DOW.coded)
    names(DOW.coded) <- c("Dummy1","Dummy2","Dummy3","Dummy4","Dummy5","Dummy6")
  }
  
  if (length(setdiff(varSet, c("weather")))==0){
    # daily weather data only
    X <- subset(weatherData,select=-Date)
    y <- crimeData$TSdetrendRes
  }
  else if (length(setdiff(varSet,c("DOW")))==0){
    # day of week only
    X <- DOW.coded
    y <- crimeData$TSdetrendRes
  }
  else if (length(setdiff(varSet, c("DOW","timelag")))==0){
    # day of week (dummy variable) + lagged variables(1,2,7)
    res <- crimeData$TSdetrendRes
    crimeData <- crimeData[8:nrow(crimeData),]
    X <- cbind(DOW.coded[8:nrow(DOW.coded),],lag7=res[1:(length(res)-7)],
               lag2=res[6:(length(res)-2)],lag1=res[7:(length(res)-1)])
    y <- crimeData$TSdetrendRes
  }
  else if (length(setdiff(varSet, c("DOW","weather")))==0){
    # day of week (dummy variable) + daily weather data
    X <- cbind(DOW.coded,subset(weatherData,select=-Date))
    y <- crimeData$TSdetrendRes
  }
  else if (length(setdiff(varSet, c("DOW","weather","weatherdiff")))==0){
    # day of week (dummy variable) + daily weather data + daily weather difference
    X <- cbind(DOW.coded,subset(weatherData,select=-Date),subset(weatherDiffData,select=-Date))
    y <- crimeData$TSdetrendRes
  }
  else if (length(setdiff(varSet, c("DOW","weather","weatherdiff","timelag")))==0){
    # day of week (dummy variable) + daily weather data + daily weather difference + lagged variables(1,2,7)   
    res <- crimeData$TSdetrendRes
    crimeData <- crimeData[8:nrow(crimeData),]
    X <- cbind(DOW.coded[8:nrow(DOW.coded),],subset(weatherData[8:nrow(weatherData),],select=-Date),
               subset(weatherDiffData[8:nrow(weatherDiffData),],select=-Date),lag7=res[1:(length(res)-7)],
               lag2=res[6:(length(res)-2)],lag1=res[7:(length(res)-1)])
    y <- crimeData$TSdetrendRes
  }
  else if (length(setdiff(varSet, c("month","DOW","weather","weatherdiff","timelag")))==0 & glm=="poisson"){
    # Month(dummy variable) + day of week (dummy variable) + daily weather data + daily weather difference + lagged variables(1,2)
    X <- cbind(timeidx=crimeData$TIMEIDX[3:nrow(crimeData)],dummy(crimeData$MONTH[3:nrow(crimeData)])[,-1],
               dummy(crimeData$DOW[3:nrow(crimeData)])[,-1],subset(weatherData[3:nrow(weatherData),],select=-Date),
               subset(weatherDiffData,select=-Date),lag2=crimeData$INC_CNT_s[1:(nrow(crimeData)-2)],
               lag1=crimeData$INC_CNT_s[2:(nrow(crimeData)-1)])
    crimeData <- crimeData[3:nrow(crimeData),]
    y <- crimeData$INC_CNT_s
  }
  return(list(X=X,y=y,crimeData=crimeData))
}


HistDateWindows <- function(ts,Date,windowSize=90,windowNum=7,interval=365.25,dir=c("backward","bidirectional","available")){
  # Pinpoint historical or available dates in date series 'ts' 
  # within a certain range ('windowSize') of the same date as in 'Date' 
  targetDates <- Date+as.integer(-(windowSize/2):(windowSize/2))
  histDates <- targetDates
  if (dir=="backward"){
    for (i in 1:windowNum){
      # histrical the same dates
      histDates <- c(histDates,targetDates-i*interval)
    }
    # remove the dates in the range of targetDates
    histDates <- histDates[!(histDates %in% targetDates)]
  }
  else if (dir=="bidirectional"){  
    for (i in 1:windowNum){
      histDates <- c(histDates,targetDates-floor(i/2)*interval,targetDates+floor(i/2)*interval)
    }
  }
  else{
    #     bound <- as.numeric(format(range(ts), "%Y"))
    #     windowNum <- diff(bound)
    windowNum <- ceiling(length(ts)/interval)
    for (i in 1:windowNum){
      histDates <- c(histDates,targetDates-i*interval,targetDates+i*interval)
    }
  }
  
  histDates <- as.Date(as.character(histDates))
  weights <- as.numeric(ts %in% histDates)
  return(dateWindows=list(weights=weights,histDates=ts[ts %in% histDates],targetDates=targetDates))
}

Standardization <- function(X1,X2,standardize=c("zscore","minmax","none"),varSet,glm=c("gaussian","poisson")){
  if (standardize=="zscore") {scalingflag=TRUE}
  else if (standardize=="minmax"){
    scalingflag=FALSE
    if (!any(c("DOW","month") %in% varSet)) {
      if(nrow(X2)>1){
        X2 <- scale(X2, center=apply(X1,2,min), scale=apply(X1,2,max)-apply(X1,2,min))
      }else{
        X2 <- (X2-apply(X1,2,min))/(apply(X1,2,max)-apply(X1,2,min))
      }
    }
    else{
      if (("DOW" %in% varSet) & !("month" %in% varSet)){ factorIdx <- 1:6 }
      else if (all(c("DOW","month") %in% varSet) & glm=="gaussian"){ factorIdx <- 1:17 }
      # poisson GLM has a timeindex variable
      else if (all(c("DOW","month") %in% varSet) & glm=="poisson"){ factorIdx <- 2:18 }
      
      ctr <- apply(X1[,-factorIdx],2,min)
      sc <- apply(X1[,-factorIdx],2,max)-apply(X1[,-factorIdx],2,min)
      if(nrow(X2)>1){
        X2[,-factorIdx] <- scale(X2[,-factorIdx],center=ctr,scale=sc)
      }else{
        X2[-factorIdx] <- (X2[-factorIdx]-ctr)/sc
      }
    }
  }
  else if (standardize=="none"){ scalingflag <- FALSE }
  return(list(flag=scalingflag,scaledData=X2))
}