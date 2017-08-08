DailyWeatherDiff<- function(WeatherData.daily){
  # calculate one-day and two-day differences of each weather variables
  diff1 <- apply(subset(WeatherData.daily,select=-Date),2,FUN=diff,lag=1)
  diff2 <- apply(subset(WeatherData.daily,select=-Date),2,FUN=diff,lag=2)
  colnames(diff1) <- sapply(names(WeatherData.daily[,-1]),paste,'_diff1',sep='')
  colnames(diff2) <- sapply(names(WeatherData.daily[,-1]),paste,'_diff2',sep='')
  WeatherData.daily_diff <- cbind(Date=WeatherData.daily$Date[3:length(WeatherData.daily$Date)],
                                  as.data.frame(cbind(diff1[2:nrow(diff1),],diff2)))
}

WeatherFilePath <- "/Users/xiaomuliu/CrimeProject/SpatioTemporalModeling/RegionEval/Data/WeatherData/"
filename.daily <- paste0(WeatherFilePath,'WeatherData_Daily_2001-01-01_2014-12-31.csv')
WeatherData.daily <- read.csv(filename.daily)
WeatherData.daily$Date <- as.Date(WeatherData.daily$Date)
WeatherData.daily_diff <- DailyWeatherDiff(WeatherData.daily)

WeatherData.nonbuffer <- WeatherData.daily[(trendLen+1):nrow(WeatherData.daily),]
WeatherDataDiff.nonbuffer <- WeatherData.daily_diff[(trendLen-1):nrow(WeatherData.daily_diff),] 