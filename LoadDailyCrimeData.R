load("MatchedViolentCrimeData_portal.RData")

# convert to daily data
CrimeData.day <- aggregate(INC_CNT~DATEOCC+YEAR+MONTH+DOW+HOLIDAY,data=CrimeData, FUN=sum, na.rm=TRUE)
CrimeData.day <- CrimeData.day[order(CrimeData.day$DATEOCC),]
CrimeData.day$DOW <- factor(CrimeData.day$DOW, levels=c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"))