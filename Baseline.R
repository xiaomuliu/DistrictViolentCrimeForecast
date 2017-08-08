# Example:
# Tomorrow's forecast - Friday, July 12, 2016.
# 1. historical overall average number of crimes for a given District (how different is tomorrow from an average day?)
# 2. same, but for a given time of year (how different is tomorrow from an average July day?)
# 3. same, but for a given day-of-week at a given time of year (how different is tomorrow compared with an average Friday in July?)
# 4. same, but for the three most recent Fridays

# BEAT
for (i in 1:NumDays.eval){
  d <- dateSeq.eval[i]
  d.year <- as.integer(format(d,"%Y"))
  d.month <- as.integer(format(d,"%m"))
  d.dow <- weekdays(d)
  CrimeData.beat_day_long <- subset(CrimeData.beat_day,YEAR>=(d.year-5) & YEAR<d.year,select=c("DATEOCC","MONTH","BEAT","INC_CNT"))
  CrimeData.beat_day_histMonth <- subset(CrimeData.beat_day_long,MONTH==d.month,select=c("DATEOCC","BEAT","INC_CNT"))
  CrimeData.beat_day_histDOW <- subset(CrimeData.beat_day_long,MONTH==d.month & weekdays(DATEOCC)==d.dow,
                                       select=c("DATEOCC","BEAT","INC_CNT"))
  CrimeData.beat_day_recentDOW <- subset(CrimeData.beat_day,DATEOCC>=d-period.short & DATEOCC<=d-1 &  weekdays(DATEOCC)==d.dow,
                                         select=c("DATEOCC","BEAT","INC_CNT"))
  for (j in beatList){
    targetIdx <- CrimeData.eval_beat$BEAT==j&CrimeData.eval_beat$DATEOCC==d
    BeatCnt.long <- subset(CrimeData.beat_day_long,BEAT==j)
    BeatCnt.histMonth <- subset(CrimeData.beat_day_histMonth,BEAT==j)
    BeatCnt.histDOW <- subset(CrimeData.beat_day_histDOW,BEAT==j)
    BeatCnt.recentDOW <- subset(CrimeData.beat_day_recentDOW,BEAT==j)
    # Some beats may have no incident records during the long/short periods
    if (nrow(BeatCnt.long)==0){
      CrimeData.eval_beat$Pred_LT[targetIdx] <- 0
      CrimeData.eval_beat$Pred_histMonth[targetIdx] <- 0
      CrimeData.eval_beat$Pred_histDOW[targetIdx] <- 0
      CrimeData.eval_beat$Pred_recentDOW[targetIdx] <- 0
    }else if (nrow(BeatCnt.histMonth)==0){
      CrimeData.eval_beat$Pred_histMonth[targetIdx] <- 0
      CrimeData.eval_beat$Pred_histDOW[targetIdx] <- 0
    }else if (nrow(BeatCnt.histDOW)==0){
      CrimeData.eval_beat$Pred_histDOW[targetIdx] <- 0
    }else if (nrow(BeatCnt.recentDOW)==0){
      CrimeData.eval_beat$Pred_recentDOW[targetIdx] <- 0
    }else{
      CrimeData.eval_beat$Pred_LT[targetIdx] <- mean(BeatCnt.long$INC_CNT)
      CrimeData.eval_beat$Pred_histMonth[targetIdx] <- mean(BeatCnt.histMonth$INC_CNT)
      CrimeData.eval_beat$Pred_histDOW[targetIdx] <- mean(BeatCnt.histDOW$INC_CNT) 
      CrimeData.eval_beat$Pred_recentDOW[targetIdx] <- mean(BeatCnt.recentDOW$INC_CNT) 
    }
  }
}

# DISTRICT
for (i in 1:NumDays.eval){
  d <- dateSeq.eval[i]
  d.year <- as.integer(format(d,"%Y"))
  d.month <- as.integer(format(d,"%m"))
  d.dow <- weekdays(d)
  CrimeData.district_day_long <- subset(CrimeData.district_day,YEAR>=(d.year-5) & YEAR<d.year,select=c("DATEOCC","MONTH","DISTRICT","INC_CNT"))
  CrimeData.district_day_histMonth <- subset(CrimeData.district_day_long,MONTH==d.month,select=c("DATEOCC","DISTRICT","INC_CNT"))
  CrimeData.district_day_histDOW <- subset(CrimeData.district_day_long,MONTH==d.month & weekdays(DATEOCC)==d.dow,
                                           select=c("DATEOCC","DISTRICT","INC_CNT"))
  CrimeData.district_day_recentDOW <- subset(CrimeData.district_day,DATEOCC>=d-period.short & DATEOCC<=d-1 & weekdays(DATEOCC)==d.dow,
                                             select=c("DATEOCC","DISTRICT","INC_CNT"))
  for (j in districtList){
    targetIdx <- CrimeData.eval_district$DISTRICT==j&CrimeData.eval_district$DATEOCC==d
    DistrictCnt.long <- subset(CrimeData.district_day_long,DISTRICT==j)
    DistrictCnt.histMonth <- subset(CrimeData.district_day_histMonth,DISTRICT==j)
    DistrictCnt.histDOW <- subset(CrimeData.district_day_histDOW,DISTRICT==j)
    DistrictCnt.recentDOW <- subset(CrimeData.district_day_recentDOW,DISTRICT==j)
    # Some districts may have no incident records during the long/short periods
    if (nrow(DistrictCnt.long)==0){
      CrimeData.eval_district$Pred_LT[targetIdx] <- 0
      CrimeData.eval_district$Pred_histMonth[targetIdx] <- 0
      CrimeData.eval_district$Pred_histDOW[targetIdx] <- 0
      CrimeData.eval_district$Pred_recentDOW[targetIdx] <- 0
    }else if (nrow(DistrictCnt.histMonth)==0){
      CrimeData.eval_district$Pred_histMonth[targetIdx] <- 0
      CrimeData.eval_district$Pred_histDOW[targetIdx] <- 0
    }else if (nrow(DistrictCnt.histDOW)==0){
      CrimeData.eval_district$Pred_histDOW[targetIdx] <- 0
    }else if (nrow(DistrictCnt.recentDOW)==0){
      CrimeData.eval_district$Pred_recentDOW[targetIdx] <- 0
    }else{
      CrimeData.eval_district$Pred_LT[targetIdx] <- mean(DistrictCnt.long$INC_CNT)
      CrimeData.eval_district$Pred_histMonth[targetIdx] <- mean(DistrictCnt.histMonth$INC_CNT)
      CrimeData.eval_district$Pred_histDOW[targetIdx] <- mean(DistrictCnt.histDOW$INC_CNT) 
      CrimeData.eval_district$Pred_recentDOW[targetIdx] <- mean(DistrictCnt.recentDOW$INC_CNT) 
    }
  }
}