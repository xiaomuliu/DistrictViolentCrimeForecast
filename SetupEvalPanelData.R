Path.beat <- paste0(Path.GIS,"cpd_beats")
beat.shp <- readOGR(Path.beat, "cpd_beats")
Path.district <- paste0(Path.GIS,"cpd_districts")
district.shp <- readOGR(Path.district, "cpd_districts")

beat_template.spdf <- beat.shp
# remove some useless/redundant attributes
beat_template.spdf@data$SECTOR <- NULL
beat_template.spdf@data$BEAT <- NULL
beat_template.spdf@data$BEAT_NUM <- NULL

district_template.spdf <- district.shp
# remove some useless/redundant attributes
district_template.spdf@data$DIST_LABEL <- NULL
district_template.spdf@data$PDF <- NULL
district_template.spdf@data$DIST_NUM <- NULL

beatList <- sort(unique(beat_template.spdf$BEAT_NUMBE))
beatList <- beatList[beatList!='3100'] # remove beat '3100', a dummy beat without examples
NumBeat <- length(beatList)

districtList <- sort(unique(district_template.spdf$DISTRICT))
districtList <- districtList[districtList!='031'] # remove district '031', a dummy district without examples
NumDistrict <- length(districtList)

RegGrd.sp <- SpatialPixels(city.df_full[isInCity])
GrdInBeat <- over(RegGrd.sp,beat_template.spdf)
GrdInBeat <- subset(cbind(GrdInBeat, RegGrd.sp@coords),select=-c(OBJECTID))
names(GrdInBeat)[names(GrdInBeat)=="BEAT_NUMBE"] <- "BEAT"
GrdInBeat <- subset(GrdInBeat,select=c("BEAT","X_COORD","Y_COORD"))
# Assign the grid cells that have missing values for BEAT to the nearest beats
BeatCtr <- coordinates(beat.shp) # centroids 
OutBoundPix <- which(is.na(GrdInBeat$BEAT))
for (i in OutBoundPix){
  distVec <- apply(BeatCtr-matrix(rep(as.matrix(GrdInBeat[i,c("X_COORD","Y_COORD")]),nrow(BeatCtr)),ncol=2,byrow=TRUE),
                   MARGIN=1,FUN=function(x){x[1]^2+x[2]^2})
  GrdInBeat$BEAT[i] <- beat_template.spdf@data$BEAT_NUMBE[which.min(distVec)]
}

GrdInDistrict <- over(RegGrd.sp,district_template.spdf)
GrdInDistrict <- subset(cbind(GrdInDistrict, RegGrd.sp@coords),select=-c(OBJECTID))
# Assign the grid cells that have missing values for DISTRICT to the nearest beats
DistrictCtr <- coordinates(district.shp) # centroids 
OutBoundPix <- which(is.na(GrdInDistrict$DISTRICT))
for (i in OutBoundPix){
  distVec <- apply(DistrictCtr-matrix(rep(as.matrix(GrdInDistrict[i,c("X_COORD","Y_COORD")]),nrow(DistrictCtr)),ncol=2,byrow=TRUE),
                   MARGIN=1,FUN=function(x){x[1]^2+x[2]^2})
  GrdInDistrict$DISTRICT[i] <- district_template.spdf@data$DISTRICT[which.min(distVec)]
}
# There is one pixel corresponding to district 31. Change its district membership to the closest other district.
OutBoundPix2 <- which(GrdInDistrict$DISTRICT=='031')
for (i in OutBoundPix2){
  distVec <- apply(DistrictCtr-matrix(rep(as.matrix(GrdInDistrict[i,c("X_COORD","Y_COORD")]),nrow(DistrictCtr)),ncol=2,byrow=TRUE),
                   MARGIN=1,FUN=function(x){x[1]^2+x[2]^2})
  GrdInDistrict$DISTRICT[i] <- district_template.spdf@data$DISTRICT[sort.int(distVec,index.return=T)$ix[2]]
}


CrimeData.beat_day <- aggregate(INC_CNT~BEAT+DISTRICT+DATEOCC+YEAR+MONTH+DOW+HOLIDAY,data=CrimeData,FUN=sum,na.rm=TRUE)
CrimeData.beat_day <- CrimeData.beat_day[order(CrimeData.beat_day$DATEOCC),]
CrimeData.beat_day$DOW <- factor(CrimeData.beat_day$DOW, levels=c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"))

CrimeData.district_day <- aggregate(INC_CNT~DISTRICT+DATEOCC+YEAR+MONTH+DOW+HOLIDAY,data=CrimeData,FUN=sum,na.rm=TRUE)
CrimeData.district_day <- CrimeData.district_day[order(CrimeData.district_day$DATEOCC),]
CrimeData.district_day$DOW <- factor(CrimeData.district_day$DOW, levels=c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"))


period.long <- 5*365
period.short <- 21

# Create a full panel (of size "number of beats * number of dates") data frame
source("ConstructPanelData.R")

# BEAT
CrimeData.eval_beat <- subset(CrimeData.beat_day,DATEOCC>=startDate.eval&DATEOCC<=endDate.eval&BEAT!='3100')
CrimeData.eval_beat <- CrimeData.eval_beat[order(CrimeData.eval_beat$DATEOCC,CrimeData.eval_beat$BEAT),]
CrimeData.eval_beat <- ConstructPanelData(CrimeData.eval_beat,beat_template.spdf,area="BEAT",AreaList=beatList) 
CrimeData.eval_beat$Pred_mesh <- rep(0,nrow(CrimeData.eval_beat))
CrimeData.eval_beat$Pred_LT <- rep(0,nrow(CrimeData.eval_beat))
CrimeData.eval_beat$Pred_histMonth <- rep(0,nrow(CrimeData.eval_beat))
CrimeData.eval_beat$Pred_histDOW <- rep(0,nrow(CrimeData.eval_beat))
CrimeData.eval_beat$Pred_recentDOW <- rep(0,nrow(CrimeData.eval_beat))

# Add corresponding district numbers for each beat (optional)
inDistrict <- aggregate(.~BEAT_NUMBE,data=beat_template.spdf@data[,c("DISTRICT","BEAT_NUMBE")],FUN=function(x){x[1]})
names(inDistrict)[names(inDistrict)=="BEAT_NUMBE"] <- "BEAT"
inDistrict$DISTRICT <- factor(inDistrict$DISTRICT)
levels(inDistrict$DISTRICT) <- levels(CrimeData$DISTRICT)
inDistrict <- inDistrict[inDistrict$DISTRICT!='031',] # remove Distirct '031' 

for (i in 1:nrow(inDistrict)){
  CrimeData.eval_beat$DISTRICT[CrimeData.eval_beat$BEAT==inDistrict$BEAT[i]] <- inDistrict$DISTRICT[i]
}

# DISTRICT
CrimeData.eval_district <- subset(CrimeData.district_day,DATEOCC>=startDate.eval&DATEOCC<=endDate.eval&DISTRICT!='031')
CrimeData.eval_district <- CrimeData.eval_district[order(CrimeData.eval_district$DATEOCC,CrimeData.eval_district$DISTRICT),]
CrimeData.eval_district <- ConstructPanelData(CrimeData.eval_district,district_template.spdf,area="DISTRICT",AreaList=districtList) 
CrimeData.eval_district$Pred_mesh <- rep(0,nrow(CrimeData.eval_district))
CrimeData.eval_district$Pred_LT <- rep(0,nrow(CrimeData.eval_district))
CrimeData.eval_district$Pred_histMonth <- rep(0,nrow(CrimeData.eval_district))
CrimeData.eval_district$Pred_histDOW <- rep(0,nrow(CrimeData.eval_district))
CrimeData.eval_district$Pred_recentDOW <- rep(0,nrow(CrimeData.eval_district))