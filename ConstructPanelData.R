ConstructPanelData <- function(CrimeData,spData,area=c("BEAT","DISTRICT"),AreaList=NULL){
  # Fill in the records (set them to be zero) for the areas that have no incidents for centain days in crime data. 
  # Expand other corresponding columns so that the data becomes 
  # a "full grid" panel data (for every time unit (day), it has incidents for all the areas)
  
  if (area=="BEAT"){areaNo <- "BEAT_NUMBE"}
  else if (area=="DISTRICT"){areaNo <- "DISTRICT"}
  if (is.null(AreaList)){
    AreaList <- sort(unique(spData@data[[areaNo]]))
  }
  
  # DateList <- aggregate(.~DATEOCC,data=subset(CrimeData,select=c("DATEOCC","YEAR","MONTH","DOW","HOLIDAY")),FUN=function(x){x[1]})
  # DateList$DOW <- factor(DateList$DOW)
  # levels(DateList$DOW) <- levels(CrimeData$DOW)
  # DateList$HOLIDAY <- factor(DateList$HOLIDAY)
  # levels(DateList$HOLIDAY) <- levels(CrimeData$HOLIDAY)
  DateList <- data.frame(DATEOCC=unique(CrimeData$DATEOCC))
    
  # Outer join two tables (DateList and AreaList)  
  # AREAfull.df <- data.frame(do.call("rbind", replicate(length(AreaList), DateList, simplify=FALSE)),AREA=rep(AreaList,each=nrow(DateList)))
  # AREAfull.df <- AREAfull.df[order(AREAfull.df$DATEOCC,AREAfull.df$AREA),]
  # Or,equivalently
  AREAfull.df <- merge(DateList,data.frame(AREA=AreaList),all=TRUE)
  AREAfull.df <- AREAfull.df[order(AREAfull.df$DATEOCC,AREAfull.df$AREA),]
  
  names(AREAfull.df)[names(AREAfull.df)=="AREA"] <- area  
  CrimePanelData <- merge(AREAfull.df,CrimeData,all.x=TRUE)
  CrimePanelData <- CrimePanelData[order(CrimePanelData$DATEOCC),]
  CrimePanelData$INC_CNT[is.na(CrimePanelData$INC_CNT)] <- 0
  
  return(CrimePanelData)
}