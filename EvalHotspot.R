HSpred <- function(CrimeData,groupSeq,Grd,subRegion=rep(TRUE,nrow(Grd)),Grd.full=NULL,
                   window.t=0,kernel.x=NULL,kernel.y=NULL,period=NULL,r=NULL,isInCity=NULL,prj=NULL){
  Ngrids <- nrow(Grd)
  Ngroups <- length(groupSeq)
  regionSize <- sum(subRegion==TRUE)
  PredVal <- rep(0,length=regionSize*Ngroups)
  
  for (i in 1:Ngroups){
    d <- groupSeq[i]
    if (period>window.t){
      # long-term
      CrimeHistPts <- subset(CrimeData,GROUP>=d-period & GROUP<=d-window.t-1,select=c("X_COORD","Y_COORD","INC_CNT"))
    }else{
      #short-term
      CrimeHistPts <- subset(CrimeData,GROUP>=d-period & GROUP<=d-1,select=c("X_COORD","Y_COORD","INC_CNT"))
    }
    
    HotSpot <- SpatialKernSmCrime(CrimeHistPts,Grd.full,r,kernel.x,kernel.y,isInCity,prj)
    HS <- HotSpot$KernSm.df_inPoly[subRegion,]
    PredVal[((i-1)*regionSize+1):(i*regionSize)] <- HS$KS_VAL/sum(HS$KS_VAL)
  } 
  return(PredVal)
}

sigma <- 2  # unit: pixel size 

window.x <- 3 
window.y <- 3
window.t <- 8

kernel.Xgrd <- seq(-window.x,window.x,length.out=2*window.x+1)
kernel.Ygrd <- seq(-window.y,window.y,length.out=2*window.y+1)

kernel.x <- 1/(sqrt(2*pi)*sigma)*exp(-kernel.Xgrd^2/(2*sigma^2))
kernel.y <- 1/(sqrt(2*pi)*sigma)*exp(-kernel.Ygrd^2/(2*sigma^2))


hotspots <- c("LT","ST")
period.short <- 2
period.long <- 5*floor(365/groupSize)
periods <- c(period.long,period.short) 


PredMat_HS.eval <- matrix(0,nrow=Ngrids*Ngroups.eval,ncol=length(hotspots))
for (m in 1:length(hotspots)){
  PredMat_HS.eval[,m] <- HSpred(CrimeData,groupSeq.eval,RegGrd,subRegion=rep(TRUE,nrow(RegGrd)),RegGrd.full,
                                window.t,kernel.x,kernel.y,periods[m],r,isInCity,prj) 
}

# remove the first and the last group from evaluation to be consistent with the TSmesh method
rmIdx <- c(1:Ngrids,((Ngroups.eval-1)*Ngrids+1):(Ngroups.eval*Ngrids))
PredMat_HS.eval <- PredMat_HS.eval[-rmIdx,]

perfList.hs <- vector("list",length(hotspots))
AUCmat.hs <- matrix(0,ncol=length(hotspots),nrow=bootn)

for (m in 1:length(hotspots)){
  bootList <- list(Pred=vector("list",bootn),Label=vector("list",bootn))
  
  PredLabel.hs <- data.frame(Pred=PredMat_HS.eval[,m],Label=Label.eval)
  for (i in 1:bootn){
    bootList$Pred[[i]] <- PredLabel.hs$Pred[repIdxMat[,i]]
    bootList$Label[[i]] <- PredLabel.hs$Label[repIdxMat[,i]]
  }
  # bootObj <- getROC(bootList$Pred,bootList$Label,downsample=TRUE,cutoff=rocCutoff)
  bootObj <- getROC(bootList$Pred,bootList$Label,downsample=TRUE,downRate=sampleRate)
  perfList.hs[[m]] <- bootObj$perf
  AUCmat.hs[,m] <- bootObj$auc
}