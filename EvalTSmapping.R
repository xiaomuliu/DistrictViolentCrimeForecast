#  Cross-validation/boostrapping the roc curves (significant test)
TSmesh <- c("TSmesh")
set.seed(123)
bootn <- 100 
perfList.TSmesh <- vector("list",length(TSmesh))
AUCmat.TSmesh <- matrix(0,ncol=length(TSmesh),nrow=bootn)

repIdxMat <- matrix(rep(1:length(Label.eval),bootn),ncol=bootn)
repIdxMat <- apply(repIdxMat,2,sample,size=nrow(repIdxMat),replace=TRUE)
# rocCutoff<- seq(1,0,length=101)
sampleRate <- 10^(-3)

for (m in 1:length(TSmesh)){
  bootList <- list(Pred=vector("list",bootn),Label=vector("list",bootn))
  
  if (length(TSmesh==1)){
    PredLabel.TSmesh <- data.frame(Pred=ProbVecMap.eval,Label=Label.eval)
  }else{
    PredLabel.TSmesh <- data.frame(Pred=ProbVecMap.eval[,m],Label=Label.eval)
  }
  
  for (i in 1:bootn){
    bootList$Pred[[i]] <- PredLabel.TSmesh$Pred[repIdxMat[,i]]
    bootList$Label[[i]] <- PredLabel.TSmesh$Label[repIdxMat[,i]]
  }
  # bootObj <- getROC(bootList$Pred,bootList$Label,downsample=TRUE,cutoff=rocCutoff)
  bootObj <- getROC(bootList$Pred,bootList$Label,downsample=TRUE,downRate=sampleRate)
  perfList.TSmesh[[m]] <- bootObj$perf
  AUCmat.TSmesh[,m] <- bootObj$auc
}