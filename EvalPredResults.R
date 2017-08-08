# Set reorder=TRUE, the result will be in order of sort(unique(group)) which is indentical to beatList. 
DistrictPred.mesh <- rowsum(RateMap_mesh.eval,group=GrdInDistrict$DISTRICT,reorder=TRUE,na.rm=TRUE)

# Fill district predictions into CrimeData.eval_beat data frame
# DISTRICT
for (i in 1:NumDays.eval){
  CrimeData.eval_district$Pred_mesh[CrimeData.eval_district$DATEOCC==dateSeq.eval[i]] <- DistrictPred.mesh[,i]
}

MSE <- function(x,x_hat){return(mean((x-x_hat)^2))}
MAPE<- function(x,x_hat){
  # avoid zero-division
  x[x==0]=1e-6
  return(median(abs(x-x_hat)/x)*100)
  }

models <- c('TSpred','LT','histMonth','histDOW','recentDOW')
NumModel <- length(models)

# DISTRICT
evalnames.district <-c('DISTRICT','MODEL','MSE','MAPE')
EvalResults.district <- data.frame(matrix(0,nrow=NumDistrict*NumModel,ncol=length(evalnames.district)))
names(EvalResults.district) <- evalnames.district

EvalResults.district$DISTRICT <- rep(districtList,each=NumModel)

EvalResults.district$MODEL <- factor(rep(models,times=NumDistrict))
idx.model <- matrix(FALSE,nrow=nrow(EvalResults.district),ncol=NumModel)
for (i in 1:NumModel){
  idx.model[,i] <- EvalResults.district$MODEL==models[i]
}

for (i in districtList){
  DistrictRecord <- subset(CrimeData.eval_district,DISTRICT==i,select=c("INC_CNT","Pred_mesh","Pred_LT","Pred_histMonth","Pred_histDOW","Pred_recentDOW"))
  
  idx.district <- EvalResults.district$DISTRICT==i
  
  EvalResults.district$MSE[idx.district&idx.model[,1]] <- with(DistrictRecord,MSE(INC_CNT,Pred_mesh))
  EvalResults.district$MSE[idx.district&idx.model[,2]] <- with(DistrictRecord,MSE(INC_CNT,Pred_LT))
  EvalResults.district$MSE[idx.district&idx.model[,3]] <- with(DistrictRecord,MSE(INC_CNT,Pred_histMonth))
  EvalResults.district$MSE[idx.district&idx.model[,4]] <- with(DistrictRecord,MSE(INC_CNT,Pred_histDOW))
  EvalResults.district$MSE[idx.district&idx.model[,5]] <- with(DistrictRecord,MSE(INC_CNT,Pred_recentDOW))
  
  EvalResults.district$MAPE[idx.district&idx.model[,1]] <- with(DistrictRecord,MAPE(INC_CNT,Pred_mesh))
  EvalResults.district$MAPE[idx.district&idx.model[,2]] <- with(DistrictRecord,MAPE(INC_CNT,Pred_LT))
  EvalResults.district$MAPE[idx.district&idx.model[,3]] <- with(DistrictRecord,MAPE(INC_CNT,Pred_histMonth))
  EvalResults.district$MAPE[idx.district&idx.model[,4]] <- with(DistrictRecord,MAPE(INC_CNT,Pred_histDOW))
  EvalResults.district$MAPE[idx.district&idx.model[,5]] <- with(DistrictRecord,MAPE(INC_CNT,Pred_recentDOW))

}