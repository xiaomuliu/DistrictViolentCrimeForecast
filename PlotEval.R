# # Plot beat evalutation by district
# metrics <- c('MSE','pMSE')
# metricNames <- c('MSE','MSE%')
# for (j in 1:length(metrics)){
#   layout(matrix(c(1,2,3,4,5,5), ncol=2, byrow=TRUE), heights=c(4, 4, 1))
#   subfigCnt <- 0
#   for (i in districtList){
#     par(mar=c(5,4,4,2),pty='m')
#     EvalDistrict <- subset(EvalResults.beat,DISTRICT==i,select=c('BEAT','MODEL',metrics[j]))
#     # reshape to contingency-table form (long-format to wide-format)
#     EvalDistrict.wide <- stats::reshape(EvalDistrict,direction='wide',v.names=metrics[j],timevar='BEAT',idvar='MODEL')
#     names(EvalDistrict.wide)[-1] <- as.character(sort(unique(EvalDistrict$BEAT)))
#     barplot(as.matrix(EvalDistrict.wide[,-1]), main=paste("DISTRICT",i),xlab="BEAT",ylab=metricNames[j], 
#             col=rainbow(length(models)), beside=TRUE)
#     subfigCnt <- subfigCnt+1
#     if (subfigCnt %% 4==0 | subfigCnt==length(districtList)){
#       par(mar=c(1,0,0,0))
#       plot.new()
#       legend(x="bottom",inset=c(0,0), legend=models, pch=15, col=rainbow(length(models)), 
#              cex=1, horiz=TRUE,xpd=NA)
#     }
#   }
# }
# 
# # Plot district evalutation 
metrics <- c('MSE','MAPE')
metricNames <- c('MSE','MdAPE%')
modelNames <- c('Proposed Model','Baseline 1','Baseline 2','Baseline 3','Baseline 4')

# for (j in 1:length(metrics)){
#   par(mfrow=c(1,1),mar=c(5.1, 4.1, 4.1, 2.1),pty='m')
#   # reshape to contingency-table form (long-format to wide-format)
#   EvalResults.district_wide <- stats::reshape(EvalResults.district[,c('DISTRICT','MODEL',metrics[j])],direction='wide',v.names=metrics[j],timevar='DISTRICT',idvar='MODEL')
#   names(EvalResults.district_wide)[-1] <- as.character(sort(unique(EvalResults.district$DISTRICT)))
#   barplot(as.matrix(EvalResults.district_wide[,-1]), xlab="DISTRICT",ylab=metricNames[j],
#           col=rainbow(length(models)), beside=TRUE, cex.lab=1.3)
# 
#   # par(new=TRUE)
#   # plot(0, 0, type="n", bty="n", xaxt="n", yaxt="n",xlab="",ylab="")
#   legend(x="topright",inset=c(0,-0.3), bty="n",legend=modelNames, pch=15, col=rainbow(length(models)),
#          cex=1.2, horiz=FALSE,xpd=TRUE)
# }

par(mar=c(5, 4, 4, 2) + 0.1)
for (j in 1:length(metrics)){
  par(xpd = T, mar = par()$mar + c(0,0,0,7))
  # reshape to contingency-table form (long-format to wide-format)
  EvalResults.district_wide <- stats::reshape(EvalResults.district[,c('DISTRICT','MODEL',metrics[j])],direction='wide',v.names=metrics[j],timevar='DISTRICT',idvar='MODEL')
  names(EvalResults.district_wide)[-1] <- as.character(sort(unique(EvalResults.district$DISTRICT)))
  barplot(as.matrix(EvalResults.district_wide[,-1]), xlab="DISTRICT",ylab=metricNames[j],
          col=rainbow(length(models)), beside=TRUE, cex.lab=1.3)
  # axis(1,at=1:(ncol(EvalResults.district_wide)-1),labels=names(EvalResults.district_wide)[2:ncol(EvalResults.district_wide)])
  # par(new=TRUE)
  # plot(0, 0, type="n", bty="n", xaxt="n", yaxt="n",xlab="",ylab="")
  legend(x="topright", inset=c(-0.1,-0.15), legend=modelNames, pch=15, col=rainbow(length(models)),
         cex=1.2, horiz=FALSE,xpd=TRUE)
  par(mar=c(5, 4, 4, 2) + 0.1)
}



# Plot a specific district evaluation
metrics <- c('MSE','pMSE')
metricNames <- c('MSE','MSE%')
distNums <- c('007','011')
modelNames <- c('Prediction', 'Baseline 1', 'Baseline 2', 'Baseline 3', 'Baseline 4')
for (i in distNums){
  for (j in 1:length(metrics)){
    par(mfrow=c(1,1),mar=c(5.1, 4.1, 4.1, 2.1),pty='s')
    E <- subset(EvalResults.district,DISTRICT==i,metrics[j])
    barplot(unname(as.matrix(E)), space=0.5, xlab="Model",ylab=metricNames[j], 
            main=paste('DISTRICT',i), col=rainbow(length(models)), beside=TRUE)
    # par(new=TRUE)
    # plot(0, 0, type="n", bty="n", xaxt="n", yaxt="n",xlab="",ylab="")
    legend(x="topleft", inset=c(-0.3,0),  bty="n", legend=modelNames, pch=15, col=rainbow(length(models)),
        cex=1, horiz=FALSE, xpd=TRUE)
  }  
}
