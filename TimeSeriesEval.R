par(mfrow=c(1,1))
plot(CrimeData.eval$DATEOCC, CrimeData.eval$INC_CNT,type="p",pch=16,cex=0.5,col="blue",
     xlab="DATE",ylab="Crime Count",main="Daily Crime Incidents and Predicted Values")
points(CrimeData.eval$DATEOCC, CrimeData.eval$TSpred,type="p",pch=16,cex=0.5,col="red")
points(CrimeData.eval$DATEOCC, CrimeData.eval$TStrend,type="b",pch=16,cex=0.5,col="green")
legend("topright",legend=c("Actual","Predicted","Trend"),col=c("blue","red","green"),pch=c(16,16,16))

MSE.pred <- with(CrimeData.eval,round(mean((TSpred-INC_CNT)^2),2))
RMSE.pred <- sqrt(MSE.pred)
pMSE.pred <- MSE.pred / mean(CrimeData.eval$INC_CNT^2)
Rsq.pred <- with(CrimeData.eval, 1-t(INC_CNT-TSpred)%*%(INC_CNT-TSpred)/(t(INC_CNT-mean(INC_CNT))%*%(INC_CNT-mean(INC_CNT))))

MSE.trend<- with(CrimeData.eval,round(mean((TStrend-INC_CNT)^2),2))
RMSE.trend <- sqrt(MSE.trend)
pMSE.trend <- MSE.trend / mean(CrimeData.eval$INC_CNT^2)
Rsq.trend <- with(CrimeData.eval, 1-t(INC_CNT-TStrend)%*%(INC_CNT-TStrend)/(t(INC_CNT-mean(INC_CNT))%*%(INC_CNT-mean(INC_CNT))))

sprintf("Predition: MSE = %.2f, RMSE = %.2f, percentage MSE = %.2f%%, R squared = %.2f\n",
        MSE.pred, RMSE.pred, pMSE.pred, Rsq.pred)
sprintf("Trend-only: MSE = %.2f, RMSE = %.2f, percentage MSE = %.2f%%, R squared = %.2f\n",
        MSE.trend, RMSE.trend, pMSE.trend, Rsq.trend)