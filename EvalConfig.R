require(glmnet)
require(dummies)
require(doMC)
registerDoMC(cores=4)
parallel <- TRUE

glm <- "gaussian"
varSet <- c("DOW","weather","weatherdiff","timelag")
standardize <- "minmax"
Windowing <- TRUE
# nlambda <- 20
lambdaSeq <- 2^seq(-5,0.5,by=0.5)

startDate.eval <- as.Date("2014-01-01")
endDate.eval <- as.Date("2014-12-31")
dateSeq.eval <- seq.Date(startDate.eval,endDate.eval,by=1)
HistSpan <- 365*12
winSize <- 90
winNum <- 12