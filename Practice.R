
a <- 3; b<-1; c<-5

ntrain <- 1000
ntest <- 1000
N <- ntrain + ntest


#x1 <- rnorm(ntrain + ntest, 0, 1)
x1 <- seq(from=-5, to=5, by=10/(ntrain+ntest))
x1 <- x1[sample(1:length(x1))]
x1 <- x1[1:(ntrain+ntest)]
#x2 <- rnorm(ntrain + ntest, 0, 1)
#x3 <- rnorm(ntrain + ntest, 0, 1)
#x4 <- rnorm(ntrain + ntest, 0, 1)
#x5 <- rnorm(ntrain + ntest, 0, 1)
e1 <- rnorm(ntrain + ntest, 0, 1)  
e2 <- rexp(ntrain + ntest, rate=1/20) - 20
e <- (1-b)*e1 + b*e2 # convex combination of errors
y <- 3*x1 + a/10*x1^2 + a/50*(x1^3)*(a>1.5) + a*(x1>0)*(a>3) + e*(c* (abs(x1) + 1))

Data <- data.frame(x1, y)
Train <- Data[1:ntrain, ]
Test <- Data[(ntrain+1):(ntrain+ntest), ]

ggplot(data=Train, aes(x=x1, y=y)) + geom_point()
ggplot(data=Test, aes(x=x1, y=y)) + geom_point()

# Intervals from Linear Model
# Assume linearity, normality, constant variance
LM <- lm(data=Train, y~.)
LMpred <- predict(LM, newdata=Test) 
MSPE_LM <- mean((LMpred-Test$y)^2)
LM_PI <- predict(LM, newdata=Test, interval="prediction") 
LM_Contains <- (Test$y >= LM_PI[,2]) & (Test$y <= LM_PI[,3])
Cov_LM <- mean(LM_Contains)
Width_LM <- mean(LM_PI[,3]-LM_PI[,2])

RF <- quantreg(data=Train, formula = y~., nodesize=5, method="forest", prob=c(0.025, 0.975))
RFpredinfo <- quantreg(object=RF, newdata=Test, prob=c(0.025, 0.975))
RFpred <- RFpredinfo$predicted
MSPE_RF <- mean((RFpred - Test$y)^2)
# Intervals from Random Forest - Sym. OOB method
# Assumes constant variance, error distribution symmetric
OOB_resid <- Train$y - RF$predicted.oob
MOE <- quantile(abs(OOB_resid), .95)
RF_PI2 <- data.frame(RFpred, RFpred-MOE, RFpred+MOE)
RF2_Contains <- (Test$y >= RF_PI2[,2]) & (Test$y <= RF_PI2[,3])
Coverage_RFOOB_Sym <- mean(RF2_Contains)
Width_RFOOB_Sym <- mean(RF_PI2[,3]-RF_PI2[,2])
# Intervals from Random Forest - Non. Sym. OOB method
# Assumes constant variance
Lower <- quantile(OOB_resid, .025)
Upper <- quantile(OOB_resid, .975)
RF_PI <- data.frame(RFpred, RFpred+Lower, RFpred+Upper)
RF_Contains <- (Test$y >= RF_PI[,2]) & (Test$y <= RF_PI[,3])
Coverage_RFOOB_NonSym <- mean(RF_Contains)
Width_RFOOB_NonSym <- mean(RF_PI[,3]-RF_PI[,2])
# Intervals from Quantile Random Forest 
# Does not assume constant variance
QRFLower <- RFpredinfo$quantreg$quantiles[,1]
QRFUpper <- RFpredinfo$quantreg$quantiles[,2]
QRF_PI <- data.frame(RFpred, QRFLower, QRFUpper)
QRF_Contains <- (Test$y >= QRF_PI[,2]) & (Test$y <= QRF_PI[,3])
Coverage_QRF <- mean(QRF_Contains)
Width_QRF <- mean(QRF_PI[,3]-QRF_PI[,2])
library(quantregForest)
Trainx <- data.frame(Train$x1)
names(Trainx) <- "x1"
Testx <- data.frame(Test$x1)
names(Testx) <- "x1"
QRF <- quantregForest(x=Trainx, y=Train$y, keep.inbag = TRUE)
QRFpred <- predict(QRF, newdata = Testx, what=mean)
mean((QRFpred - Test$y)^2)
QRFLower <- predict(QRF, newdata=Testx, what=alpha/2)
QRFUpper <- predict(QRF, newdata=Testx, what=1-alpha/2)
QRF_PI <- data.frame(QRFpred, QRFLower, QRFUpper)
mean(Test$y >= QRF_PI[,2] & Test$y <= QRF_PI[,3])
mean(QRF_PI[,3]-QRF_PI[,2])



Res_LM <- c(MSPE_LM, Cov_LM, Width_LM)
Res_RFOOBSym <- c(MSPE_RF, Coverage_RFOOB_Sym, Width_RFOOB_Sym)
Res_RFOOBNonSym <- c(MSPE_RF, Coverage_RFOOB_NonSym, Width_RFOOB_NonSym)
Res_QRF <- c(MSPE_RF, Coverage_QRF, Width_QRF)

Res_Table <- t(data.frame(Res_LM, Res_RFOOBSym, Res_RFOOBNonSym, Res_QRF))
colnames(Res_Table) <- c("MSPE", "Coverage", "Width")
rownames(Res_Table) <- c("Linear Model", "Random Forest with Symmetry Assumption", "Random Forest - No Symmetry Assumption", "Random Forest - No Constant Variance Assumption")

#Plot <- qplot(Test$x1, Test$y)
p1<-ggplot(data=Test, aes(x=x1, y=y)) + geom_point(aes(color=LM_Contains)) + geom_line(aes(x=x1, y=LM_PI[,1]), color="red")
p1<-p1+geom_ribbon(aes(ymin=LM_PI[,2], ymax=LM_PI[,3]), linetype=2, alpha=0.5) + ggtitle("Assumes Lin., Norm., CV")

p2<-ggplot(data=Test, aes(x=x1, y=y)) + geom_point() + geom_line(aes(x=x1, y=RF_PI2[,1]), color="red")
p2<-p2+geom_ribbon(aes(ymin=RF_PI2[,2], ymax=RF_PI2[,3]), linetype=2, alpha=0.5) + ggtitle("Assumes Norm., CV")

p3<-ggplot(data=Test, aes(x=x1, y=y)) + geom_point() + geom_line(aes(x=x1, y=RF_PI[,1]), color="red")
p3<-p3+geom_ribbon(aes(ymin=RF_PI[,2], ymax=RF_PI[,3]), linetype=2, alpha=0.5)  + ggtitle("Assumes CV")

p4<-ggplot(data=Test, aes(x=x1, y=y)) + geom_point() + geom_line(aes(x=x1, y=QRF_PI[,1]), color="red")
p4<-p4+geom_ribbon(aes(ymin=QRF_PI[,2], ymax=QRF_PI[,3]), linetype=2, alpha=0.5)  + ggtitle("Assumes None of 3")

