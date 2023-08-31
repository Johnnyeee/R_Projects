#data
setwd("/Users/johnnys/Downloads/PSTAT 174")

frozen = read.table("IPN31152N.csv",
                    sep=",", header=FALSE, skip=1)

fro <- frozen[1:132,]
fro.test <- frozen[133:144,]


#plot original
ice = ts(frozen[,2], start = c(1981,1), end = c(1992,12), frequency = 12)
reg <- lm(V2 ~ as.numeric(1:132), data = fro)
plot(V2 ~ as.numeric(1:132), data = fro); abline(reg)
op = par(mfrow = c(1,2))
ice1 = ts(fro[,2], start = c(1981,1), end = c(1991,12), frequency = 12)
ice1_1 = ts(fro[,2], start = c(0), end = c(131), frequency = 1)
ts.plot(ice1_1, ylab = "index 2017 = 100", main ="Original Time Series"); abline(reg,col = "red")
ts.plot(ice1, ylab = "index 2017 = 100", main ="Original Time Series")
ts.plot(ice)
#histogram of original data
hist(ice1, main = "")

op = par(mfrow = c(2,2))
hist(ice1,main ="Histogram of Original Time Series")
acf(ice1,lag.max = 60, main ="ACF of Original Time Series")
pacf(ice1,lag.max = 60,main = "")

#find lambda to transform (constant variables)
library(MASS)
t = 1:length(ice1)
fit = lm(ice1 ~ t)
bcTransform = boxcox(ice1 ~ t,plotit = TRUE)
bcTransform = boxcox(ice1_1 ~ t,plotit = TRUE)


lambda = bcTransform$x[which(bcTransform$y == max(bcTransform$y))]
ice1.bc = (1/lambda)*(ice1^lambda-1)
ice1.log = log(ice1)
op= par(mfrow=c(1,2))
ts.plot(ice1, main = "Original Times Series")
ts.plot(ice1_1, main = "Original Times Series"); abline(reg,col = "red")
ts.plot(ice1.bc, main = "Box-Cox Transform")
ts.plot(ice1.log, main = "Log Transform")
hist(ice1.bc)
hist(ice1.log)

#Compare before and after log
par(mfrow=c(3,3))
plot.ts(ice1,xlab = "", main = "")
hist(ice1, xlab = "", main="")
qqnorm(ice1, main = "", xlab = "")
qqline(ice1, col = "red")

plot.ts(ice1.log,xlab = "", main = "")
hist(ice1.log, xlab = "", main = "")
qqnorm(ice1.log, main = "", xlab = "")
qqline(ice1.log, col = "red")

plot.ts(ice1.bc,xlab = "", main = "")
hist(ice1.bc, xlab = "", main = "")
qqnorm(ice1.bc, main = "", xlab = "")
qqline(ice1.bc, col = "red")

# de-seasonal and de-trend
y2 = diff(ice1.log, 12)
y21 = diff(y2, 1)
y211 = diff(y21, 1)
ts.plot(y2, main = "De-seasonalized Time Series",
        ylab = expression(nabla^{12}~nabla~U[t]))
abline(h = mean(y2),lty = 2)

ts.plot(y21,main= "De-trended/seasonalized Time Series",
        ylab = expression(nabla^{12}~nabla~U[t]))
abline(h = mean(y21),lty = 2)

c(Ut = var(ice1.log),delta_12 = var(y2),delta_1_12 = var(y21)
  ,deta_1_1_12 = var(y211))

op= par(mfrow=c(2,3))
acf(ice1.log,lag.max = 60,main = expression(ln(U[t])))
acf(y2,lag.max = 60,main = expression(nabla[12]~~ln(U[t])))
acf(y21,lag.max = 60,main = expression(nabla[12]~nabla[1]~~ln(U[t])))
pacf(ice1.log,lag.max = 60,main = "")
pacf(y2,lag.max = 60,main = "")
pacf(y21,lag.max = 60,main = "")

#compare histograms
op= par(mfrow=c(1,3))
hist(ice1.log)
hist(y21)


#ACF and PACF of de-trend&season
decomp <- decompose(ts(as.ts(ice1.bc), 
                       start=c(1981,1), end=c(1991,12), frequency = 12))
plot(decomp)
op= par(mfrow=c(1,2))
acf(y21,lag.max = 60,main = "ACF of the log(U_t) differenced at lags 12 and 1")
pacf(y21,lag.max = 60,main = "PACF of the log(U_t) differenced at lags 12 and 1")

# D = 1; s = 12; d = 1; Q = 1; P = 0 or 1 or 2; q = 1; p = 1 or 0(tail off)
fit_1 <- arima(ice1.log, order = c(0,1,1), seasonal = list(order = c(0,1,1))
               ,method = "ML")
fit_2 <- arima(ice1.log, order = c(1,1,1), seasonal = list(order = c(0,1,1))
               ,method = "ML")
fit_3 <- arima(ice1.log, order = c(0,1,1), seasonal = list(order = c(1,1,1))
               ,method = "ML")
fit_4 <- arima(ice1.log, order = c(1,1,1), seasonal = list(order = c(1,1,1))
               ,method = "ML")
fit_5 <- arima(ice1.log, order = c(0,1,1), seasonal = list(order = c(2,1,1))
               ,method = "ML")
fit_6 <- arima(ice1.log, order = c(1,1,1), seasonal = list(order = c(2,1,1))
               ,method = "ML")
fit_1_1_1 <- arima(ice1.log, order = c(4,1,1), seasonal = list(order = c(0,1,1))
               ,fixed = c(0,0,0,NA,NA,NA),method = "ML")

library(qpcR)
c(fit_1 = AICc(fit_1),fit_2 = AICc(fit_2),fit_3 = AICc(fit_3),
  fit_4 = AICc(fit_4),fit_5 = AICc(fit_5),fit_6 = AICc(fit_6))
fit_1
fit_2
# 0=ar1=ma2
fit_2_1 <- arima(ice1.log, order = c(1,1,1), seasonal = list(order = c(0,1,1),period=12)
                 ,fixed = c(0,NA,NA),method = "ML")
fit_3_1 <- arima(ice1.log, order = c(1,1,1), seasonal = list(order = c(1,1,1),period=12)
               ,fixed = c(0,0,0,NA),method = "ML")
AICc(fit_2_1)
#AIC(3_1) is higher than AIC(3)
fit_1_1 <- arima(ice1.log, order = c(1,1,0), seasonal = list(order = c(1,1,1),period=12)
                 ,method = "ML")
AICc(fit_1_1)
c(fit_3_1 = AICc(fit_3_1), fit_3 = AICc(fit_3), fit_1_1 = AICc(fit_1_1),
  fit_1 = AICc(fit_1))

fit_1_1
fit_3

modelA <- arima(ice1.log, order = c(9,1,1), seasonal = list(order = c(0,1,1))
                ,fixed = c(0,0,0,0,0,0,0,0,NA,NA,NA),method = "ML")
modelA
resA <- residuals(modelA)
op= par(mfrow=c(1,2))
acf(resA, lag.max = 40)
pacf(resA, lag.max = 40)

pred.orig <- exp(pred.tr$pred)
ts.plot(ice, xlim= c(1990,1993),ylim=c(90,270))
lines(U, col="blue", lty="dashed")
lines(L, col="blue", lty="dashed")
points(pred.orig,col = "red", pch = 1)
points(ice1,col = "black", pch = 20)
legend("bottomright", c("Prediction","95% C.I.","original data"),
       fill = c("red", "blue","black"), cex = 0.7)

