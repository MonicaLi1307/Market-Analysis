library(ggplot2)
library(ggfortify)
library(MASS) 

data <- read.csv('StockArchive/BPCL.csv',encoding="UTF-8")
#data$Date <- ymd(data$Date)
#data_tsi <- as_tsibble(data, index=Date)
close = data$Close

plot(close,type='l',xlab='Time',ylab='Close')

hist(close)
# acf(log(close))
bcTransform <- boxcox(close~ as.numeric(1:length(close)))
bcTransform$x[which(bcTransform$y == max(bcTransform$y))] 
lambda = bcTransform$x[which(bcTransform$y == max(bcTransform$y))]

close.bc = (1/lambda)*(close^lambda-1)
close.log <- log(close)
plot.ts(close.bc)
plot.ts(close.log)

hist(close.log, col="light blue", xlab="", main="histogram; ln(U_t)") 
hist(close.bc, col="light blue", xlab="", main="histogram; bc(U_t)") 

y <- ts(as.ts(close.log), frequency = 12)
decomp <- decompose(y)
plot(decomp)

log_var = var(close.log)
close.log_12 <- diff(close.log, lag=12)
plot.ts(close.log_12, main="Ln(U_t) differenced at lag 12")
log12_var = var(close.log_12)
fit <- lm(close.log_12 ~ as.numeric(1:length(close.log_12))); 
abline(fit, col="red") 
log12_mean = mean(close.log_12)
abline(h=mean(close.stat), col="blue")

log12_mean = mean(close.log_12)
close.stat <- diff(close.log_12, lag=1)
plot.ts(close.stat, main="Ln(U_t) differenced at lag 12 and lag 1")
# plot.ts(close.stat, main="Ln(U_t) differenced at lag 12 & lag 1")
fit <- lm(close.stat ~ as.numeric(1:length(close.stat))); 
abline(fit, col="red")
stat_mean = mean(close.stat)
abline(h=mean(close.stat), col="blue")
stat_var = var(close.stat)

acf(close.log, lag.max=40, main="ACF of the log(U_t)")
acf(close.log_12, lag.max=40, main="ACF of the log(U_t), differenced at lag 12")
acf(close.stat, lag.max=40, main="ACF of the log(U_t), differenced at lags 12 and 1")
hist(close.stat, col="light blue", xlab="", main="histogram; ln(U_t) differenced at lags 12 & 1")

hist(close.stat, density=20,breaks=20, col="blue", xlab="", prob=TRUE)
m <- mean(close.stat)
std <- sqrt(var(close.stat))
curve( dnorm(x,m,std), add=TRUE )

pacf(close.stat, lag.max=40, main="PACF of the ln(U_t), differenced at lags 12 and 1")



p = 0
d = 1
q = 3
P = 1
D = 1
Q = 1

# modeA
modelA = arima(close.log, order=c(p,d,q), seasonal = list(order = c(P,D,Q), period = 12), method="ML")
summary(modelA)

win.graph(width=6.5,height=6); 
tsdiag(modelA)
win.graph(width=4,height=3,pointsize=8)
hist(residuals(modelA),xlab='Residuals')
win.graph(width=6.5,height=6)
qqnorm(residuals(modelA))
qqline(residuals(modelA))

res_A = residuals(modelA)
shapiro.test(res_A[1:5000])
Box.test(res_A,lag=12,type=c("Box-Pierce"),fitdf=3)
Box.test(res_A,lag=12,type=c("Ljung-Box"),fitdf=3)
Box.test(res_A,lag=12,type=c("Ljung-Box"),fitdf=0)

ar(res_A,aic = TRUE,order.max=NULL,method=c("yule-walker"))

# modelB
modelB = arima(close.log, order=c(0,1,1), seasonal = list(order = c(1,1,1), period = 12), method="ML")
summary(modelB)

win.graph(width=6.5,height=6); 
tsdiag(modelB)
win.graph(width=4,height=3,pointsize=8)
hist(residuals(modelB),xlab='Residuals')
win.graph(width=6.5,height=6)
qqnorm(residuals(modelB))
qqline(residuals(modelB))

res_B = residuals(modelB)
shapiro.test(res_B[1:5000])
Box.test(res_B,lag=12,type=c("Box-Pierce"),fitdf=3)
Box.test(res_B,lag=12,type=c("Ljung-Box"),fitdf=3)
Box.test(res_B,lag=12,type=c("Ljung-Box"),fitdf=0)

ar(res_B,aic = TRUE,order.max=NULL,method=c("yule-walker"))
