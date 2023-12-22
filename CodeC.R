library(ggplot2)
library(ggfortify)
library(MASS)
library(forecast)


data <- read.csv('StockArchive/BPCL.csv',encoding="UTF-8")
#data$Date <- ymd(data$Date)
#data_tsi <- as_tsibble(data, index=Date)
close = data$Close

plot(close,type='l',xlab='Time',ylab='Close')

hist(close)
bcTransform <- boxcox(close~ as.numeric(1:length(close)))
bcTransform$x[which(bcTransform$y == max(bcTransform$y))] 
lambda = bcTransform$x[which(bcTransform$y == max(bcTransform$y))]

close.bc = (1/lambda)*(close^lambda-1)
close.log <- log(close)
plot.ts(close.bc)
plot.ts(close.log)

fit.A <- arima(close.log[1:(length(close.log)-12)], order=c(0,1,1), seasonal = list(order = c(1,1,1), period = 12), method="ML")
forecast(fit.A)
# To produce graph with 12 forecasts on transformed data:
pred.tr <- predict(fit.A, n.ahead = 12)
U.tr= pred.tr$pred + 2*pred.tr$se # upper bound of prediction interval
L.tr= pred.tr$pred - 2*pred.tr$se # lower bound

win.graph(width=6.5,height=6); 
ts.plot(close.log[(length(close.log)-150):(length(close.log))],
        gpars=list(xlab="Time(last 150 points)",ylab='close log'))
a = 150+1
b = 150+12
lines(a:b,U.tr, col="blue", lty="dashed")
lines(a:b,L.tr, col="blue", lty="dashed")
points(a:b,pred.tr$pred, col="red")

# To produce graph with forecasts on original data:
win.graph(width=6.5,height=10); 
pred.orig <- exp(pred.tr$pred)
U= exp(U.tr)
L= exp(L.tr)
ts.plot(close[(length(close.log)-150):(length(close.log))], 
        gpars=list(xlab="Time(last 150 points)",ylab='close'),lty=c(1:3))
a = 150+1
b = 150+12
lines(a:b, U, col="blue", lty="dashed")
lines(a:b, L, col="blue", lty="dashed")
points(a:b, pred.orig, col="red")

# To zoom the graph, starting from entry 100:
win.graph(width=6.5,height=10); 
ts.plot(close[(length(close.log)-150):(length(close.log))], 
        gpars=list(xlab="Time(last 150 points)",ylab='close'),lty=c(1:3))
lines(U, col="blue", lty="dashed")
lines(L, col="blue", lty="dashed")
a = 150+1
b = 150+12
points(a:b, pred.orig, col="red")



