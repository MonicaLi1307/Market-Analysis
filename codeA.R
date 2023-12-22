library(feasts)
library(tidyverse)
library(lubridate)
library(TSA)

data <- read.csv('StockArchive/BPCL.csv',encoding="UTF-8")
data$Date <- ymd(data$Date)
data_tsi <- as_tsibble(data, index=Date)


plot(data$Close,type='l',xlab='Time',ylab='Close')
periodogram(data$Close)


#data_tsi %>%
#gg_season(data_tsi$Close)

diff = data$Close[2:length(data$Close)] - data$Close[1:length(data$Close)-1]
diff_mean = mean(diff)
diff_var = var(diff)
qq <- quantile(diff)
sig <- 1.5 * (qq[4]-qq[2])

plot(diff,type='l',xlab='Time',ylab='Difference Close')
par(new = TRUE)
plot(diff(which(diff>=sig)),col = "red",ylab='')
par(new = TRUE)
plot(diff(which(diff<=-sig)),col = "red",ylab='')

