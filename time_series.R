
#timeseries forecasting
visitors <- read.csv(file.choose())

#shows top 5 data
head(visitors, 5)

#statistical summary of the dataset
summary(visitors)

#converting class of dataset to ts
visitors_ts <- ts(visitors[, -1], frequency = 4, start=c(1998, 1))

#to check the class of dataset
class(visitors_ts)

#to plot graph to understand the trend
plot(visitors_ts)

#to fit in a linear line
abline(reg = lm(visitors_ts~time(visitors_ts)))

plot(aggregate(visitors_ts, FUN=mean))

#to make variance equal
plot(log(visitors_ts))

#to make mean constant
plot(diff(log(visitors_ts)))

#to plot acf graph to determine the value of q
acf(diff(log(visitors_ts)))

#to plot pacf graph to determine the value of p
pacf(diff(log(visitors_ts)))

#to determine the ARIMA model
fit <- arima(log(visitors_ts), c( 0, 1, 1 ), seasonal = list(order=c( 0, 1, 1 ), period=4))

#to forecast for 10 years
pred <- predict(fit, n.ahead=10*4)

#e^pred
pred1 <- 2.718^pred$pred

#plotting graph 
ts.plot(visitors_ts, pred1, log='y', lty=c(1, 3))

#Testing our model

visitors_model <- ts(visitors_ts, frequency = 4, start=c(1998, 1), end=c(2011, 4))

fit <- arima(log(visitors_model), c(0, 1, 1), seasonal = list(order=c(0, 1, 1), period=4))

pred2 <- predict(fit, n.ahead=10)

pred3 <- 2.718^pred$pred

data1 <- head(pred3, 4)

data2 <- head(pred1, 4)
