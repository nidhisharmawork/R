#Initiating 'forecast' library
library(forecast)
library(zoo)

#Setting working directory
setwd("C:/Users/nidhi/Downloads/BAN 673 Files")

#Creating dataframe
Case1.data <- read.csv("673_case1.csv")

#Seeing first and last six records of the file
head(Case1.data)
tail(Case1.data)

#Question 1

#1(a)Creating time series data set sales.ts in R using the ts() function.
sales.ts <-ts(Case1.data$Sales, 
              start = c(2015, 1), end = c(2023, 12), freq = 12)
sales.ts

sales.stl <- stl(sales.ts, s.window = "periodic")

autoplot(sales.stl, main = "Grocery Store Chain Time Series Components")

#1(b)Employ the plot() function to create a data plot of the historical data.

plot(sales.ts, 
     xlab = "Time", ylab = "Sales (in million,$)", 
     ylim = c(100, 500), xaxt = 'n',
     main = "Grocery Store Chain Sales")

# Establish x-axis scale interval for time in months.
axis(1, at = seq(2015, 2024, 1), labels = format(seq(2015, 2024, 1)))


#1(c)Apply the Acf() function to identify possible time series components.
autocor <- Acf(sales.ts, lag.max = 12, 
               main = "Autocorrelation for Grocery Store Chain Sales")

#Displaying autocorrelation coefficients for various lags.
Lag <- round(autocor$lag, 0)
ACF <- round(autocor$acf, 3)
data.frame(Lag, ACF)

#Question 2

#2(a) Data partition with the validation partition of 24 monthly periods (2 years) 
#and training partition of 84 monthly periods (7 years)
nValid <- 24
nTrain <- length(sales.ts) - nValid
nTrain
train.ts <- window(sales.ts, start = c(2015, 1), end = c(2015, nTrain))
valid.ts <- window(sales.ts, start = c(2015, nTrain + 1), 
                   end = c(2015, nTrain + nValid))

#2(b)
# Using rollmean() to create trailing moving average with window widths 
#of k = 4, 6, and 12.
ma.trailing_4 <- rollmean(train.ts, k = 4, align = "right")
ma.trailing_6 <- rollmean(train.ts, k = 6, align = "right")
ma.trailing_12 <- rollmean(train.ts, k = 12, align = "right")

#2(c)
#Using forecast() to create a trailing MA forecast for each window width 
ma.trail_4.pred <- forecast(ma.trailing_4, h = nValid, level = 0)
ma.trail_4.pred

ma.trail_6.pred <- forecast(ma.trailing_6, h = nValid, level = 0)
ma.trail_6.pred

ma.trail_12.pred <- forecast(ma.trailing_12, h = nValid, level = 0)
ma.trail_12.pred

#2(d)
#Using accuracy() function to identify common accuracy measures.

round(accuracy(ma.trail_4.pred$mean, valid.ts), 3)
round(accuracy(ma.trail_6.pred$mean, valid.ts), 3)
round(accuracy(ma.trail_12.pred$mean, valid.ts), 3)

#3(a)
#Using tslm() to develop a regression model with linear trend and seasonality
trend.seas <- tslm(train.ts ~ trend  + season)
summary(trend.seas)

trend.seas.pred <- forecast(trend.seas, h = nValid, level = 0)
trend.seas.pred

#3(b)
#Identifying regression residuals in the training period 
#Applying a trailing MA  (window width of 3)

#Residuals in training period.
trend.seas.res <- trend.seas$residuals
trend.seas.res

#Applying trailing MA for residuals with window width k = 3. 
ma.trail.res <- rollmean(trend.seas.res, k = 3, align = "right")
ma.trail.res

#Residuals in validation period.
trend.seas.res.valid <- valid.ts - trend.seas.pred$mean
trend.seas.res.valid

#Residuals forecast for validation period.
ma.trail.res.pred <- forecast(ma.trail.res, h = nValid, level = 0)
ma.trail.res.pred

#3(c)
#Developing two-level forecast for validation period 
#by combining regression forecast and trailing MA forecast for residuals
fst.2level <- trend.seas.pred$mean + ma.trail.res.pred$mean
fst.2level

#For validation period: validation data, regression, forecast, 
#trailing MA for residuals and total forecast.
valid.df <- data.frame(valid.ts, trend.seas.pred$mean, 
                       ma.trail.res.pred$mean, 
                       fst.2level)
names(valid.df) <- c("Sales", "Regression.Fst", 
                     "MA.Residuals.Fst", "Combined.Fst")
valid.df

#Using accuracy() function to identify common accuracy measures.
#Useing round() function to round accuracy measures to three decimal digits.
round(accuracy(trend.seas.pred$mean, valid.ts), 3)
round(accuracy(fst.2level, valid.ts), 3)

# 3(d)
#Identifying the regression model with linear trend and seasonality
#and trailing MA with the window width of 3 for the regression residuals
tot.trend.seas <- tslm(sales.ts ~ trend + season)
summary(tot.trend.seas)

#Regression forecast for future 12 periods.
tot.trend.seas.pred <- forecast(tot.trend.seas, h = 12, level = 0)
tot.trend.seas.pred

#Regression residuals for entire data set.
tot.trend.seas.res <- tot.trend.seas$residuals
tot.trend.seas.res

#Using trailing MA to forecast residuals for entire data set.
tot.ma.trail.res <- rollmean(tot.trend.seas.res, k = 3, align = "right")
tot.ma.trail.res

#Forecasting for trailing MA residuals for 12 periods.
tot.ma.trail.res.pred <- forecast(tot.ma.trail.res, h = 12, level = 0)
tot.ma.trail.res.pred

#Developing a 2-level forecast for future 12 periods 
tot.fst.2level <- tot.trend.seas.pred$mean + tot.ma.trail.res.pred$mean
tot.fst.2level

#Creating a table with regression forecast, trailing MA for residuals,
#total forecast for future 12 periods.
future12.df <- data.frame(tot.trend.seas.pred$mean, tot.ma.trail.res.pred$mean, 
                          tot.fst.2level)
names(future12.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
future12.df

# 3(e)
#Developing a seasonal naïve forecast for the entire historical data set 
#and applying accuracy() to compare accuracy of the three forecasting models.
round(accuracy(tot.trend.seas.pred$fitted, sales.ts), 3)
round(accuracy(tot.trend.seas.pred$fitted+tot.ma.trail.res, sales.ts), 3)
round(accuracy((snaive(sales.ts))$fitted, sales.ts), 3)


#Question 4

# 4(a)
#Using ets() to develop a Holt-Winter’s (HW) model with 
#automated selection of error, trend, and seasonality options,
#and automated selection of smoothing parameters for the training partition.
#Using optimal alpha, beta, & gamma to fit HW over the training period.
hw.ZZZ <- ets(train.ts, model = "ZZZ")
hw.ZZZ 

hw.ZZZ.pred <- forecast(hw.ZZZ, h = nValid, level = 0)
hw.ZZZ.pred

# 4(b)
#Making a forecast in the 12 months of 2024
HW.ZZZ <- ets(sales.ts, model = "ZZZ")
HW.ZZZ 

#Using forecast() function to make predictions using the model for 12 months
HW.ZZZ.pred <- forecast(HW.ZZZ, h = 12 , level = 0)
HW.ZZZ.pred

# 4(c)
#Applying the accuracy() function to compare the two models
round(accuracy(snaive(sales.ts)$fitted, sales.ts), 3)
round(accuracy(HW.ZZZ.pred$fitted, sales.ts), 3)

