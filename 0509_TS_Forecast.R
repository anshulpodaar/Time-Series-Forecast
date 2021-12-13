###########################################################################
### Forecasting Sales per Depot
###   Created by: Anshul Podaar, 04-May-2018
###########################################################################

###########################################################################
### Clear workspace variables, load forecasting packages and load the data
###########################################################################
rm(list=ls())
library(fpp2)
library(seasonal)
data <- read.csv("Sales.csv")
#plot.new()

###########################################################################
### Analyse data, one Depot at a time (looping depot serial numbers)
###########################################################################
i <- 10
while(i < 47) {
  i = i+1
  
  ###########################################################################
  ### Declare this as time series data using time series (ts) function
  ###########################################################################
  Y <- ts(data[data$Depot==i,3], start=c(2015,4), frequency=12)
  #  Y <- ts(data[data$Depot==i,3], start=c(2015,4,1), frequency=365)  # For daily data over 3 years
  
  message("\n###########################     Depot ",i," begins     ###########################")
  
  ###########################################################################
  ### Analysis of Seasonality mandates that each season requires at least 2 observations.
  ### Check for minimum 2 years of data.
  ###########################################################################
  if(length(Y) < (24)) {
    message("\nInsuffient data to process. Moving to the next Depot.\n")
    #    invisible(readline(prompt="Press [enter] to continue"))
    next
  }
  
  
  ###########################################################################
  ### Preliminary Analysis 
  ### (to understand the underlying principle behind the fpp2 package for time series forecasting)
  ###########################################################################
  
  ### Time Plot of Data
  #  autoplot(Y) + 
  #    ggtitle("Time Plot: Sales of Depot, per month") + 
  #    ylab("Metric Tonnes")
  
  ### Take the first difference of the data to remove the trend component.
  DY <- diff(Y)
  
  ### Time Plot of Difference Data
  #  autoplot(DY) + 
  #    ggtitle("Time Plot: Difference Data by month for Depot") + 
  #    ylab("Metric Tonnes")  
  
  ### Trend appears Stationary. Use series to investigate Seasonality.
  #  ggseasonplot(DY) + 
  #    ggtitle("Seasonal Plot: Change in Monthly Sales by Depot") +
  #    ylab("Metric Tonnes")  
  ### Let's look at another seasonal plot (the seasonal subseries plot)
  #  ggsubseriesplot(DY) + 
  #    ggtitle("Seasonal Sub-series Plot: Average Change by Month for Sales of Depot") +
  #    ylab("Metric Tonnes")  
  
  
  ###########################################################################
  ### Our series Y, has trend and seasonality.
  ### To remove trend, we take the first difference.
  ### The first differenced series still has seasonality.
  
  ### Forecast with various methods
  ###########################################################################
  
  ######################################
  ### Use a benchmark method to forecast. -----> Not so great.
  ### Let's use the Seasonal-Naive method as our benchmark.
  ###     y_t = y_{t-s}+ e_t
  ######################################
  fit_snaive <- snaive(DY)   # Residual SD => (SD = Standard Deviation)
  sd_snaive <- fit_snaive$model$sd
  #  print(sd_snaive)
  #  print(summary(fit_snaive))
  #  checkresiduals(fit_snaive)
  
  
  
  ######################################
  ### Fit ETS model (Exponential Smoothing)   ###  Model used is ETS(A,N,N) ==> ETS(Additive,N,N)
  ######################################
  fit_ets <- ets(Y)   # Residual SD = sqrt(sigma^2)
  sd_ets <- sqrt(fit_ets$sigma2)
  #  print(fit_ets$sigma2)
  #  print(sd_ets)
  #  print(summary(fit_ets))
  #  checkresiduals(fit_ets)
  
  
  
  ######################################
  ### Fit ARIMA model (Only for Stationary data)
  ######################################
  fit_arima <- auto.arima(Y,d=1,D=1, stepwise = FALSE, approximation = FALSE, trace = TRUE)
  #   d=1 --> remove trend, D=1 --> remove seasonality
  sd_arima <- sqrt(fit_arima$sigma2)
  #   Residual SD = sqrt(sigma^2)
  #   print(fit_arima$sigma2)
  #   print(sd_arima)
  #   print(summary(fit_arima))
  #   checkresiduals(fit_arima)
  
  
  
  ######################################
  ### Simple Exponential Smoothing
  ######################################
  fit_ses <- ses(Y, h=6)    ### h=6 --> forecasting 6 months in the future
  sd_ses <- sqrt(fit_ses$model$sigma2)
  
  ######################################
  ### Holt's Linear Trend Method
  ######################################
  fit_holt <- holt(Y, h=6, damped=TRUE)    ### h=6 --> forecasting 6 months in the future
  sd_holt <- sqrt(fit_holt$model$sigma2)
  
  
  ######################################
  ### Calculate model with minimum Residual SD
  ######################################
  if(sd_snaive & sd_ets & sd_arima & sd_ses & sd_holt) {
    if(sd_snaive < sd_ets) {
      fit1 <- fit_snaive
      sd1 <- sd_snaive
    } else {
      fit1 <- fit_ets
      sd1 <- sd_ets
    }
    if(sd_arima < sd_ses) {
      fit2 <- fit_arima
      sd2 <- sd_arima
    } else {
      fit2 <- fit_ses
      sd2 <- sd_ses
    }
    if(sd1 < sd2) {
      if(sd1 < sd_holt) {
        fit <- fit1
        sd <- sd1
      } else {
        fit <- fit_holt
        sd <- sd_holt
      }
    } else {
      if(sd2 < sd_holt) {
        fit <- fit2
        sd <- sd2
      } else {
        fit <- fit_holt
        sd <- sd_holt
      }
    }
  } else {
    print('SD Data not available. moving on to next Depot')
    #    invisible(readline(prompt="Press [enter] to continue"))
    next
  }
  
  ######################################
  ### Forecast with model that has lowest Residual SD value
  ######################################
  fcst <- forecast(fit,h=6)    ### h=24 ===> forecast 24 months into the future
  ### For daily data, use h=7 for 7 days future prediction
  #frame()
  p <- autoplot(fcst, include=60) +      ### include=60 ===> plot upto 60 months in the past
    ### For daily data, use include=(3*365) for including 3 years of data
    ggtitle("Time Plot: Prediction of sales for next 6 months") + 
    ylab("Metric Tonnes")  
  print(p)
  summary(fcst)
  #   checkresiduals(fcst)
  #   invisible(readline(prompt="Press [enter] to continue"))
  
  
  message("\n###########################     Depot ",i," ends     ###########################")
  #  invisible(readline(prompt="Press [enter] to continue"))
  
}
