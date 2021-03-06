setwd("C:/Users/aidan/desktop/Time")
set.seed(1111)
#This will pull the IBM and Yahoo data using the Quantmod package

library(quantmod)

# tickers <- c("AAPL", "IBM")
# getSymbols(tickers)
# rAAPL <- weeklyReturn(AAPL)
# rIBM <- weeklyReturn(IBM)

#http://cran.r-project.org/web/packages/quantmod/quantmod.pdf
getSymbols("AAPL", src="yahoo")
head(AAPL)
#Not quite sure what the difference is between these methods of adjustment. Use .uA
head(AAPL.a <- adjustOHLC(AAPL)) 
head(AAPL.uA <- adjustOHLC(AAPL, use.Adjusted=TRUE))
#Adjusted for stock splits and dividends AAPL.uA (I think!)
chartSeries(AAPL.uA)


#chartSeries(IBM)
getSymbols("IBM", src="yahoo")
head(IBM)
head(IBM.a <- adjustOHLC(IBM))
head(IBM.uA <- adjustOHLC(IBM, use.Adjusted=TRUE))
chartSeries(IBM.uA)

AAPL <- AAPL.uA
IBM <- IBM.uA

#Add fake "interest level" variable. Sampling between 1:50

AAPL$AAPL.Interest <- sample(50, size=nrow(AAPL), replace=TRUE)
IBM$IBM.Interest <- sample(50, size=nrow(IBM), replace=TRUE)

reg <- lm(AAPL$AAPL.Close ~ AAPL$AAPL.Interest)
summary(reg)

# playing around with the lag function:
# 
# head(AAPL$AAPL.Interest)
# head(AAPL$AAPL.Interest <- lag(AAPL$AAPL.Interest, k=-1))
# tail(AAPL$AAPL.Interest)
# AAPL <- AAPL[-nrow(AAPL),]

#note, the above pushes the data the wrong way, k should be positive for lagged values

chartSeries(log(AAPL))
chartSeries(AAPL)

#Stabalize the variance with log
AAPL$AAPL.Close <- log(AAPL$AAPL.Close)
IBM$IBM.Close <- log(IBM$IBM.Close)


library(vars) #why did I load this package?

####Augmented Dickey-Fuller (ADF) 
#testing if the data is stationary ... the null is non-stationary
#small p-values we reject the null
#large p-values we fail to reject the null 
#(and should transform the data if we require it to be stationary)

#adf.test requires tseries package
library(tseries)
adf.test(AAPL$AAPL.Close, alternative="stationary")
adf.test(IBM$IBM.Close, alternative="stationary")
#both appear to require differencing


# https://www.otexts.org/fpp/8/1
# The following code can be used to find how to make a seasonal series stationary. 
# The resulting series stored as xstar has been differenced appropriately.
# 
# ns <- nsdiffs(x)
# if(ns > 0) {
#   xstar <- diff(x,lag=frequency(x),differences=ns)
# } else {
#   xstar <- x
# }
# nd <- ndiffs(xstar)
# if(nd > 0) {
#   xstar <- diff(xstar,differences=nd)
# }
#   
# ***nsdiffs & ndiffs require the forcast package
library(forecast)

# Trying nsdiffs on both AAPL and IBM implies that they are non seasonal

ndiffs(AAPL$AAPL.Close)
AAPL$AAPL.Close <- diff(AAPL$AAPL.Close, differences=1)
AAPL <- AAPL[-1,] # remove first row because NA introduced from differencing
adf.test(AAPL$AAPL.Close, alternative="stationary") # stationary yahoo!
plot(AAPL$AAPL.Close) # variance doesn't appear perfectly constant 
acf(AAPL$AAPL.Close)


ndiffs(IBM$IBM.Close)
IBM$IBM.Close <- diff(IBM$IBM.Close, differences=1)
IBM <- IBM[-1,]
adf.test(IBM$IBM.Close, alternative="stationary")
plot(IBM$IBM.Close)
acf(IBM$IBM.Close)


#To preserve interpretability use the same differencing for all var
#according to https://www.otexts.org/fpp/9/1 
#I have not done this here, just taking note


fit <- Arima(IBM$IBM.Close, xreg=AAPL$AAPL.Close, order=c(2,0,0))
tsdisplay(arima.errors(fit), main="ARIMA errors")

#At this point we look at the plot (? I think?) to determine possible candidates for 
#the ARIMA model. After testing various options we choose the one with the lowest AIC
#then the model is refit with those specifications and the errors are obtained

fit2 <- Arima(IBM$IBM.Close, xreg=AAPL$AAPL.Close, order=c(1,0,2))
fit2

Box.test(residuals(fit2), fitdf=5, lag=10, type="Ljung")
















