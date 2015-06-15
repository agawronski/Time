setwd("C:/Users/aidan/desktop/Time")
set.seed(1111)
#This will pull the IBM and Yahoo data using the Quantmod package

library(quantmod)

# tickers <- c("AAPL", "IBM")
# getSymbols(tickers)
# rAAPL <- weeklyReturn(AAPL)
# rIBM <- weeklyReturn(IBM)

tickers <- c("GSPC", "AAPL", "IBM", "GE", "MSFT")

#http://cran.r-project.org/web/packages/quantmod/quantmod.pdf
getSymbols(tickers, src="yahoo")

#head(IBM.uA <- adjustOHLC(IBM, use.Adjusted=TRUE))
#chartSeries(IBM.uA)

GSPC <- adjustOHLC(GSPC, use.Adjusted=TRUE) 
AAPL <- adjustOHLC(AAPL, use.Adjusted=TRUE) 
IBM <- adjustOHLC(IBM, use.Adjusted=TRUE) 
GE <- adjustOHLC(GE, use.Adjusted=TRUE) 
MSFT <- adjustOHLC(MSFT, use.Adjusted=TRUE) 

data <- cbind(GSPC$GSPC.Close, AAPL$AAPL.Close, IBM$IBM.Close, GE$GE.Close, MSFT$MSFT.Close)

#Are there NA's?
apply(is.na(data), 2, sum)

#Apply the if else function the the columns of our data
#if the value is NA, then return the mean, else return the value
#wrapped in xts(          , order.by=time(data)  ) to maintain the dates
data <- xts(apply(data, 2, function(x) ifelse(is.na(x)==TRUE, mean(x,na.rm=TRUE), x)), order.by=time(data))

#------------------------------------------------------------------------


####Augmented Dickey-Fuller (ADF) 
#testing if the data is stationary ... the null is non-stationary
#small p-values we reject the null
#large p-values we fail to reject the null 
#(and should transform the data if we require it to be stationary)

#adf.test requires tseries package
library(tseries)

#this gives the p-value of the stationarity test for each
#less than 0.1 and we can reject the null
#in favor of the alternative, that the data is stationary
#with 10% significance
# ... less then 0.05 and we have 5 percent significance
pVal <- apply(data, 2, function(x) adf.test(x, alternative="stationary")$p.value)

library(forecast) # neeeded for ndiffs


makeStation <- function(data, pVal) {
  
  for (i in 1:length(pVal)) {
    if (i > 0.1) {
      
      ns <- try(nsdiffs(data[,i]), silent=TRUE)
      if(class(ns) == "try-error") { ns <- 0  }
      
      if(ns > 0) {
        #if the data is seasonal we difference it according to nsdiffs
        data[-c(1:ns),i] <- diff(as.numeric(data[,i]),lag=frequency(data[,i]),differences=ns)
      } else {
        nd <- ndiffs(as.numeric(data[,i]))
        if(nd > 0) {
          data[-c(1:nd),i] <- diff(as.numeric(data[,i]),differences=nd)
          data <<- data[-c(1:nd),]
        }
      }
      
    }
  }
  pVal <<- apply(data, 2, function(x) adf.test(x, alternative="stationary")$p.value)
}

makeStation(data, pVal)

#if not all pVals are less than 0,05 then use the makeStation function again
if(sum(pVal < 0.05)!=length(pVal)) { makeStation(data, pVAl) }


#---------------------------------------------------------------------------


######### Everything following doesn't require stationarity

library(nnet)
library(caret)

#subset the last month of data into testing
testing <- data[(nrow(data)-30):nrow(data),]
#training <- data[-testing,]
#For the sake of computation I'm make the training set small
training <- data[-testing,]
training <- training[(nrow(training)-30):nrow(training),]

depVar <- training[,1]
indVar <- training[,-1]

# When you set linout=TRUE you choose the identity function (f(a) = a) 
# as activation function for the output layer, 
# i. e. you actually don't have an activation function in the output layer.

modelData <- data.frame(depVar, indVar)

# netModel <- train(as.numeric(depVar) ~ indVar, method='nnet', data=modelData,linout=TRUE, trace = FALSE,
#                   #Grid of tuning parameters to try:
#                   tuneGrid=expand.grid(.size=c(1,5,10, 15),.decay=c(0,0.001,0.1)))

netModel <- train(as.numeric(depVar) ~ indVar, method='nnet', data=modelData,linout=TRUE, trace = FALSE,
                  #Grid of tuning parameters to try:
                  tuneGrid=expand.grid(.size=c(1, 5, 10, 15),.decay=c(0,0.001,0.1)))

#__________

testingData <- data.frame(testing[,-1])

#using nnet to see if anyting has changed
object <- nnet(as.numeric(depVar) ~ indVar, size=10, decay=0.1)


netPredict <- predict(object, newdata=testingData[,-1])

check <- netPredict*(max(testing[,1]))

# sum of squared errors 
sse1 <- sum((check-testing[,1])^2)

# sum of testing values
salesP1 <- sum(check)



