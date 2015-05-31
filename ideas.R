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

chartSeries(log(AAPL))
chartSeries(AAPL)

####Creating lagged variables and seasonal dummies

AAPL$AAPL.Close1 <- lag(AAPL$AAPL.Close, 1)
AAPL$AAPL.Close2 <- lag(AAPL$AAPL.Close, 2)
AAPL$AAPL.Close3 <- lag(AAPL$AAPL.Close, 3)
AAPL$AAPL.Close4 <- lag(AAPL$AAPL.Close, 4)
AAPL$AAPL.Close5 <- lag(AAPL$AAPL.Close, 5)

IBM$IBM.Close1 <- lag(IBM$IBM.Close, 1)
IBM$IBM.Close2 <- lag(IBM$IBM.Close, 2)
IBM$IBM.Close3 <- lag(IBM$IBM.Close, 3)
IBM$IBM.Close4 <- lag(IBM$IBM.Close, 4)
IBM$IBM.Close5 <- lag(IBM$IBM.Close, 5)

AAPL$AAPL.Interest1 <- lag(AAPL$AAPL.Interest, 1)
AAPL$AAPL.Interest2 <- lag(AAPL$AAPL.Interest, 2)
AAPL$AAPL.Interest3 <- lag(AAPL$AAPL.Interest, 3)
AAPL$AAPL.Interest4 <- lag(AAPL$AAPL.Interest, 4)
AAPL$AAPL.Interest5 <- lag(AAPL$AAPL.Interest, 5)

IBM$IBM.Interest1 <- lag(IBM$IBM.Interest, 1)
IBM$IBM.Interest2 <- lag(IBM$IBM.Interest, 2)
IBM$IBM.Interest3 <- lag(IBM$IBM.Interest, 3)
IBM$IBM.Interest4 <- lag(IBM$IBM.Interest, 4)
IBM$IBM.Interest5 <- lag(IBM$IBM.Interest, 5)

library(lubridate)

IBM$jan <- ifelse(month(IBM)==1, 1, 0)
IBM$feb <- ifelse(month(IBM)==2, 1, 0)
IBM$mar <- ifelse(month(IBM)==3, 1, 0)
IBM$apr <- ifelse(month(IBM)==4, 1, 0)
IBM$may <- ifelse(month(IBM)==5, 1, 0)
IBM$jun <- ifelse(month(IBM)==6, 1, 0)
IBM$jul <- ifelse(month(IBM)==7, 1, 0)
IBM$aug <- ifelse(month(IBM)==8, 1, 0)
IBM$sep <- ifelse(month(IBM)==9, 1, 0)
IBM$oct <- ifelse(month(IBM)==10, 1, 0)
IBM$nov <- ifelse(month(IBM)==11, 1, 0)
IBM$dec <- ifelse(month(IBM)==12, 1, 0)


collect1 <- AAPL[,c("AAPL.Close1", "AAPL.Close2", "AAPL.Close3", "AAPL.Close4", "AAPL.Close5")]

collect2 <- IBM[,c("IBM.Close1", "IBM.Close2", "IBM.Close3", "IBM.Close4", "IBM.Close5")]

collect3 <- IBM[,c("IBM.Interest1", "IBM.Interest2", "IBM.Interest3", "IBM.Interest4", "IBM.Interest5")] 

collect4 <- IBM[,c("feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")]

data <- cbind(collect1, collect2, collect3, collect4)

fit <- lm(IBM$IBM.Close ~ data)
summary(fit)

data2 <- cbind(collect1, collect2, collect4)

fit2 <- lm(IBM$IBM.Close ~ data2)
summary(fit2)



####Trying pure prediction for the heck of it

library(caret)
library(ggplot2)



myTimeControl <- trainControl(method = "timeslice",
                              initialWindow = 36,
                              horizon = 12,
                              fixedWindow = TRUE)

plsFitTime <- train(unemploy ~ pce + pop + psavert,
                    data = economics,
                    method = "pls",
                    preProc = c("center", "scale"),
                    trControl = myTimeControl)

