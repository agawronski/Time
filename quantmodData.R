setwd("C:/Users/aidan/desktop/Time")

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


chartSeries(IBM)
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
