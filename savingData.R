setwd("C:/Users/aidan/desktop/Time")
library(tseries) # this isn't even used
library(zoo)


# na.locf removes NA and carries forward the most recent non NA value
AAPL <- na.locf(get.hist.quote("AAPL", quote="Adj", start="2005-12-25", retclass="zoo"))
IBM <- na.locf(get.hist.quote("IBM", quote="Adj", start="2005-12-25", retclass="zoo"))


# To make week end prices:
nextfri.Date <- function(x) 7 * ceiling(as.numeric(x - 1)/7) + as.Date(1)
wAAPL <- aggregate(AAPL, nextfri.Date,tail,1)
wIBM <- aggregate(IBM, nextfri.Date,tail,1)

# Convert weekly prices into weekly returns
lwrAAPL <- diff(log(wAAPL)) # convert prices to log returns
lwrIBM <- diff(log(wIBM)) # convert prices to log returns


# Write output data to csv file
write.zoo(wAAPL, file="w_AAPL.csv",sep=",",col.names=c("date","percentReturn"))
write.zoo(wIBM, file="w_IBM.csv",sep=",",col.names=c("date","percentReturn"))



plot(wAAPL)
lines(wIBM)

plot(lwrIBM)
lines(lwrAAPL)

acf(lwrIBM)
acf(lwrAAPL)

# library(devtools)
# devtools::install_github("google/CausalImpact")
library(CausalImpact)

dim(lwrAAPL)
dim(lwrIBM)

