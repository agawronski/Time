winback <- as.data.frame(sample(1000, 100, replace=FALSE))
colnames(winback) <- c("contact_ID")

ordersDetail <- as.data.frame(sample(2000, 300, replace=FALSE))
colnames(ordersDetail) <- c("CONTACT_ID")

ordersDetail$Total <- sample(500, 300, replace=TRUE)
ordersDetail$ORDERNUMBER <- sample(300, 300, replace=FALSE)

ordersDetail <- ordersDetail[order(ordersDetail[,"ORDERNUMBER"]),]
ordersDetail$ORDERDATE <- seq(as.Date("2011/1/5"), as.Date("2016/9/28"), by = "week")
