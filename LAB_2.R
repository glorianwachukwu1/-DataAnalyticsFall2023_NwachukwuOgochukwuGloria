EPI_data<-read.csv("~/DATA ANALYTICS/LAB/EPI/2010EPI_data.csv")

View(EPI_data)

# changing the heading from x x x, to its original name
names(EPI_data)<-as.matrix(EPI_data[1,])
EPI_data <-EPI_data[-1,]
EPI_data[]<- lapply(EPI_data, function(x) type.convert(as.character(x)))
EPI_data

#Cumulative density function

plot(ecdf(EPI),do.points=FALSE,verticals=TRUE)

#Quantile-Quantile