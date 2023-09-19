install.packages("readxl")
library(readxl)

EPI_data<-read.csv("~/DATA ANALYTICS/LAB/EPI/2010EPI_data.csv")

View(EPI_data)

# I noticed that the heading of the table is showing x x x , so the code below will fix it

names(EPI_data)<-as.matrix(EPI_data[1,])
EPI_data <- EPI_data[-1,]
EPI_data[]<- lapply(EPI_data, function(x) type.convert(as.character(x)))
EPI_data


#To view table and check if the heading has been fixed
View(EPI_data)

#sets the 'default' object
attach(EPI_data)


# launches a simple data editor – test it!
fix(EPI_data)

# prints out values
EPI <- EPI_data$EPI

# records True values if the value is NA

#vhhk.bjsnl;m





#okay








