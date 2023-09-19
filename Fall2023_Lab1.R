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
EPI <-EPI_data$EPI

# records True values if the value is NA

tf<-is.na(EPI_data)

View (tf)

# filters out NA values, new array
E<- EPI[!tf]

View (E)



#Exercise 1 - Exploring the distribtion (Histogram diagram of the EPI)

summary(EPI)

fivenum(EPI,na.rm=TRUE)

stem(EPI)

hist(EPI)


hist(EPI,seq(30.,95.,1.0),probability=TRUE)

lines(density(EPI,na.rm=TRUE,bw=1.))

rug(EPI)

help(stem)


#Exercise 1: fitting a distribution beyond histograms

#Diagram of ecdf(EFI)

plot(ecdf(EPI),do.points=FALSE, verticals = TRUE)

par(pty="s")
qqnorm(EPI); qqline(EPI)

x <- seq(30, 95, 1)

#Q-Q plot
qqplot(qt(ppoints(250),df=5),x,xlab = "Q-Q plot for t dsn" )

#line that crosses the plot of the Q-Q
qqline(x)



#Using CLIMATE column for the exercise
CLIMATE <- as.numeric(EPI_data$CLIMATE)
t <- is.na(CLIMATE)
CLIMATE [!t]
summary(CLIMATE)
fivenum(CLIMATE)
stem(CLIMATE)
hist(CLIMATE)

# To create the density plot over a histogram with specified breaks
data_range <- range(CLIMATE, na.rm = TRUE)

# Create a histogram using the breaks
hist(CLIMATE, seq(30., 95., 1.0), prob=TRUE)
lines(density(CLIMATE, na.rm = TRUE, bw=1.))

# For empirical cumulative distribution function (ecdf) of CLIMATE
plot(ecdf(CLIMATE), do.points=FALSE, verticals = TRUE)

# for Quantile-Quantile (Q-Q) plot for a normal distribution
par(pty="s")

qqnorm(CLIMATE); qqline(CLIMATE)

#  To make a Q-Q plot against the generating distribution 
x <- seq(30, 95, 1)

# For t-distribution Q-Q plot (with 5 degrees of freedom)
qqplot(qt(ppoints(250), df=5), x, xlab = "Q-Q plot for t dsn" )

qqline(x)

#Using BIODIVERSITY column for the exercise
BIODIVERSITY <- as.numeric(EPI_data$ BIODIVERSITY)

t <- is.na(BIODIVERSITY)

BIODIVERSITY [!t]

summary(BIODIVERSITY)

fivenum(BIODIVERSITY)

stem(BIODIVERSITY)

hist(BIODIVERSITY)

# To create the density plot over a histogram with specified breaks
data_range <- range(BIODIVERSITY, na.rm = TRUE)

# Create a histogram using the breaks
hist(BIODIVERSITY, seq(30., 95., 1.0), prob=TRUE)
lines(density(BIODIVERSITY, na.rm = TRUE, bw=1.))

# For empirical cumulative distribution function (ecdf) of BIODIVERSITY
plot(ecdf(BIODIVERSITY), do.points=FALSE, verticals = TRUE)

# for Quantile-Quantile (Q-Q) plot for a normal distribution
par(pty="s")

qqnorm(CLIMATE); qqline(CLIMATE)

#  To make a Q-Q plot against the generating distribution 
x <- seq(30, 95, 1)

# For t-distribution Q-Q plot (with 5 degrees of freedom)
qqplot(qt(ppoints(250), df=5), x, xlab = "Q-Q plot for t dsn" )
qqline(x)


help(boxplot)

# boxplot (CLIMATE)
boxplot(CLIMATE, main = "Boxplot of CLIMATE", ylab = "Values")


HELP(boxplot)
# Boxplot (BIODIVERSITY)
boxplot(BIODIVERSITY, main = "Boxplot of BIODIVERSITY", ylab = "Values")


#comparing Climate and Biodiversity distribution

#comparing with boxplot
boxplot(CLIMATE,BIODIVERSITY)

#comparing with qqlot
qqplot(CLIMATE,BIODIVERSITY)

#From exercise 1: Creating boxplot of Daly and EPI
boxplot(EPI, DALY, names=c("EPI", "DALY"), main="Boxplot of EPI and DALY")

# Getting  the quantiles of EPI and DALY
qEPI <- quantile(EPI, probs=ppoints(length(EPI)), na.rm = TRUE)
qDALY <- quantile(DALY, probs=ppoints(length(DALY)), na.rm = TRUE)

# The Q-Q plot of EPI and DALY
qqplot(qEPI, qDALY, xlab="Q-Q plot of EPI", ylab="Q-Q plot of DALY", main="Q-Q plot of EPI vs DALY")


#Intercomparing  to converting 
EPI_data <- type.convert(EPI_data, as.is = TRUE)


variables <- c("EPI", "ENVHEALTH", "ECOSYSTEM", "DALY", "AIR_H", "WATER_H", "WATER_E", "BIODIVERSITY")

for(var in variables) {EPI_data[[var]] <- as.numeric(EPI_data[[var]])}

boxplot(EPI_data[, variables], las=2)
title("Combined Boxplot")

#for Histogram

par(mfrow=c(2, 4), mar=c(4, 4, 2, 1))

for (var in variables) {valid_values <- EPI_data[[var]][!is.na(EPI_data[[var]]) & !is.infinite(EPI_data[[var]])]
  hist(valid_values, main=var, xlab=var, probability = TRUE, breaks=20, col='yellow', border='pink')}

##Using Kernel Density Plots for the distributions
for (var in variables) {valid_values <- EPI_data[[var]][!is.na(EPI_data[[var]]) & !is.infinite(EPI_data[[var]])]
  dens <- density(valid_values)
  plot(dens, main=var, xlab=var, col='blue', lwd=2)
  polygon(dens, col="skyblue", border="blue")
}

##Filtering

EPILand <- EPI_data$EPI[EPI_data$Landlock == 1]
ELand <- EPILand[!is.na(EPILand)]
##Plotting the histogram
hist(ELand, main = "Histogram of ELand", xlab = "ELand")
hist(ELand, breaks = seq(30., 95., 1.0), prob = TRUE, main = "Histogram of ELand with Specified Breaks", xlab = "ELand")

##Boxplot
boxplot(ELand, main = "Boxplot of ELand", ylab = "ELand")


## Plotting for No_surface water, desert and high_population

variable_names <- c('No_surface_water', 'Desert', 'High_Population_Density')

for (variable_name in variable_names) {
for (condition in 0:1) {

# Filtering the values based on the condition (0 or 1)
VarLand <- EPI_data$EPI[EPI_data[[variable_name]] == condition]
    
# Boxplot
boxplot(ELand, main = paste("Boxplot of", variable_name, "under condition", condition), ylab = variable_name)
    
# Histogram with default breaks
hist(ELand, main = paste("Histogram of", variable_name, "under condition", condition), xlab = variable_name)
    
# Histogram with specified breaks and density
hist(ELand, breaks = seq(30., 95., 1.0), probability = TRUE, main = paste("Histogram of", variable_name, "with specified Breaks under condition", condition), xlab = variable_name)
    
# Q-Q plot
qqnorm(ELand, main = paste("Q-Q Plot of", variable_name, "under condition", condition))
qqline(ELand, col = "yellow")
  }
}

# Filter data for an EPI region
EPI_region_data1 <- subset(EPI_data, EPI_regions == 'Europe')
View(EPI_region_data1)

# Filter data for a  GEO subregion
GEO_subregion_data <- subset(EPI_data, GEO_subregion == 'North America')
View(GEO_subregion_data)

# Viewing summary statistics for the EPI column for the region and subregion
summary(EPI_region_data1$EPI)
summary(GEO_subregion_data$EPI)

# region or subregion
hist(EPI_region_data1$EPI, main = "Histogram for North America EPI Region", xlab = "EPI")
hist(GEO_subregion_data$EPI, main = "Histogram for South America GEO Subregion", xlab = "EPI")
install.packages("raster")
install.packages("rgdal")
install.packages("sf")

EPI_data<-read.csv("~/DATA ANALYTICS/LAB/EPI/ GPW3_GRUMP_SummaryInformation_2010.csv")
View(GPW3_GRUMP)

# summary for PopulationPerUnit
summary(GPW3_GRUMP$PopulationPerUnit)

# boxplot for PopulationPerUnit
boxplot(GPW3_GRUMP$PopulationPerUnit, main="Boxplot of PopulationPerUnit", ylab="Values")

# histogram for "PopulationPerUnit"
hist(GPW3_GRUMP$PopulationPerUnit, main="Histogram of PopulationPerUnit", xlab="Values")

# Q-Q plot for "PopulationPerUnit"
qqnorm(GPW3_GRUMP$PopulationPerUnit, main="Q-Q Plot of PopulationPerUnit")
qqline(GPW3_GRUMP$PopulationPerUnit, col="brown")

# Filtering 
specific_continent_data <- subset(GPW3_GRUMP, ContinentName == 'North America')
View(specific_continent_data )

#Water treatment
EPI_data<-read.csv("~/DATA ANALYTICS/LAB/EPI/ water-treatment")
View(water_treatment)

# Get summary statistics for "PH-E"
summary(water_treatment$`PH-E`)
# boxplot for "PH-E"
boxplot(water_treatment$`PH-E`, main="Boxplot of PH-E", ylab="Values")

# histogram for "PH-E"
hist(water_treatment$`PH-E`, main="Histogram of PH-E", xlab="Values", breaks=20)


# Filtering of PH-E values
specific_range_data <- subset(water_treatment, `PH-E` >= 7 & `PH-E` <= 8)
View(specific_range_data)

# Q-Q plot for "PH-E"
qqnorm(water_treatment$`PH-E`, main="Q-Q Plot of PH-E")
qqline(water_treatment$`PH-E`, col="red")


                    





