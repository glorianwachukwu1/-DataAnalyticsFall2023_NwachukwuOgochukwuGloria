#Assignment_3_NWACHUKWU_OGOCHUKWU_GLORIA


#Libraries to use
library(ggplot2)
library(dplyr)
library(styler)
library(nortest)



#import nyt3, nyt4, nyt5, nyt6, nyt7, nyt8, nyt9 dataset
library(readr)

#nyt3
nyt3 <- read_csv("DATA_ANALYTICS/Assignment/Assignment 3/nytimes/nyt3.csv")
View(nyt3)

#nyt4
nyt4 <- read_csv("DATA_ANALYTICS/Assignment/Assignment 3/nytimes/nyt4.csv")
View(nyt4)

#nyt5
nyt5 <- read_csv("DATA_ANALYTICS/Assignment/Assignment 3/nytimes/nyt5.csv")
View(nyt5)


#nyt6
nyt6 <- read_csv("DATA_ANALYTICS/Assignment/Assignment 3/nytimes/nyt6.csv")
View(nyt6)

#nyt7
nyt7 <- read_csv("DATA_ANALYTICS/Assignment/Assignment 3/nytimes/nyt7.csv")
View(nyt7)

#nyt8
nyt8 <- read_csv("DATA_ANALYTICS/Assignment/Assignment 3/nytimes/nyt8.csv")
View(nyt8)

#nyt9
nyt9 <- read_csv("DATA_ANALYTICS/Assignment/Assignment 3/nytimes/nyt9.csv")
View(nyt9)

#to clean my data and remove missing values
nyt3 <- na.omit(nyt3)
nyt4 <- na.omit(nyt4)
nyt5 <- na.omit(nyt5)
nyt6 <- na.omit(nyt6)
nyt7 <- na.omit(nyt7)
nyt8 <- na.omit(nyt8)
nyt9 <- na.omit(nyt9)

#Boxplot of nyt3, nyt4, nyt5, nyt6, nyt7, nyt8, nyt9 with Age and Impressions


#boxplot nyt3
head(nyt3)
boxplot (nyt3$Age, nyt3$Impressions,
         names=c("Age","Impressions"), 
         main = "Boxplot of nyt3 Age and Impressions",
         col = c("yellow", "blue"))

summary(nyt3$Age)
summary(nyt3$Impressions)


#boxplot of nyt4
head(nyt4)
boxplot (nyt4$Age, nyt4$Impressions,
         names=c("Age","Impressions"), 
         main = "Boxplot of nyt4 Age and Impressions",
         col = c("green", "pink"))
summary(nyt4$Age)
summary(nyt4$Impressions)


#boxplot of nyt5
head(nyt5)
boxplot (nyt5$Age, nyt5$Impressions,
         names=c("Age","Impressions"), 
         main = "Boxplot of nyt5 Age and Impressions",
         col = c("purple", "brown"))
summary(nyt5$Age)
summary(nyt5$Impressions)


#boxplot of  nyt6
head(nyt6)
boxplot (nyt6$Age, nyt6$Impressions,
         names=c("Age","Impressions"), 
         main = "Boxplot of nyt6 Age and Impressions",
         col = c("red", "black"))
summary(nyt6$Age)
summary(nyt6$Impressions)


#boxplot of nyt7
head(nyt7)
boxplot (nyt7$Age, nyt7$Impressions,
         names=c("Age","Impressions"), 
         main = "Boxplot of nyt7 Age and Impressions",
         col = c("blue", "yellow"))
summary(nyt7$Age)
summary(nyt7$Impressions)


#boxplot of nyt8
head(nyt8)
boxplot (nyt8$Age, nyt8$Impressions,
         names=c("Age","Impressions"), 
         main = "Boxplot of nyt8 Age and Impressions",
         col = c("green", "blue"))
summary(nyt8$Age)
summary(nyt8$Impressions)


#boxplot nyt9
head(nyt9)
boxplot (nyt9$Age, nyt9$Impressions,
         names=c("Age","Impressions"), 
         main = "Boxplot of nyt9 Age and Impressions",
         col = c("orange", "purple"))
summary(nyt9$Age)
summary(nyt9$Impressions)



# b)normality test

dim(nyt3)

#Used Anderson Darling test to check for normality, could not use Shapiro Wilk because dataset is too large and above 3 and 5000 specified for Shapiro Wilktest

#Anderson-Darling test for Age and Impressions

#nyt3
ad_nyt3  <- ad.test(nyt3$Age)
print(ad_nyt3)

ad_nyt3  <- ad.test(nyt3$Impressions)
print(ad_nyt3)


#nyt4
ad_nyt4  <- ad.test(nyt4$Age)
print(ad_nyt4)

ad_nyt4  <- ad.test(nyt4$Impressions)
print(ad_nyt4)


#nyt5
ad_nyt5  <- ad.test(nyt5$Age)
print(ad_nyt5)

ad_nyt5 <- ad.test(nyt5$Impressions)
print(ad_nyt5)


#nyt6
ad_nyt6 <- ad.test(nyt6$Age)
print(ad_nyt6)

ad_nyt6  <- ad.test(nyt6$Impressions)
print(ad_nyt6)

#nyt7
ad_nyt7 <- ad.test(nyt7$Age)
print(ad_nyt7)

ad_nyt7 <- ad.test(nyt7$Impressions)
print(ad_nyt7)


#nyt8
ad_nyt8  <- ad.test(nyt8$Age)
print(ad_nyt8)	

ad_nyt8  <- ad.test(nyt8$Impressions)
print(ad_nyt8)

#nyt9
ad_nyt9 <- ad.test(nyt9$Age)
print(ad_nyt9)

ad_nyt9  <- ad.test(nyt9$Impressions)
print(ad_nyt9)


#Histogram (Used binwidth = 4, i used breaks parameter to specify bin width)

#nyt3 (Age and Impressions)
hist(nyt3$Age, main = "Histogram for nyt3 Age", col = "yellow", breaks = seq(min(nyt3$Age), max(nyt3$Age) + 4, by = 4))

hist(nyt3$Impressions, main = "Histogram for nyt3 Impressions", col = "yellow", breaks = seq(min(nyt3$ Impressions), max(nyt3$ Impressions) + 4, by = 4))


# nyt4(Age and Impressions)
hist(nyt4$Age, main = "Histogram for nyt4 Age", col = "pink", breaks = seq(min(nyt4$Age), max(nyt4$Age) + 4, by = 4))

hist(nyt4$Impressions, main = "Histogram for nyt4 Impressions ", col = "pink", breaks = seq(min(nyt4$Impressions), max(nyt4$Impressions) + 4, by = 4))


# nyt5(Age and Impressions)
hist(nyt5$Age, main = "Histogram for nyt5 Age", col = "purple", breaks = seq(min(nyt5$Age), max(nyt5$Age) + 4, by = 4))

hist(nyt5$Impressions, main = "Histogram for nyt5 Impressions ", col = "purple", breaks = seq(min(nyt5$Impressions), max(nyt5$Impressions) + 4, by = 4))


# nyt6(Age and Impressions)
hist(nyt6$Age, main = "Histogram for nyt6 Age", col = "red", breaks = seq(min(nyt6$Age), max(nyt6$Age) + 4, by = 4))

hist(nyt6$Impressions, main = "Histogram for nyt6 Impressions ", col = "red", breaks = seq(min(nyt6$Impressions), max(nyt6$Impressions) + 4, by = 4))


# nyt7 (Age and Impressions)
hist(nyt7$Age, main = "Histogram for nyt7 Age", col = "blue", breaks = seq(min(nyt7$Age), max(nyt7$Age) + 4, by = 4))

hist(nyt7$Impressions, main = "Histogram for nyt7 Impressions ", col = "blue", breaks = seq(min(nyt7$Impressions), max(nyt7$Impressions) + 4, by = 4))


# nyt8(Age and Impressions)
hist(nyt8$Age, main = "Histogram for nyt8 Age", col = "green", breaks = seq(min(nyt8$Age), max(nyt3$Age) + 4, by = 4))

hist(nyt8$Impressions, main = "Histogram for nyt8 Impressions ", col = "green", breaks = seq(min(nyt8$Impressions), max(nyt8$Impressions) + 4, by = 4))


# nyt9 (Age and Impressions)
hist(nyt9$Age, main = "Histogram for nyt9 Age", col = "orange", breaks = seq(min(nyt9$Age), max(nyt9$Age) + 4, by = 4))

hist(nyt9$Impressions, main = "Histogram for nyt9 Impressions ", col = "orange", breaks = seq(min(nyt9$Impressions), max(nyt9$Impressions) + 4, by = 4))




#c)ECDFs (Empirical Cumulative Distribution Function)) for nyt3, nyt4, nyt5, nyt6, nyt7, nyt8, nyt9 for Age and Impressions

#ECDF for nyt3 Age
ecdf_age <- ecdf(nyt3$Age)
plot(ecdf_age, main = "ECDF for nyt3 Age", xlab = "Age", ylab = "y")

#ECDF for nyt3 Impressions
ecdf_impressions <- ecdf(nyt3$Impressions)
plot(ecdf_impressions, main = "ECDF for nyt3 Impressions", xlab = "Impressions", ylab = "y")


#ECDF for nyt4 Age
ecdf_age <- ecdf(nyt4$Age)
plot(ecdf_age, main = "ECDF for nyt4 Age", xlab = "Age", ylab = "y", col = "pink")

#ECDF for nyt4 Impressions
ecdf_impressions <- ecdf(nyt4$Impressions)
plot(ecdf_impressions, main = "ECDF for nyt4 Impressions", xlab = "Impressions", ylab = "y", col = "pink")


#ECDF for nyt5 Age
ecdf_age <- ecdf(nyt5$Age)
plot(ecdf_age, main = "ECDF for nyt5 Age", xlab = "Age", ylab = "y", col = "purple")

#ECDF for nyt5 Impressions
ecdf_impressions <- ecdf(nyt5$Impressions)
plot(ecdf_impressions, main = "ECDF for nyt5 Impressions", xlab = "Impressions", ylab = "y", col = "purple")


#ECDF for nyt6 Age
ecdf_age <- ecdf(nyt6$Age)
plot(ecdf_age, main = "ECDF for nyt6 Age", xlab = "Age", ylab = "y", col = "red")

#ECDF for nyt6 Impressions
ecdf_impressions <- ecdf(nyt6$Impressions)
plot(ecdf_impressions, main = "ECDF for nyt6 Impressions", xlab = "Impressions", ylab = "y", col = "red")


#ECDF for nyt7 Age
ecdf_age <- ecdf(nyt7$Age)
plot(ecdf_age, main = "ECDF for nyt7 Age", xlab = "Age", ylab = "y", col = "blue")

#ECDF for nyt7 Impressions
ecdf_impressions <- ecdf(nyt7$Impressions)
plot(ecdf_impressions, main = "ECDF for nyt7 Impressions", xlab = "Impressions", ylab = "y", col = "blue")


#ECDF for nyt8 Age
ecdf_age <- ecdf(nyt8$Age)
plot(ecdf_age, main = "ECDF for nyt8 Age", xlab = "Age", ylab = "y", col = "green")

#ECDF for nyt8 Impressions
ecdf_impressions <- ecdf(nyt8$Impressions)
plot(ecdf_impressions, main = "ECDF for nyt8 Impressions", xlab = "Impressions", ylab = "y", col = "green")


#ECDF for nyt9 Age
ecdf_age <- ecdf(nyt9$Age)
plot(ecdf_age, main = "ECDF for nyt9 Age", xlab = "Age", ylab = "y", col = "orange")

#ECDF for nyt9 Impressions
ecdf_impressions <- ecdf(nyt9$Impressions)
plot(ecdf_impressions, main = "ECDF for nyt9 Impressions", xlab = "Impressions", ylab = "y", col = "orange")


#Quantile-Quantile (Q-Q) for nyt3, nyt4, nyt5, nyt6, nyt7, nyt8, nyt9 for Age and Impressions

# Q-Q plot for nyt3 Age
qqplot_Age <- qqplot(qnorm(ecdf(nyt3$Age)(nyt3$Age)), nyt3$Age,xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",main = "Q-Q Plot for nyt3 Age")

# Q-Q plot for nyt3 Impressions
qqplot_impressions <- qqplot(qnorm(ecdf(nyt3$Impressions)(nyt3$Impressions)), nyt3$Impressions, xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", main = "Q-Q Plot for nyt3 Impressions")

# Q-Q plot for nyt4 Age
qqplot_Age <- qqplot(qnorm(ecdf(nyt4$Age)(nyt4$Age)), nyt4$Age,xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",main = "Q-Q Plot for nyt4 Age", col = "pink")

# Q-Q plot for nyt4 Impressions
qqplot_impressions <- qqplot(qnorm(ecdf(nyt4$Impressions)(nyt4$Impressions)), nyt4$Impressions, xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", main = "Q-Q Plot for nyt4 Impressions", col = "pink")


# Q-Q plot for nyt5Age
qqplot_Age <- qqplot(qnorm(ecdf(nyt5$Age)(nyt5$Age)), nyt5$Age,xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",main = "Q-Q Plot for nyt5 Age", col = "purple")

# Q-Q plot for nyt5 Impressions
qqplot_impressions <- qqplot(qnorm(ecdf(nyt5$Impressions)(nyt5$Impressions)), nyt5$Impressions, xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", main = "Q-Q Plot for nyt5 Impressions", col = "purple")


# Q-Q plot for nyt6 Age
qqplot_Age <- qqplot(qnorm(ecdf(nyt6$Age)(nyt6$Age)), nyt6$Age,xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",main = "Q-Q Plot for nyt6 Age", col = "red")

# Q-Q plot for nyt6 Impressions
qqplot_impressions <- qqplot(qnorm(ecdf(nyt6$Impressions)(nyt6$Impressions)), nyt6$Impressions, xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", main = "Q-Q Plot for nyt6 Impressions", col = "red")


# Q-Q plot for nyt7 Age
qqplot_Age <- qqplot(qnorm(ecdf(nyt7$Age)(nyt7$Age)), nyt7$Age,xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",main = "Q-Q Plot for nyt7 Age", col = "blue")

# Q-Q plot for nyt7 Impressions
qqplot_impressions <- qqplot(qnorm(ecdf(nyt7$Impressions)(nyt7$Impressions)), nyt7$Impressions, xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", main = "Q-Q Plot for nyt7 Impressions", col = "blue")


# Q-Q plot for nyt8 Age
qqplot_Age <- qqplot(qnorm(ecdf(nyt8$Age)(nyt8$Age)), nyt8$Age,xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",main = "Q-Q Plot for nyt8 Age", col = "green")

# Q-Q plot for nyt8 Impressions
qqplot_impressions <- qqplot(qnorm(ecdf(nyt8$Impressions)(nyt8$Impressions)), nyt8$Impressions, xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", main = "Q-Q Plot for nyt8 Impressions", col = "green")


# Q-Q plot for nyt9 Age
qqplot_Age <- qqplot(qnorm(ecdf(nyt9$Age)(nyt9$Age)), nyt9$Age,xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",main = "Q-Q Plot for nyt9 Age", col = "orange")

# Q-Q plot for nyt9 Impressions
qqplot_impressions <- qqplot(qnorm(ecdf(nyt9$Impressions)(nyt9$Impressions)), nyt9$Impressions, xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", main = "Q-Q Plot for nyt9 Impressions", col = "orange")




#d) Significance test using T-Test

# T-Test for nyt3$Age and nyt3$Impressions
t_nyt3 <- t.test(nyt3$Age, nyt3$Impressions)
print (t_nyt3)

# T-Test for nyt4$Age and nyt4$Impressions
t_nyt4 <- t.test(nyt4$Age, nyt4$Impressions)
print(t_nyt4)


# T-Test for nyt5$Age and nyt5$Impressions
t_nyt5 <- t.test(nyt5$Age, nyt5$Impressions)
print(t_nyt5)


# T-Test for nyt6$Age and nyt6$Impressions
t_nyt6 <- t.test(nyt6$Age, nyt6$Impressions)
print(t_nyt6)


# T-Test for nyt7$Age and nyt7$Impressions
t_nyt7 <- t.test(nyt7$Age, nyt7$Impressions)
print(t_nyt7)


# T-Test for nyt8$Age and nyt8$Impressions
t_nyt8 <- t.test(nyt8$Age, nyt8$Impressions)
print(t_nyt8)


# T-Test for nyt9$Age and nyt9$Impressions
t_nyt9 <- t.test(nyt9$Age, nyt9$Impressions)
print(t_nyt9)







#2) Filter the distribution for Age and Impressions.Repeated the analyses performed in the previous questions (Q1b, Q1c, and Q1d) using the filtered variables.
#Filter for Age and Impressions

# Filter nyt3 Age
filtered_nyt3 <- subset(nyt3,Age > 1  & Impressions > 1)
View(filtered_nyt3)


# Filter nyt4 
filtered_nyt4 <- subset(nyt4, Age > 1  & Impressions > 1)
View(filtered_nyt4)

# Filter nyt5 based on Impressions
filtered_nyt5 <- subset(nyt5, Age > 1  & Impressions > 1)
View(filtered_nyt5)

# Filter nyt6
filtered_nyt6 <- subset(nyt6, Age > 1  & Impressions > 1)
View(filtered_nyt6)




#Boxplot
# Boxplot for filtered nyt3 Age and Impressions
boxplot(filtered_nyt3$Age, filtered_nyt3$Impressions,
        names = c("Age", "Impressions"),
        main = "Boxplot of Filtered nyt3 Age and Impressions",
        col = c("yellow", "blue"))

# Summary statistics for filtered nyt3
summary(filtered_nyt3$Age)
summary(filtered_nyt3$Impressions)



# Boxplot for filtered nyt4 Age and Impressions
boxplot(filtered_nyt4$Age, filtered_nyt4$Impressions,
        names = c("Age", "Impressions"),
        main = "Boxplot of Filtered nyt4 Age and Impressions",
        col = c("pink", "yellow"))

# Summary statistics for filtered nyt4
summary(filtered_nyt4$Age)
summary(filtered_nyt4$Impressions)



# Boxplot for filtered nyt5 Age and Impressions
boxplot(filtered_nyt5$Age, filtered_nyt5$Impressions,
        names = c("Age", "Impressions"),
        main = "Boxplot of Filtered nyt5 Age and Impressions",
        col = c("purple", "black"))

# Summary statistics for filtered nyt5
summary(filtered_nyt5$Age)
summary(filtered_nyt5$Impressions)



# Boxplot for filtered nyt6 Age and Impressions
boxplot(filtered_nyt6$Age, filtered_nyt6$Impressions,
        names = c("Age", "Impressions"),
        main = "Boxplot of Filtered nyt6 Age and Impressions",
        col = c("red", "pink"))

# Summary statistics for filtered nyt6
summary(filtered_nyt6$Age)
summary(filtered_nyt6$Impressions)





# b)normality test
#Anderson-Darling test for Age and Impressions

#filtered_nyt3
ad_filtered_nyt3<- ad.test(filtered_nyt3$Age)
print(ad_filtered_nyt3)

ad_filtered_nyt3  <- ad.test(filtered_nyt3$Impressions)
print(ad_filtered_nyt3)


#nyt4
ad_filtered_nyt4  <- ad.test(filtered_nyt4$Age)
print(ad_filtered_nyt4)

ad_filtered_nyt4  <- ad.test(filtered_nyt4$Impressions)
print(ad_filtered_nyt4)


#nyt5
ad_filtered_nyt5 <- ad.test(filtered_nyt5$Age)
print(ad_filtered_nyt5)

ad_filtered_nyt5 <- ad.test(filtered_nyt5$Impressions)
print(ad_filtered_nyt5)


#nyt6
ad_filtered_nyt6 <- ad.test(filtered_nyt6$Age)
print(ad_filtered_nyt6)

ad_filtered_nyt6  <- ad.test(filtered_nyt6$Impressions)
print(ad_filtered_nyt6)


#Histogram (Used binwidth = 4, i used breaks parameter to specify bin width)

# filtered_nyt3 (Age and Impressions)
hist(filtered_nyt3$Age, main = "Histogram for filtered_nyt3 Age", col = "yellow", breaks = seq(min(filtered_nyt3$Age), max(filtered_nyt3$Age) + 4, by = 4))

hist(filtered_nyt3$Impressions, main = "Histogram for filtered_nyt3 Impressions", col = "yellow", breaks = seq(min(filtered_nyt3$ Impressions), max(filtered_nyt3$ Impressions) + 4, by = 4))


# filtered_nyt4(Age and Impressions)
hist(filtered_nyt4$Age, main = "Histogram for filtered_nyt4 Age", col = "pink", breaks = seq(min(filtered_nyt4$Age), max(filtered_nyt4$Age) + 4, by = 4))

hist(filtered_nyt4$Impressions, main = "Histogram for filtered_nyt4 Impressions ", col = "pink", breaks = seq(min(filtered_nyt4$Impressions), max(filtered_nyt4$Impressions) + 4, by = 4))


# filtered_nyt5(Age and Impressions)
hist(filtered_nyt5$Age, main = "Histogram for filtered_nyt5 Age", col = "purple", breaks = seq(min(filtered_nyt5$Age), max(filtered_nyt5$Age) + 4, by = 4))

hist(filtered_nyt5$Impressions, main = "Histogram for nyt5 Impressions ", col = "purple", breaks = seq(min(filtered_nyt5$Impressions), max(filtered_nyt5$Impressions) + 4, by = 4))


# filtered_nyt6(Age and Impressions)
hist(filtered_nyt6$Age, main = "Histogram for filtered_nyt6 Age", col = "red", breaks = seq(min(filtered_nyt6$Age), max(filtered_nyt6$Age) + 4, by = 4))

hist(filtered_nyt6$Impressions, main = "Histogram for filtered_nyt6 Impressions ", col = "red", breaks = seq(min(filtered_nyt6$Impressions), max(filtered_nyt6$Impressions) + 4, by = 4))




#c)ECDFs (Empirical Cumulative Distribution Function)) for filtered_nyt3, nyt4, nyt5, nyt6, for Age and Impressions

#ECDF for filtered_nyt3 Age
ecdf_age <- ecdf(filtered_nyt3$Age)
plot(ecdf_age, main = "ECDF for filtered_nyt3 Age", xlab = "Age", ylab = "y")

#ECDF for filtered_nyt3 Impressions
ecdf_impressions <- ecdf(filtered_nyt3$Impressions)
plot(ecdf_impressions, main = "ECDF for filtered_nyt3 Impressions", xlab = "Impressions", ylab = "y")


#ECDF for filtered_nyt4 Age
ecdf_age <- ecdf(filtered_nyt4$Age)
plot(ecdf_age, main = "ECDF for filtered_nyt4 Age", xlab = "Age", ylab = "y", col = "pink")

#ECDF for filtered_nyt4 Impressions
ecdf_impressions <- ecdf(filtered_nyt4$Impressions)
plot(ecdf_impressions, main = "ECDF for filtered_nyt4 Impressions", xlab = "Impressions", ylab = "y", col = "pink")


#ECDF for filtered_nyt5 Age
ecdf_age <- ecdf(filtered_nyt5$Age)
plot(ecdf_age, main = "ECDF for filtered_nyt5 Age", xlab = "Age", ylab = "y", col = "purple")

#ECDF for filtered_nyt5 Impressions
ecdf_impressions <- ecdf(filtered_nyt5$Impressions)
plot(ecdf_impressions, main = "ECDF for filtered_nyt5 Impressions", xlab = "Impressions", ylab = "y", col = "purple")


#ECDF for filtered_nyt6 Age
ecdf_age <- ecdf(filtered_nyt6$Age)
plot(ecdf_age, main = "ECDF for filtered_nyt6 Age", xlab = "Age", ylab = "y", col = "red")

#ECDF for filtered_nyt6 Impressions
ecdf_impressions <- ecdf(filtered_nyt6$Impressions)
plot(ecdf_impressions, main = "ECDF for filtered_nyt6 Impressions", xlab = "Impressions", ylab = "y", col = "red")


#Quantile-Quantile (Q-Q) for nyt3, nyt4, nyt5, nyt6, for Age and Impressions

# Q-Q plot for filtered_nyt3 Age
qqplot_Age <- qqplot(qnorm(ecdf(filtered_nyt3$Age)( filtered_nyt3$Age)), nyt3$Age,xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",main = "Q-Q Plot for filtered_nyt3 Age")


# Q-Q plot for filtered_nyt3 Impressions
qqplot_impressions <- qqplot(qnorm(ecdf(filtered_nyt3$Impressions)( filtered_nyt3$Impressions)), filtered_nyt3$Impressions, xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", main = "Q-Q Plot for filtered_nyt3 Impressions")

# Q-Q plot for filtered_nyt4 Age
qqplot_Age <- qqplot(qnorm(ecdf(filtered_nyt4$Age)( filtered_nyt4$Age)), filtered_nyt4$Age,xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",main = "Q-Q Plot for filtered_nyt4 Age", col = "pink")

# Q-Q plot for filtered_nyt4 Impressions
qqplot_impressions <- qqplot(qnorm(ecdf(filtered_nyt4$Impressions)( filtered_nyt4$Impressions)), filtered_nyt4$Impressions, xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", main = "Q-Q Plot for filtered_nyt4 Impressions", col = "pink")


# Q-Q plot for nyt5Age
qqplot_Age <- qqplot(qnorm(ecdf(filtered_nyt5$Age)( filtered_nyt5$Age)), filtered_nyt5$Age,xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",main = "Q-Q Plot for filtered_nyt5 Age", col = "purple")

# Q-Q plot for filtered_nyt5 Impressions
qqplot_impressions <- qqplot(qnorm(ecdf(filtered_nyt5$Impressions)( filtered_nyt5$Impressions)), filtered_nyt5$Impressions, xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", main = "Q-Q Plot for filtered_nyt5 Impressions", col = "purple")


# Q-Q plot for filtered_nyt6 Age
qqplot_Age <- qqplot(qnorm(ecdf(filtered_nyt6$Age)( filtered_nyt6$Age)), filtered_nyt6$Age,xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",main = "Q-Q Plot for filtered_nyt6 Age", col = "red")

# Q-Q plot for  filtered_nyt6 Impressions
qqplot_impressions <- qqplot(qnorm(ecdf(filtered_nyt6$Impressions)( filtered_nyt6$Impressions)), filtered_nyt6$Impressions, xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", main = "Q-Q Plot for filtered_nyt6 Impressions", col = "red")



#d) Significance test using T-Test

# T-Test for filtered_nyt3$Age and filtered_nyt3$Impressions
t_filtered_nyt3 <- t.test(filtered_nyt3$Age, filtered_nyt3$Impressions)
print (t_filtered_nyt3)

# T-Test for filtered_nyt4$Age and filtered_nyt4$Impressions
t_filtered_nyt4 <- t.test(filtered_nyt4$Age, filtered_nyt4$Impressions)
print(t_filtered_nyt4)


# T-Test for filtered_nyt5$Age and filtered_nyt5$Impressions
t_filtered_nyt5 <- t.test(filtered_nyt5$Age, filtered_nyt5$Impressions)
print(t_filtered_nyt5)


# T-Test for filtered_nyt6$Age and filtered_nyt6$Impressions
t_filtered_nyt6 <- t.test(filtered_nyt6$Age, filtered_nyt6$Impressions)
print(t_filtered_nyt6)


#End of Assignment

