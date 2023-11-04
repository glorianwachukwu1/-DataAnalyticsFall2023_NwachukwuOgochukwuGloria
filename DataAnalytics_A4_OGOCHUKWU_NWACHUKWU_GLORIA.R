#Assignment_4_NWACHUKWU_OGOCHUKWU_GLORIA

#NYC Citywide Annualized Calendar Sales Update datasets
#Libraries I intend to use
library(ggplot2) #for creating plots 
library(dplyr) #for data manipulation.
library(stats)
library(rpart)
library(class)
library(readr)
library(caret)


#I choose Manhattan dataset

manhattan_data <- read_csv("DATA_ANALYTICS/Assignment/Assignment 4/NYC_Citywide_Annualized_Calendar_Sales_Update.csv")
View(manhattan_data)

#To see the structure
str(manhattan)


# To perform EDA for my manhattan_data

#I start by checking the summary of my data
summary(manhattan_data)

#I started with visualization of the distributions of SALE_PRICE, LAND_SQUARE_FEET, and GROSS_SQUARE_FEET.since i will be working with theses three variables
#Histogram i used breaks parameter to specify bin width)

options(scipen = 999)  # Disable scientific notation for numeric output


# Histogram for SALE_PRICE
hist(manhattan_data$SALE_PRICE, main = "Histogram SALE PRICE of Manhattan", col = "orange", breaks = 30)

# Histogram for LAND_SQUARE_FEET
hist(manhattan_data$LAND_SQUARE_FEET, main = "Histogram LAND SQUARE FEET of Manhattan", col = "blue", breaks = 30)


# Histogram for GROSS_SQUARE_FEET
hist(manhattan_data$GROSS_SQUARE_FEET, main = "Histogram GROSS SQUARE FEET of Manhattan", col = "green", breaks = 30)



#I noticed missing values in LAND_SQUARE_FEET and GROSS_SQUARE_FEET. so i will tackle it by Inputting missing values in LAND_SQUARE_FEET and GROSS_SQUARE_FEET with the median.

median_land_square_feet <- median(manhattan_data$LAND_SQUARE_FEET, na.rm = TRUE)
median_gross_square_feet <- median(manhattan_data$GROSS_SQUARE_FEET, na.rm = TRUE)
manhattan_data$LAND_SQUARE_FEET[is.na(manhattan_data$LAND_SQUARE_FEET)] <- median_land_square_feet
manhattan_data$GROSS_SQUARE_FEET[is.na(manhattan_data$GROSS_SQUARE_FEET)] <- median_gross_square_feet

# To get my Summary statistics
mean_sale_price <- mean(manhattan_data$SALE_PRICE)
median_sale_price <- median(manhattan_data$SALE_PRICE)
sd_sale_price <- sd(manhattan_data$SALE_PRICE)



#boxplot of  manhattan_data
# Box plot for SALE_PRICE
boxplot(manhattan_data$SALE_PRICE, main = "Boxplot of SALE_PRICE in Manhattan",  col = "white", border = "blue")

# Box plot for GROSS SQUARE FEET
boxplot(manhattan_data$GROSS_SQUARE_FEET, main = "Boxplot of GROSS SQUARE FEET in Manhattan",  col = "white", border = "red")

# Box plot for LAND_SQUARE_FEET
boxplot(manhattan_data$LAND_SQUARE_FEET, main = "Boxplot of LAND SQUARE FEET in Manhattan", col = "white", border = "green")




# Box plot for SALE_PRICE, GROSS_SQUARE_FEET, and LAND_SQUARE_FEET with a legend
boxplot(manhattan_data$SALE_PRICE, manhattan_data$GROSS_SQUARE_FEET, manhattan_data$LAND_SQUARE_FEET,
        names = c("SALE PRICE", "GROSS SQUARE FEET", "LAND SQUARE FEET"),
        main = "Boxplot of SALE PRICE, GROSS SQUARE FEET, and LAND SQUARE FEET in Manhattan",
        col = c("blue", "red", "green"))




# I used scatter plot to visualize correlations between SALE_PRICE and LAND_SQUARE_FEET and GROSS_SQUARE_FEET. :
#Scatter plot for LAND_SQUARE_FEET vs. SALE_PRICE:
plot(manhattan_data$LAND_SQUARE_FEET, manhattan_data$SALE_PRICE,
     xlab = "LAND SQUARE FEET", ylab = "SALE PRICE",
     main = "Scatter Plot of LAND SQUARE FEET vs. SALE PRICE",
     col = "brown", pch = 16)


#Scatter plot for GROSS_SQUARE_FEET vs. SALE_PRICE
plot(manhattan_data$GROSS_SQUARE_FEET, manhattan_data$SALE_PRICE,
     xlab = "GROSS SQUARE FEET", ylab = "SALE PRICE",
     main = "Scatter Plot of GROSS SQUARE FEET vs. SALE PRICE",
     col = "green", pch = 16)

# Calculate Pearson correlation between SALE_PRICE, GROSS_SQUARE_FEET, and LAND_SQUARE_FEET
cor_sale_gross <- cor(manhattan_data$SALE_PRICE, manhattan_data$GROSS_SQUARE_FEET, method = "pearson")
cor_sale_land <- cor(manhattan_data$SALE_PRICE, manhattan_data$LAND_SQUARE_FEET, method = "pearson")



#1b)
# Calculate Cook's distance
# Identify outliers using IQR
#i will use the portion of the sample data
mysample_data <- 2000

# I will then fit the linear model into mysampled data

sampled_data <- manhattan_data[sample(1:nrow(manhattan_data), sample_size), ]
# then to Fit the linear model 
model <- lm(SALE_PRICE ~ GROSS_SQUARE_FEET + LAND_SQUARE_FEET, data = sampled_data)
cooks_dist <- cooks.distance(model)

#To Calculate Cook's distance for each data point
cooks_dist <- cooks.distance(model)

# Set a Cook's distance threshold at the 99th percentile
cooks_threshold <- quantile(cooks_dist, 0.99, na.rm = TRUE)

# Identify Cook's Distance outliers
outliers_cooks <- which(cooks_dist > cooks_threshold)

# Print the indices of Cook's Distance outliers
print(outliers_cooks)

# Get the data points corresponding to Cook's Distance outliers
outlier_data_cooks <- sampled_data[outliers_cooks, ]
print(outlier_data_cooks)

# Calculate IQR for SALE_PRICE
iqr <- IQR(manhattan_data$SALE_PRICE)

# Define lower and upper bounds for IQR-based outlier detection
lower_bound <- quantile(manhattan_data$SALE_PRICE, 0.25) - 1.5 * iqr
upper_bound <- quantile(manhattan_data$SALE_PRICE, 0.75) + 1.5 * iqr

# Identify IQR-based outliers
iqr_outliers <- manhattan_data[manhattan_data$SALE_PRICE < lower_bound | manhattan_data$SALE_PRICE > upper_bound, ]

# Print the indices of IQR-based outliers
print(iqr_outliers)

# Create a boxplot for IQR-based outliers
boxplot_data <- list(IQROutliers = iqr_outliers$SALE_PRICE)
boxplot(boxplot_data, main = "Boxplot of IQR Outliers", col = "red")


#1c)#For multivariate

#I start by  performing  multivariate regression on the entire dataset 
model_full_manhattan_data <- lm(SALE_PRICE ~ GROSS_SQUARE_FEET + LAND_SQUARE_FEET , data = manhattan_data)

# Summarize the regression results
summary(model_full_manhattan_data)


#Then, conduct random sampling your dataset to create three different samples
set.seed(123)  # Setting a seed for reproducibility

sample1 <- manhattan_data[sample(nrow(manhattan_data), size = nrow(manhattan_data), replace = TRUE), ]
sample2 <- manhattan_data[sample(1: nrow(manhattan_data), size = nrow(manhattan_data), replace = TRUE), ]
sample3 <- manhattan_data[sample(nrow(manhattan_data), size = nrow(manhattan_data), replace = TRUE), ]

#Then, i will  use the lm function for linear regression perform a multivariate regression for each of the three samples.

# Multivariate Regression for Sample 1
model1 <- lm(SALE_PRICE ~ GROSS_SQUARE_FEET + LAND_SQUARE_FEET, data = sample1)
summary(model1)


# Multivariate Regression for Sample 2
model2 <- lm(SALE_PRICE ~ GROSS_SQUARE_FEET + LAND_SQUARE_FEET, data = sample2)
summary(model2)


# Multivariate Regression for Sample 3
model3 <- lm(SALE_PRICE ~ GROSS_SQUARE_FEET + LAND_SQUARE_FEET, data = sample3)
summary(model3)


# To Compare the results of the three models
summary(model1)
summary(model2)
summary(model3)


............
#KNN

# Load the required libraries
library(class)

# Split your data into training and testing sets
set.seed(123)  # For reproducibility
split_ratio <- 0.7
sample_indices <- sample(1:nrow(manhattan_data), split_ratio * nrow(manhattan_data))

if (length(sample_indices) > 5000) 
  train_data <- manhattan_data[sample_indices, ]
  test_data <- manhattan_data[-sample_indices, ]
  
  # Standardize the features
  train_data_scaled <- scale(train_data[, c("GROSS_SQUARE_FEET", "LAND_SQUARE_FEET")])
  test_data_scaled <- scale(test_data[, c("GROSS_SQUARE_FEET", "LAND_SQUARE_FEET")])
  
  # Fit the KNN model
  k_value <- 10  # Choose an appropriate k value
  model_knn <- knn(train = train_data_scaled, 
                   test = test_data_scaled, 
                   cl = train_data$SALE_PRICE, 
                   k = k_value, 
                   prob = TRUE, 
                   use.all = TRUE)
  
  # Make predictions on the test data
  predictions <- as.numeric(model_knn)
  
  # Calculate Mean Absolute Error (MAE)
  mae <- mean(abs(test_data$SALE_PRICE - predictions))
  
  # Calculate Mean Squared Error (MSE)
  mse <- mean((test_data$SALE_PRICE - predictions)^2)
  
  # Calculate R-squared
  r_squared <- 1 - (sum((test_data$SALE_PRICE - predictions)^2) / sum((test_data$SALE_PRICE - mean(test_data$SALE_PRICE))^2))

  # Create a data frame with actual and predicted prices
  predictions_df <- data.frame(Actual = test_data$SALE_PRICE, Predicted = predictions)
  
  # Create a scatter plot
  ggplot(predictions_df, aes(x = Actual, y = Predicted)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, color = "brown", linetype = "dashed") +  
    labs(title = "Actual vs. Predicted Sale Prices", x = "Actual Sale Price", y = "Predicted Sale Price")
  
#To print various evaluation metrics, such as Mean Absolute Error (MAE), Mean Squared Error (MSE), and R-squared, to quantitatively assess how well the model has performed at all
  cat("Mean Absolute Error (MAE):", mae, "\n")
  cat("Mean Squared Error (MSE):", mse, "\n")
  cat("R-squared:", r_squared, "\n")
  

  
  
# I want to use decision tree model too. so as to compare the results
 
  # Decision Tree
  dt_model <- rpart(SALE_PRICE ~ GROSS_SQUARE_FEET + LAND_SQUARE_FEET, data = test_data)
  dt_predictions <- predict(dt_model, newdata = test_data)
  dt_mae <- mean(abs(test_data$SALE_PRICE - dt_predictions))
  cat("Decision Tree MAE:", dt_mae, "\n")
  
  # Create a data frame with actual and predicted Sale Prices
  dt_predictions_df <- data.frame(Actual = test_data$SALE_PRICE, Predicted = dt_predictions)
  

  # Create a scatter plot
  ggplot(dt_predictions_df, aes(x = Actual, y = Predicted)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
    labs(title = "Actual vs. Predicted SALE_PRICE (Decision Tree)", x = "Actual Sale Price", y = "Predicted Sale Price")
  
  
#To calculate additional evaluation metrics such as Mean Absolute Error (MAE), Mean Squared Error (MSE), and R-squared for a quantitative assessment of my model's performance
  
  #Decision tree
  
  # Calculate Mean Absolute Error (MAE) for Decision Tree
  dt_mae <- mean(abs(test_data$SALE_PRICE - dt_predictions))
  
  # Calculate Mean Squared Error (MSE) for Decision Tree
  dt_mse <- mean((test_data$SALE_PRICE - dt_predictions)^2)
  
  # Calculate R-squared for Decision Tree
  dt_residuals <- test_data$SALE_PRICE - dt_predictions
  dt_ss_residuals <- sum(dt_residuals^2)
  dt_ss_total <- sum((test_data$SALE_PRICE - mean(test_data$SALE_PRICE))^2)
  dt_r_squared <- 1 - (dt_ss_residuals / dt_ss_total)
  
  # Print the metrics
  cat("Decision Tree Model Metrics:\n")
  cat("Mean Absolute Error (MAE):", dt_mae, "\n")
  cat("Mean Squared Error (MSE):", dt_mse, "\n")
  cat("R-squared:", dt_r_squared, "\n")
  
  
  
#2)
  
  
  # Make predictions using the decision tree model
  predicted_prices <- predict(decision_tree_model, newdata = filtered_data)
  
  # Create a data frame with actual and predicted prices
  predictions_df <- data.frame(Actual = filtered_data$SALE_PRICE, Predicted = predicted_prices)
  
  # Print the first few rows of the predictions data frame
  head(predictions_df)
  
  # Create a data frame with the provided data
  predictions_df <- data.frame(
    Actual = c(43300000, 148254147, 11000000, 591800000, 0, 99350000),
    Predicted = c(32318681.6, 32318681.6, 969116.5, 443635423.4, 969116.5, 32318681.6)
  )
  
 
  # Create a scatter plot
  ggplot(predictions_df, aes(x = Actual, y = Predicted)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  
    labs(title = "Actual vs. Predicted Sale Prices", x = "Actual Sale Price", y = "Predicted Sale Price")
  

  
  
#To perform significance test that is suitable for my variables
t_test_manhattan_Gross <- t.test(manhattan_data$SALE_PRICE, manhattan_data$GROSS_SQUARE_FEET)

print(t_test_manhattan_Gross)
      
t_test_manhattan_land <- t.test(manhattan_data$SALE_PRICE,manhattan_data$LAND_SQUARE_FEET)
print(t_test_manhattan_land)      


# Assuming you have already conducted the t-tests as you provided

# Create a bar plot to visualize the t-test results
bar_data <- data.frame(
  Variable = c("SALE_PRICE vs. GROSS_SQUARE_FEET", "SALE_PRICE vs. LAND_SQUARE_FEET"),
  p_value = c(t_test_manhattan_Gross$p.value, t_test_manhattan_land$p.value)
)



      #End of Assignment
      
      