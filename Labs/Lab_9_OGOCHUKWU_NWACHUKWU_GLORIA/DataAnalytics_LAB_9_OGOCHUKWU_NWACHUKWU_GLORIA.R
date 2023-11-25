#Lab_9_NWACHUKWU_OGOCHUKWU_GLORIA


#Fitting a Regression Trees

#Libraries to use
library(MASS) 
install.packages("tree")
library(tree)
library(randomForest)

head(Boston)
View(Boston)

train = sample(1:nrow(Boston), nrow (Boston)/2)

tree.boston = tree(medv ~.,Boston, subset = train)

summary(tree.boston)


#Output

#Variables actually used in tree construction:
# "rm"    "lstat" "nox"

#The variables above are the variables that the decision tree found most useful in predicting "medv" 

#Residual mean deviance is  14.44
#A lower value indicates that the predictions from the model are closer to the actual values, suggesting a better fit, so it’s a better fit.



#Regression Tree
tree(formula = medv ~., data = Boston, subset = train)

#To plot the tree
plot(tree.boston)

#To add text to my plot
text(tree.boston, pretty = 0)

#The variable "Istat" measure the percentage of the individuals with lower socioeconomics status

#The tree indicates that the lower values of Istat corresponds to more expensive house


#To perform cross-validation on the decision tree model to evaluate whether pruning the tree (reducing its size) would improve its performance

#the cv.tree() function,is typically used for cross-validation of decision trees

cv.boston = cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, typ ='b')


#The most complex tree i selected by cross-validation in this case.
#To prune the tree, we use the prune.tree() function

prune.boston = prune.tree(tree.boston, best = 5)
plot(prune.boston)
text(prune.boston, pretty = 0)


#In keeping with the cross-validation results, will use the unpruned tree to make predictions on the test set

yhat = predict(tree.boston, newdata = Boston[-train, ])
boston.test = Boston[-train,"medv"]
plot(yhat, boston.test) #scatter plot

#Then add the abline() that is diagonal line 
abline(0,1)

mean((yhat-boston.test)^2)


# In other words, the test set MSE associated with the regression tree is 29.86.
# The square root of the MSE is around 5.464,
# indicating that, on average, the model's predictions are within around $5,464 of the true median home value for the suburb.



#Bagging & Random Forest Example

set.seed(1)
bag.boston = randomForest(medv~., data=Boston, subset= train, mtry =13, importance =TRUE)

bag.boston


# The argument mtry=13 indicates that all 13 predictors should be considered
# for each split of the tree—in other words, that bagging should be done.

# So, how well does this bagged model perform on the test set?


yhat.bag = predict(bag.boston, newdata = Boston [-train,])

#a scatter plot to compare the predicted values (yhat.bag) with the actual values in the test set (boston.test)

plot(yhat.bag, boston.test)


#Then add the abline() that is diagonal line 
abline(0,1)

#This calculates the average squared difference between predicted and actual values
# It's a measure of how well the model is doing - Mean Squared Error (MSE)
mean((yhat.bag-boston.test)^2)


# The test set MSE associated with the bagged regression tree is 13.16,
#almost half that obtained using an optimally-pruned single tree.
#We could change the number of trees grown by randomForest() using the ntree argument:



bag.boston = randomForest(medv~., data = Boston, subset = train, mtry =13, ntree =25)

yhat.bag = predict(bag.boston, newdata = Boston [-train,])

mean((yhay.bag- boston.test)^2)



set.seed(1)

rf.boston= randomForest(medv~.,data = Boston, subset=train, mtry=6, importance=TRUE)

yhat.rf=predict(rf.boston, newdata = Boston[-train ,])

mean((yhat.rf-boston.test)^2)

# The test set MSE is 11.31;
# this indicates that random forests yielded an improvement over bagging in this case.

# then, use the importance() function, to view the importance of each variable

importance(rf.boston)

# Two measures of variable importance are reported.
#The former is based upon the mean decrease of accuracy in predictions on
# the out of bag samples when a given variable is excluded from the model.
#The latter is a measure of the total decrease in node impurity that results
# from splits over that variable, averaged over all trees.
# In the case of regression trees, the node impurity is measured by the training RSS, and for classification trees by the deviance
# Plots of these importance measures can be produced using the varImpPlot() function.

varImpPlot(rf.boston)




#End of Lab 9
#Lab_9_NWACHUKWU_OGOCHUKWU_GLORIA
      
      