# This is the question4 in homework2 for WPI DS502 Statistical Method for Data Science.

# Set working directory.
setwd('/Users/Eric/GoogleDrive/2015@WPI/DS502/HW2/q4/')
# Load dataset and check it
library(ISLR)
names(Weekly)
dim(Weekly)
summary(Weekly)
# Find pattern
cor(Weekly[,-9])
attach(Weekly)
# Found Volume and Year are highly correlated, plot them:
plot(Volume, type = 'l')
# Train linear regression model
glm.fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family = binomial, data = Weekly)
# Summarize glm
summary(glm.fit)
# Find dummy variable for response
contrasts(Weekly$Direction)
# Compute confusion matrix
glm.prob = predict(glm.fit,type = "response")
glm.pred = rep("Down", nrow(Weekly))
glm.pred[glm.prob > 0.5] = "Up"
# Directly compute confusion matrix
table(glm.pred, Direction)
# Compute the accuracy
mean(glm.pred == Direction)

# Compute confusion matrix using glm, training and testing set
train = Weekly[Year<2009,]
test = Weekly[Year>2008,]
glm.fit = glm(train$Direction ~ Lag2, data = train, family = binomial)
summary(glm.fit)
glm.prob = predict(glm.fit, test, type = "response")
glm.pred = rep("Down", nrow(test))
glm.pred[glm.prob > 0.5] = "Up"
# Directly compute confusion matrix
table(glm.pred, test$Direction)
# Compute the accuracy
mean(glm.pred == test$Direction)

# Compute confusion matrix using LDA, training and testing set
library(MASS)
lda.fit = lda(train$Direction ~ Lag2, data = train, family = binomial)
lda.fit
lda.pred = predict(lda.fit, test)
lda.pred$posterior
# Directly compute confusion matrix
table(lda.pred$class, test$Direction)
# Compute the accuracy
mean(lda.pred$class == test$Direction)

# Compute confusion matrix using QDA, training and testing set
library(MASS)
qda.fit = qda(train$Direction ~ Lag2, data = train, family = binomial)
qda.fit
qda.pred = predict(qda.fit, test)
qda.pred$posterior
# Directly compute confusion matrix
table(qda.pred$class, test$Direction)
# Compute the accuracy
mean(qda.pred$class == test$Direction)

# Compute confusion matrix using KNN, training and testing set
library(class)
train.X = data.frame(train$Lag2)
test.X = data.frame(test$Lag2)
train.Direction = train$Direction
set.seed(1992)
K = 300
accuracy = array(0, c(1,K))
highest_accuracy = 0
for (i in 1:K) {
  knn.pred = knn(train.X, test.X, train.Direction, k = i)
  table(knn.pred, test$Direction)
  if(highest_accuracy < mean(knn.pred == test$Direction)){
    highest_accuracy = mean(knn.pred == test$Direction)
    optimalK = i
  }
  accuracy[i] = mean(knn.pred == test$Direction)
}
index = array(1:K)
plot(index, accuracy, type = "l", main = "Accuracy VS. K", xlab = "# of neighbors", ylab = "accuracy")
highest_accuracy
optimalK
plot(index, 1 - accuracy, type = "l", main = "Error VS. K", xlab = "# of neighbors", ylab = "Error")


cor(Weekly[,-9])
Lag1+Lag2
