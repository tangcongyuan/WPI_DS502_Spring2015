# This is the question5 in homework2 for WPI DS502 Statistical Method for Data Science.

# Set working directory.
setwd('/Users/Eric/GoogleDrive/2015@WPI/DS502/HW2')

# Load dataset and check it
library(ISLR)
summary(Auto)
mpg01 = data.frame(mpg01 = rep(0, nrow(Auto)))
mpg01[Auto$mpg > median(Auto$mpg),] = 1
Auto = cbind(mpg01, Auto)
summary(Auto)

# Explore the data graphically.
plot(as.factor(Auto$cylinders), as.factor(Auto$mpg01), main = "Cylinders VS. mpg01", xlab = "Cylinders", ylab = "mpg01")
plot(Auto$horsepower, Auto$mpg01, main = "horsepower VS. mpg01", xlab = "horsepower", ylab = "mpg01")
plot(Auto$weight, Auto$mpg01, main = "weight VS. mpg01", xlab = "weight", ylab = "mpg01")
plot(Auto$displacement, Auto$mpg01, main = "displacement VS. mpg01", xlab = "displacement", ylab = "mpg01")
plot(Auto$acceleration, Auto$mpg01, main = "acceleration VS. mpg01", xlab = "acceleration", ylab = "mpg01")

# Split Auto into training and testing set.
cor(Auto[,-10])
dim(Auto)
set.seed(19920606)
temp = runif(nrow(Auto))
train = Auto[temp > 0.5, ]
dim(train)
test = Auto[temp <= 0.5, ]
dim(test)

# LDA
library(MASS)
lda.fit = lda(train$mpg01 ~ cylinders + horsepower + weight + displacement + acceleration, data = train, family = binomial)
lda.pred = predict(lda.fit, test)
# Directly compute confusion matrix
table(lda.pred$class, test$mpg01)
# Compute the accuracy
mean(lda.pred$class == test$mpg01)

# QDA
library(MASS)
qda.fit = qda(train$mpg01 ~ cylinders + horsepower + weight + displacement + acceleration, data = train, family = binomial)
qda.pred = predict(qda.fit, test)
# Directly compute confusion matrix
table(qda.pred$class, test$mpg01)
# Compute the accuracy
mean(qda.pred$class == test$mpg01)

# glm
glm.fit = glm(train$mpg01 ~ cylinders + horsepower + weight + displacement + acceleration, data = train, family = binomial)
glm.prob = predict(glm.fit, test, type = "response")
glm.pred = rep(0, nrow(test))
glm.pred[glm.prob > 0.5] = 1
# Directly compute confusion matrix
table(glm.pred, test$mpg01)
# Compute the accuracy
mean(glm.pred == test$mpg01)

# KNN with K changing.
library(class)
train.X = data.frame(train$cylinders, train$horsepower, train$weight, train$displacement, train$acceleration)
test.X = data.frame(test$cylinders, test$horsepower, test$weight, test$displacement, test$acceleration)
train.mpg01 = train$mpg01
set.seed(1992)
K = 200
accuracy = array(0, c(1,K))
highest_accuracy = 0
for (i in 1:K) {
  knn.pred = knn(train.X, test.X, train.mpg01, k = i)
  table(knn.pred, test$mpg01)
  if(highest_accuracy < mean(knn.pred == test$mpg01)){
    highest_accuracy = mean(knn.pred == test$mpg01)
    optimalK = i
  }
  accuracy[i] = mean(knn.pred == test$mpg01)
}
index = array(1:K)
plot(index, accuracy, type = "l", main = "Accuracy VS. K", xlab = "# of neighbors", ylab = "accuracy")
highest_accuracy
optimalK
plot(index, 1 - accuracy, type = "l", main = "Error VS. K", xlab = "# of neighbors", ylab = "Error")


