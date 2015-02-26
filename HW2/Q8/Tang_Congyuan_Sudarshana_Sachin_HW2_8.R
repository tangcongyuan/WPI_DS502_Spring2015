# QUESTION 7 - Section 5.4 Question 5

library(ISLR)
library(class)  # class library for logistic regression and knn
library(MASS)   # MASS library for LDA and QDA

# 5 (a) Fit a logistic regression model that uses income and balance to predict default.

Default = ISLR::Default
Default = na.omit(Default)
set.seed(1)
glm.fit.default = glm (default~balance+income, data=Default, family = binomial)
summary(glm.fit.default)

# 5 (b) Estimating the test error of this model.

# i. Splitting the sample set into a training set and a validation set

size.default = dim(Default)[1]
train = sample(1:size.default, size.default/2, replace = F)
DefaultTrain = Default[train,]
DefaultTest = Default[-train,]

# ii. Fit a multiple logistic regression model using only the training observations.

glm.fit.DefaultTrain = glm (default~balance+income, data=Default, family=binomial, subset=train)
summary(glm.fit.DefaultTrain)

# iii. Prediction of default status for each individual in the validation set

glm.probs.default = predict(glm.fit.DefaultTrain, DefaultTest, type="response")
glm.pred.default = rep("No",5000)
glm.pred.default[glm.probs.default >.5]="Yes"

# iv. Validation set error.

validation.error = mean (glm.pred.default != DefaultTest$default)
validation.error

# 5 (c) Repeating (b) using 3 different splits 

# Taking 2/3rds of the data set as training data...
train = sample(1:size.default, 3*size.default/4, replace = F)
DefaultTrain = Default[train,]
DefaultTest = Default[-train,]

glm.fit.DefaultTrain = glm (default~balance+income, data=Default, family=binomial, subset=train)
summary(glm.fit.DefaultTrain)
glm.probs.default = predict(glm.fit.DefaultTrain, DefaultTest, type="response")
glm.pred.default = rep("No",5000)
glm.pred.default[glm.probs.default >.5]="Yes"

validation.error = mean (glm.pred.default != DefaultTest$default)
validation.error

# Taking 3/4th of the data set as training data...
train = sample(1:size.default, 3*size.default/4, replace = F)
DefaultTrain = Default[train,]
DefaultTest = Default[-train,]

glm.fit.DefaultTrain = glm (default~balance+income, data=Default, family=binomial, subset=train)
summary(glm.fit.DefaultTrain)
glm.probs.default = predict(glm.fit.DefaultTrain, DefaultTest, type="response")
glm.pred.default = rep("No",5000)
glm.pred.default[glm.probs.default >.5]="Yes"

validation.error = mean (glm.pred.default != DefaultTest$default)
validation.error

# Taking 4/5th of the data set as training data...
train = sample(1:size.default, 4*size.default/5, replace = F)
DefaultTrain = Default[train,]
DefaultTest = Default[-train,]

glm.fit.DefaultTrain = glm (default~balance+income, data=Default, family=binomial, subset=train)
summary(glm.fit.DefaultTrain)
glm.probs.default = predict(glm.fit.DefaultTrain, DefaultTest, type="response")
glm.pred.default = rep("No",5000)
glm.pred.default[glm.probs.default >.5]="Yes"

validation.error = mean (glm.pred.default != DefaultTest$default)
validation.error


# 5 (d)  Using a Dummy Variable

tr_data = sample(dim(Default)[1], dim(Default)[1]/2)
glm.fit = glm(default ~ income + balance + student, data = Default, family = binomial, subset = tr_data)
summary(glm.fit)
glm.pred = rep("No", dim(Default)[1]/2)
glm.prob = predict(glm.fit, Default[-tr_data, ], type = "response")
glm.pred[glm.prob > 0.5] = "Yes"
validation.error = mean(glm.pred != Default[-tr_data,]$default)
validation.error