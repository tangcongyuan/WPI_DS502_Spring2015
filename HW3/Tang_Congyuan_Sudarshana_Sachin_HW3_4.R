# HW-3 Question - 4 - Section 6.8, Page 263, Q9
# Predict the number of applications received using the other variables in the college data set

#(a) Split the data set into a training set and a test set.
library(glmnet)
library(ISLR)
set.seed(3)
college = ISLR::College
n = dim(college)
train = sample(c(TRUE,FALSE), n, rep=TRUE)
test = (!train)
college.train = college[train,]
college.test = college[test,]


#(b) Fit a linear model using least squares on the training set, and report the test error obtained.

linear.fit = lm(Apps~., data=college.train)
summary(linear.fit)
y_hat = predict(linear.fit, college.test)
# MSE for Linear fit:
mse = mean((college.test$Apps-y_hat)^2)
print (mse)

# (c) Fit a ridge regression model on the training set, with ?? chose by cross-validation. Report
# the test error obtained.
y = college.train$Apps 
x = model.matrix(Apps~., data=college.train)
y.test = college.test$Apps
cv.out=cv.glmnet(x,y,alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
print (bestlam)
ridge.mod = cv.out
ridge.coeff = predict(ridge.mod,type="coefficients",s=bestlam)
print (ridge.coeff)

x.test = model.matrix(Apps~., data = college.test)
ridge.pred=predict(ridge.mod,s=bestlam ,newx=x.test)

# MSE for Ridge Regression:
mean((ridge.pred-y.test)^2)


# (d) Fit a lasso model on the training set, with ?? chosen by cross- validation. Report the test
# error obtained, along with the number of non-zero coefficient estimates.
y = college.train$Apps 
x = model.matrix(Apps~., data=college.train)
y.test = college.test$Apps
cv.out=cv.glmnet(x,y,alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
print (bestlam)
lasso.mod = cv.out
lasso.coeff = predict(lasso.mod,type="coefficients",s=bestlam)
print (lasso.coeff)

x.test = model.matrix(Apps~., data = college.test)
lasso.pred=predict(lasso.mod,s=bestlam ,newx=x.test)

# MSE for Lasso Regression:
mean((lasso.pred-y.test)^2)

#(e) Fit a PCR model on the training set, with M chosen by cross- validation. Report the test 
# error obtained, along with the value of M selected by cross-validation.
library(pls)
pcr.fit = pcr(Apps~., data=college.train, scale=TRUE, validation="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP")
# MSEP is minimum when all predictors are included. So we use all predictors:
pcr.pred = predict(pcr.fit, college.test, ncomp=17)

#MSE for PCR model:
MSE = mean((pcr.pred-y.test)^2)
print (MSE)

# (f) PLS model
pls.fit = plsr(Apps~., data=college.train, scale=TRUE, validation="CV")
summary(pls.fit)
validationplot(pls.fit,val.type="MSEP")
# MSEP is minimum when about 9 predictors are included
pls.pred = predict(pls.fit, college.test, ncomp=12)

#MSE for PLS model:
MSE = mean((pls.pred-y.test)^2)
print (MSE)
