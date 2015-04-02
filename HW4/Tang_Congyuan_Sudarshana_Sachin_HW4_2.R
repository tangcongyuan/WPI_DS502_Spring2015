# Problem 2: Section 6.8 Page 263, question 11

library(MASS)
library(glmnet)
Boston = MASS::Boston
set.seed(10)

# Splitting data into training and test data
train.size = dim(Boston)[1] / 2
train = sample(1:dim(Boston)[1], train.size)
test = (-train)*0.25
train_data = Boston[train, ]
test_data = Boston[test, ]


#Best Subset
library(leaps)
bestSubset = regsubsets(crim~.,Boston)
bestsummary=summary(forwardSubset)
bestsummary


#Ridge Regression Model
library(glmnet)
train.mat = model.matrix(crim~., data=train_data)
test.mat = model.matrix(crim~., data=test_data)
grid = 10 ^ seq(4, -2, length=100)

# finding lambda using train_data and performimg ridge model regression (alpha = 0)
mod.ridge = cv.glmnet(train.mat, train_data[, "crim"], alpha=0, lambda=grid, thresh=1e-12)
plot(mod.ridge, main='Ridge')
coef(mod.ridge)
lambda.best = mod.ridge$lambda.min
lambda.best

# Above lambda is used to find test error
ridge.pred = predict(mod.ridge, newx=test.mat, s=lambda.best)
ridge_test_rss = mean((test_data[, "crim"] - ridge.pred)^2)
ridge_test_rss 

# 53.27709

# Lasso model
# Finding lambda using train_data (alpha = 1)
mod.lasso = cv.glmnet(train.mat, train_data[, "crim"], alpha=1, lambda=grid, thresh=1)
plot (mod.lasso, main ='Lasso')
coef(mod.lasso)
lambda.best = mod.lasso$lambda.min
lambda.best

# Above found lambda is used to find test error
lasso.pred = predict(mod.lasso, newx=test.mat, s=lambda.best)

lasso_test_rss = mean((test_data[, "crim"] - lasso.pred)^2)
lasso_test_rss 
# 54.81

"The test RSS obtained from Ridge model is less than the test RSS of lasso model"

# PCR model
library(pls)
pcr.fit = pcr(crim~., data=train_data, scale=T, validation="CV")
validationplot(pcr.fit, val.type="MSEP")
summary(pcr.fit)
pcr.pred = predict(pcr.fit, test_data, ncomp=10)
pcr_test_rss = mean((test_data[, "crim"] - data.frame(pcr.pred))^2)
pcr_test_rss  

# 56.70

# The Ridge regression is the best since it gives least test RSS





