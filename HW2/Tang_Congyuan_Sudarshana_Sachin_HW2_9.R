# QUESTION 8 - Section 5.4 Question 6

library(ISLR)   # for the data
library(class)  # class library for logistic regression and knn
library(MASS)   # MASS library for LDA and QDA

set.seed(1)
Default = ISLR::Default

# 6 (a) Determining the estimated standard errors 

glm.fit.default = glm (default~balance+income, data=Default, family = binomial)
summary(glm.fit.default)

# From the coefficients table, we can observe that both balance and income are signficant
# predictors in this model.

# 6 (b) function for multiple logistic regression

boot.fn = function(data,index){
  glm.fit = glm(default ~ income+balance, data=data, subset=index, family=binomial)
  return (coef(glm.fit))
}

# 6 (c) Estimating the standard errors of the logistic regression .

boot.fn(Default,1:10000)

# 6 (d) 

# The estimates from the bootstrap function and glm function are very close in this case.
