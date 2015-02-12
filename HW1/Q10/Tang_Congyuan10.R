# This is the question10 in homework1 for WPI DS502 Statistical Method for Data Science.
# Read in dataset.
library(ISLR)
auto = ISLR::Auto
summary(auto)
attach(auto)
pairs(auto)
# Matrix correlation of the dataset.
auto = auto[,-ncol(auto)]
cor(auto)
# Multiple linear regression
myFit = lm(mpg ~ cylinders + displacement + horsepower + weight + acceleration + year + origin)
summary(myFit)
# There is a relationship between the predictors and the response.
# displacement, weight, year, origin have a statistically significant relationship to the response.
# The coefficients of year indicates with every unit increase in year, mpg inceases 0.75 unit.
par(mfrow = c(2,2))
plot(myFit)
# From the residuals vs fitted graph we can see that, there is still non-linearity in out model. Plus, the graph suggests some outlier points with high residuals.
# From the residuals vs leverage graph we can see that, most data points are in the low leverage side, although their residual may not be perfectly small. Also, the graph shows high-leverage data points.

# Interaction effects:
This part is not finished yet!
myFit = lm(mpg ~ cylinders * displacement * horsepower * weight + acceleration + year + origin)
plot(myFit)
