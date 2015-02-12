# This is the question9 in homework1 for WPI DS502 Statistical Method for Data Science.
# Read in dataset.
library(ISLR)
auto = ISLR::Auto
summary(auto)
# The standard way in R to fit a linear model.
myFit <- lm(mpg ~ horsepower, data=auto)
# Summary the model.
summary(myFit)
# The extremely small number of p-value suggest it is very likely, almost certain, to say the null hypothesis is true, which is equivalent to say there is a relationship between the predictor and the response.
# By ploting horsepower verse mpg, we could see the relationship between predictor and response is negative.
plot(auto$horsepower, auto$mpg)
# Predict mpg associated with horsepower = 98 and confidence interval.
predict(myFit, data.frame(horsepower=(c(98))), interval = "confidence")
# Predict mpg associated with horsepower = 98 and prediction interval.
predict(myFit, data.frame(horsepower=(c(98))), interval = "prediction")

# Plot the response and the predictor, using abline() function to display the least squares regression line.
abline(myFit)
# Plot diagnostic plots.
par(mfrow = c(2,2))
plot(myFit)
# Residual verse fitted plot show a discernible pattern, which indicates a problem with our linear model.
# The residuals vs fitted graph, scale-location along with residuals vs leverage graph clearly shows the outliers.


