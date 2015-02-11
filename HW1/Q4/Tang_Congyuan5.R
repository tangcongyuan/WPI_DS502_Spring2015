# This is the question5 in homework1 for WPI DS502 Statistical Method for Data Science.
# Be sure to check Auto.csv file has no missing value.

# Set working directory.
setwd('/Users/Eric/GoogleDrive/2015@WPI/DS502/HW1/')
# Read in the dataset.
auto <- read.csv('Auto.csv')
# Summary of the dataset.
summary(auto)
# From the summary we can see, that 'mpg', 'cylinders', 'displacement', 'weight', 'acceleration', 'year', 'origin', are quantitative.
# 'horsepower', 'name' are qualitative.

# Finding range of each quantitative predictor.
range(auto$mpg)
range(auto$cylinders)
range(auto$displacement)
range(auto$weight)
range(auto$acceleration)
range(auto$year)
range(auto$origin)

# Mean and standard deviation of each quantitative predictor.
mean(auto$mpg)
sd(auto$mpg)

mean(auto$cylinders)
sd(auto$cylinders)

mean(auto$displacement)
sd(auto$displacement)

mean(auto$weight)
sd(auto$weight)

mean(auto$acceleration)
sd(auto$acceleration)

mean(auto$year)
sd(auto$year)

mean(auto$origin)
sd(auto$origin)

# Remove from 10th to 85th records and culculate Range, Mean, Standard Deviation again.
t1 = rep(TRUE,9)
t2 = rep(FALSE, 76)
t3 = rep(TRUE, nrow(auto)-85)
t = c(t1, t2, t3)
auto = auto[t,]
# Finding range of each quantitative predictor.
range(auto$mpg)
range(auto$cylinders)
range(auto$displacement)
range(auto$weight)
range(auto$acceleration)
range(auto$year)
range(auto$origin)

# Mean and standard deviation of each quantitative predictor.
mean(auto$mpg)
sd(auto$mpg)

mean(auto$cylinders)
sd(auto$cylinders)

mean(auto$displacement)
sd(auto$displacement)

mean(auto$weight)
sd(auto$weight)

mean(auto$acceleration)
sd(auto$acceleration)

mean(auto$year)
sd(auto$year)

mean(auto$origin)
sd(auto$origin)

# Read in Auto.csv dataset again to get full dataset.
auto <- read.csv('Auto.csv')
# Try to find interesting stuff.
plot(auto$cylinders, auto$mpg)
plot(as.factor(auto$cylinders), auto$mpg)
# It is obvious that even number of cylinders are more common in cars, and in average, with more cylinders, the less mpg would be.

plot(auto$cylinders, auto$acceleration)
plot(as.factor(auto$cylinders), auto$acceleration)
# It is also obvious that in average, 4-cylinder cars have the same acceleration of 6-cylinder cars, but have more acceleration than 8-cylinder cars (due to lack of records, we ignore comparing to odd-cylinder cars).
# Predict mpg and justify it.
# These four predictors are helpful.
par(mfrow = c(2,2))
plot(auto$cylinders, auto$acceleration)
plot(auto$displacement, auto$mpg)
plot(as.numeric(auto$horsepower), auto$mpg)
plot(auto$weight, auto$mpg)
# These three predictors are not very helpful.
par(mfrow = c(2,2))
plot(auto$acceleration, auto$mpg)
plot(auto$year, auto$mpg)
plot(auto$origin, auto$mpg)

