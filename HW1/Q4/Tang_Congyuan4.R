# This is the question4 in homework1 for WPI DS502 Statistical Method for Data Science.

# Set working directory.
setwd('/Users/Eric/GoogleDrive/2015@WPI/DS502/HW1/')
# Read in college dataset.
college <- read.csv('College.csv')
# Set DataFrame's rowname to college names, and its not a data column.
rownames(college) = college[,1]
# Examine the dataset.
fix(college)
# Remove college names from original dataset.
college = college[,-1]
# Examine the dataset.
fix(college)
# Numerical summary of the variables in the dataset.
summary(college)
# Scatter plot of the first ten columns of the dataset.
pairs(college[,1:10])
# Boxplots of Outstate and Private
plot(as.factor(college$Private), college$Outstate)
# Binning the Top10perc variable.
Elite = rep("No", nrow(college))
Elite[college$Top10perc > 50] = "yes"
Elite = as.factor(Elite)
college = data.frame(college, Elite)
# Examine Elite
summary(Elite)
# Boxplots of Outstate verse Elite
plot(as.factor(college$Elite), college$Outstate)
# Plot histogram
par(mfrow=c(2,2))
hist(college$Apps)
hist(college$Accept)
hist(college$Enroll)
hist(college$Top10perc)

par(mfrow=c(1,1))
hist(college$Top10perc)
# Interesting findings:
# After ploting the histogram of college$Top10perc, we find the most common frequency is still around 10%.
# It suggests that most colleges are just as selective as high schools are. However, there are some colleges which is very selective with Top10perc higher than 80%.
# This also suggests that in general, colleges are not extremely selective.
par(mfrow=c(1,1))
plot(college$Apps, college$Accept)
highRatio = (college$Apps/college$Accept) > 5
college[highRatio,]
# Interesting findings 2:
# Most school recieves more applications will also accept more students, but there are also some colleges with high application number and low acceptance number.
# The most selective university (with application number five times more than the actual acceptance number) are Harvard and Princeton.

