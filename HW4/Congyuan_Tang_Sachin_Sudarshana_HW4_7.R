# The “Wage” data set contains a number of other features nor explored in this chapter, such as marital status (“marit1”), job class (“jobclass”), and others. Explore the relationships between some of these other predictors and “wage”, and use non-linear fitting techniques in order to fit flexible models to the data. Create plots of the results obtained, and write a summary of your findings.
library(ISLR)
attach(Wage)
set.seed(1)
summary(maritl)
summary(jobclass)
summary(Wage)

par(mfrow = c(1, 2))
plot(Wage$maritl, Wage$wage)
plot(Wage$jobclass, Wage$wage)
summary(jobclass)
# We may conclude that a married couple earns more money on average, and also that informational jobs earns more on average. We will now use GAM to predict “wage” using natural spline functions of “year”, “age”, “education”, “jobclass” and “maritl”.
library(gam)
fit0 <- gam(wage ~ lo(year, span = 0.7) + s(age, 5) + education, data = Wage)
fit1 <- gam(wage ~ lo(year, span = 0.7) + s(age, 5) + education + jobclass, data = Wage)
fit2 <- gam(wage ~ lo(year, span = 0.7) + s(age, 5) + education + maritl, data = Wage)
fit3 <- gam(wage ~ lo(year, span = 0.7) + s(age, 5) + education + jobclass + maritl, data = Wage)
anova(fit0, fit1, fit2, fit3)

# plot
par(mfrow = c(2, 3))
plot(fit3, se = T, col = "blue")
