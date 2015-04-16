library(plotrix)

set.seed(1992)
x1 = runif(500) * 2 - 1
x2 = runif(500) * 2 - 1

index = x1^2 + x2^2
index1 = index > 0.5^2
index2 = index <= 0.5^2

blue = data.frame(x1[index1], x2[index1])
red = data.frame(x1[index2], x2[index2])

# the order of displaying following graphs is important!
plot(x1, x2, asp = 1)
draw.circle(0,0,0.5,border = "red")
points(blue, col="blue")
points(red, col="red")

# fit logistic regression
y = 1 * (x1^2 + x2^2 > 0.5^2)
data = data.frame(x1, x2, y)
summary(data)
logit.fit = glm(y ~ x1 + x2, family = "binomial")
summary(logit.fit)

probs <- predict(logit.fit, data, type = "response")
preds <- rep(0, 500)
preds[probs > 0.5] = 1
plot(data[preds == 1, ]$x1, data[preds == 1, ]$x2, col = "blue", pch = 2, xlab = "X1", ylab = "X2", asp=1)
points(data[preds == 0, ]$x1, data[preds == 0, ]$x2, col = "red", pch = 3)
draw.circle(0,0,0.5,border = "red")

# logistic regression with non-linear predictors
logitnl.fit <- glm(y ~ poly(x1, 2) + poly(x2, 2) + I(x1 * x2), family = "binomial")
summary(logitnl.fit)

probs <- predict(logitnl.fit, data, type = "response")
preds <- rep(0, 500)
preds[probs > 0.5] = 1
plot(data[preds == 1, ]$x1, data[preds == 1, ]$x2, col = "blue", pch = 2, xlab = "X1", ylab = "X2", asp=1)
points(data[preds == 0, ]$x1, data[preds == 0, ]$x2, col = "red", pch = 3)
draw.circle(0,0,0.5,border = "red")

# Fit a support vector classifier to the data with X1 and X2 as predictors.
library(e1071)
data$y <- as.factor(data$y)
svm.fit <- svm(y ~ x1 + x2, data, kernel = "linear", cost = 0.01)
preds <- predict(svm.fit, data)
# still, the order matters.
plot(data[preds == 1, ]$x1, data[preds == 1, ]$x2, col = "blue", pch = 2, xlab = "X1", ylab = "X2", asp=1)
points(data[preds == 0, ]$x1, data[preds == 0, ]$x2, col = "red", pch = 3)
draw.circle(0,0,0.5,border = "red")

# Fit a SVM using a non-linear kernel to the data with X1 and X2 as predictors.
data$y <- as.factor(data$y)
svmnl.fit <- svm(y ~ x1 + x2, data, kernel = "radial", gamma = 10000)
preds <- predict(svmnl.fit, data)
plot(data[preds == 1, ]$x1, data[preds == 1, ]$x2, col = "blue", pch = 2, xlab = "X1", ylab = "X2", asp=1)
points(data[preds == 0, ]$x1, data[preds == 0, ]$x2, col = "red", pch = 3)
draw.circle(0,0,0.5,border = "red")






