# Section 6.8, page 262-263, question 8

X = rnorm(100,3)
hist(X)
noise = rnorm(100, 10)
hist(noise)
B0 = 1
B1 = 2
B2 = 0.3
B3 = 0.04
X2 = X * X
X3 = X2 * X
X4 = X3 * X
X5 = X4 * X
X6 = X5 * X
X7 = X6 * X
X8 = X7 * X
X9 = X8 * X
X10 = X9 * X
Y = B0 + B1*X + B2*X2 + B3*X3 + noise;
plot(Y)
library(leaps)
data = data.frame(Y, X, X2, X3, X4, X5, X6, X7, X8, X9, X10)
summary(data)
# Best subset selection
regfit.full = regsubsets(Y~., data=data, nvmax=10)
regfit.full
reg.summary = summary(regfit.full)
names(reg.summary)
reg.summary$rsq
reg.summary
plot(regfit.full, scale="Cp")
plot(regfit.full, scale="bic")
plot(regfit.full, scale="adjr2")
which.min(reg.summary$cp)
which.min(reg.summary$bic)
which.max(reg.summary$adjr2)
# Forward and backward stepwise selection
regfit.fwd = regsubsets(Y~., data=data, nvmax=10, method="forward")
fwd.summary = summary(regfit.fwd)
plot(regfit.fwd, scale="Cp")
plot(regfit.fwd, scale="bic")
plot(regfit.fwd, scale="adjr2")
which.min(fwd.summary$cp)
which.min(fwd.summary$bic)
which.max(fwd.summary$adjr2)

regfit.bwd = regsubsets(Y~., data=data, nvmax=10, method="backward")
summary(regfit.bwd)
plot(regfit.bwd, scale="Cp")
plot(regfit.bwd, scale="bic")
plot(regfit.bwd, scale="adjr2")
bwd.summary = summary(regfit.bwd)
which.min(bwd.summary$cp)
which.min(bwd.summary$bic)
which.max(bwd.summary$adjr2)

# Lasso
library(glmnet)
predictor = model.matrix(Y~., data)[,-1]
response = data$Y
grid = 10^seq(10,-2,length=100)

lasso.mod = glmnet(predictor, response, alpha=1, lambda=grid)
plot(lasso.mod)
set.seed(1992)
# Make half of the data training data.
train = sample(1:nrow(predictor),nrow(predictor)/2)
cv.out = cv.glmnet(predictor[train,],response[train],alpha=1)
bestlam = cv.out$lambda.min
bestlam
plot(cv.out)
out = glmnet(predictor, response, alpha=1)
lasso.coef = predict(out, type="coefficients", s=bestlam)[1:12,]
lasso.coef[lasso.coef!=0]

# Best subset selection and the lasso given Y = B0 + B7X7 + epsilon
B7 = 0.5
Y2 = B0 + B7*X7 + noise
data2 = data[,-1]
data2 = data.frame(Y2,data2)
# Best subset
regfit.y2 = regsubsets(Y2~.,data,nvmax=10)
data2.summary = summary(regfit.y2)
names(data2.summary)
data2.summary$cp
data2.summary$bic
data2.summary$adjr2
which.min(data2.summary$cp)
which.min(data2.summary$bic)
which.max(data2.summary$adjr2)
coef(regfit.y2,which.min(data2.summary$cp))
coef(regfit.y2,which.min(data2.summary$bic))
coef(regfit.y2,which.max(data2.summary$adjr2))

# lasso
predictor2 = model.matrix(Y2~.,data)[,-1]
response2 = data2$Y2
grid = 10^seq(10,-2,length=100)
lasso.mod2 = glmnet(predictor2,response2,alpha=1,lambda=grid)
set.seed(1992)
train2 = sample(1:nrow(predictor2),nrow(predictor2)/2)
test2 = (-train2)
cv.out2 = cv.glmnet(predictor2[train,],response[train],alpha=1)
bestlam2 = cv.out2$lambda.min
bestlam2
plot(cv.out2)
out2=glmnet(predictor2,response2,alpha=1)
lasso.coef2 = predict(out2,type="coefficients",s=bestlam2)[1:12,]
lasso.coef2[lasso.coef2!=0]
# compute the associated test error
lasso.pred2 = predict(lasso.mod2,s=bestlam2,newx=predictor2[test2,])
mean((lasso.pred2-response2[test2])^2)
