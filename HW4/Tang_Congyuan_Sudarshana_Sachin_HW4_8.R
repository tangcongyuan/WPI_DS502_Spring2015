# Problem 8: Section 8.4, Page 333-334, question 8

library(ISLR)
attach(Carseats)
set.seed(1)

#a: Splitting data into training and testing
train = sample(dim(Carseats)[1], dim(Carseats)[1]/2)
Carseats.train = Carseats[train, ]
Carseats.test = Carseats[ .33*train, ]

#b: Fitting a regression tree to training set
library(tree)
mytree = tree(Sales ~ ., data = Carseats.train)
summary(mytree)
plot(mytree)
text(mytree, pretty = 2)
pred.carseats = predict(mytree, Carseats.test)
mean((Carseats.test$Sales - pred.carseats)^2)
# [1] 3.020105


#c: Cross validation
cv.carseats = cv.tree(mytree, FUN = prune.tree)
par(mfrow = c(1, 2))
plot(cv.carseats$size, cv.carseats$dev, type = "b")
plot(cv.carseats$k, cv.carseats$dev, type = "b")

# Observing the plot, best size for CV = 8
pruned.carseats = prune.tree(mytree, best = 8)
par(mfrow = c(1, 1))
plot(pruned.carseats)
text(pruned.carseats, pretty = 0)

pred.pruned = predict(pruned.carseats, Carseats.test)
mean((Carseats.test$Sales - pred.pruned)^2)
# [1] 4.002948

#d: Bagging
library(randomForest)
mybag = randomForest(Sales ~ ., data = Carseats.train, mtry = 10, ntree = 500, 
                     importance = T)
bag.pred = predict(mybag, Carseats.test)
mean((Carseats.test$Sales - bag.pred)^2)
# [1] 1.892222
importance(mybag)

#e: Random forest
myrf = randomForest(Sales ~ ., data = Carseats.train, mtry = 5, ntree = 500, 
                    importance = T)
rf.pred = predict(myrf, Carseats.test)
mean((Carseats.test$Sales - rf.pred)^2)
#[1] 2.128623
importance(myrf)
