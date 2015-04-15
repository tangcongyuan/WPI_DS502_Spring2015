# Section 9.7, Page 369, Question 4

set.seed(1234)
x = rnorm(100)
y=exp(x)+exp(-x) + rnorm(100)
train = sample(100, 50)
y[train] = y[train] + 3
y[-train] = y[-train] - 3

# Plot using different colors
plot(x[train], y[train], pch="+", lwd=4, col="red", ylim=c(-4, 20), xlab="X", ylab="Y")
points(x[-train], y[-train], pch="o", lwd=4, col="blue")

#The plot clearly shows non-linear separation. 

#create both train and test dataframes by taking half of +ve and -ve classes 
#and create a new z vector of 0 and 1 for classes.

set.seed(123)
z = rep(0, 100)
z[train] = 1

# Take 25 observations each from train and -train
final_train = c(sample(train, 25), sample(setdiff(1:100, train), 25))
data_train = data.frame(x=x[final_train], y=y[final_train], z=as.factor(z[final_train]))
data_test = data.frame(x=x[-final_train], y=y[-final_train], z=as.factor(z[-final_train]))

library(e1071)
svm_linear = svm(z~., data=data_train, kernel="linear", cost=10)
plot(svm_linear, data_train)
table(z[final_train], predict(svm_linear, data_train))

#The plot shows the linear boundary. The classifier makes 10% classification errors 

#Train an SVM with polynomial kernel
set.seed(1325)
svm_poly = svm(z~., data=data_train, kernel="polynomial", cost=10)
plot(svm_poly, data_train)
table(z[final_train], predict(svm_poly, data_train))

#This is a default polynomial kernel with degree 3. It makes 22% errors on train data.


#SVM with radial basis kernel with gamma of 1.
set.seed(921)
svm_radial = svm(z~., data=data_train, kernel="radial", gamma=1, cost=10)
plot(svm_radial, data_train)
table(z[final_train], predict(svm_radial, data_train))
#This classifier perfectly classifies train data!.


#On Test data, errors look like.

plot(svm_linear, data_test)
table(z[-final_train], predict(svm_linear, data_test))
plot(svm_poly, data_test)
table(z[-final_train], predict(svm_poly, data_test))
plot(svm_radial, data_test)
table(z[-final_train], predict(svm_radial, data.test))

#The tables show that linear, polynomial and radial basis kernels classify 6, 14, and 0 
#test points incorrectly respectively. 
#Radial basis kernel is the best .
