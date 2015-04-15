# Question 2: Section 8.4, page 334, question 9
#a. Splitting the data into training and testing
library(ISLR)
attach(OJ)
train=sample(1:nrow(OJ),800)
train_data=OJ[train,]
test_data=OJ[-train,]

#b. Fitting a tree to the training data
library(tree)
tree_fit=tree(Purchase~.,data=train_data)
summary(tree_fit)

#c. Detailed text output
tree_fit

#d. Plotting the tree
plot(tree_fit)
text(tree_fit,pretty=1)

#e. Confusion Matrix and test error rate
predicted_tree=predict(tree_fit,test_data,type='class')
cm=table(test_data$Purchase,predicted_tree)
1-sum(diag(cm))/sum(cm)
# 0.1851852

#f. Optimal tree
optimal_tree=cv.tree(tree_fit,FUN=prune.tree)

#g. Plot of the optimal size tree
plot(optimal_tree$size, optimal_tree$dev, type="b", xlab = "Tree Size", ylab = "Deviance")

#h.
# The tree with size 7 has the lowest cross-validation classification error rate.

#i. Pruned tree with size 7
tree_pruned = prune.tree(tree_fit, best = 7)

#j. Summary of pruned tree - Training error rates
summary(tree_pruned)

#k. Test error rates
predicted_pruned_tree = predict(tree_pruned,test_data,type='class')
table(predicted_pruned_tree,test_data$Purchase)
  