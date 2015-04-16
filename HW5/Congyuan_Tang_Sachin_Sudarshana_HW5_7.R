# a
set.seed(1992)
data = matrix(rnorm(20*3*50, mean = 0, sd = 0.01), ncol = 50)
data = data.frame(data)
data[1:20,1] = 1
data[21:40,1] = 2
data[41:60,1] = 3
label = c(rep(1,20), rep(2,20), rep(3,20))
label = as.factor(label)
# b
pr.out <- prcomp(data)
plot(pr.out$x[, 1:2], col = 1:3, xlab = "Z1", ylab = "Z2", pch = 19)

# c
km.out <- kmeans(data, 3, nstart = 20)
table(label, km.out$cluster)

# d
km.out <- kmeans(data, 2, nstart = 20)
table(label, km.out$cluster)

# e
km.out <- kmeans(data, 4, nstart = 20)
table(label, km.out$cluster)

# f
km.out <- kmeans(pr.out$x[, 1:2], 3, nstart = 20)
table(label, km.out$cluster)

# g
km.out <- kmeans(scale(data), 3, nstart = 20)
table(label, km.out$cluster)


