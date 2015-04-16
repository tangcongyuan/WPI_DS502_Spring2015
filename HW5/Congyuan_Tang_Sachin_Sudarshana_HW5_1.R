# plot Gini index 
Gini = rep(0, 101)
i = 0
p1 = seq(0,1,by=0.01)
for (p in p1) {
  Gini[i] = p * (1 - p)
  i = i + 1
}
plot(p1, Gini, type = "l")

# plot cross-entropy
crossEntropy = rep(0, 101)
i = 0
p1 = seq(0,1,by=0.01)
for (p in p1) {
  crossEntropy[i] = -p * log2(p) - (1-p) * log2(1-p)
  i = i + 1
}
plot(p1, crossEntropy, type="l")

# plot classification error
error = rep(0, 101)
i = 0
p1 = seq(0,1,by=0.01)
for (p in p1) {
  error[i] = 1 - p
  i = i + 1
}
plot(p1, error, type="l")
