K = 100000
prob = array(0, c(1,K))
for (i in 1:K) {
  prob[i] = 1 - ((i-1)/i)^i
}
index = array(1:K)
plot(index, prob, type = "l")
