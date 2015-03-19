# HW-3 Question - 2 - Section 6.8, Page 261, Q6

#(a) - Consider (6.12) with p = 1. For some choice of y1 and λ > 0, plot (6.12) as a function of β1. 
# Your plot should confirm that (6.12) is solved by (6.14).
lambda = 3
yhat = 3
betas = seq(from=-5, to= 5,0.1)

# Equ 6.12
yR = ((yhat-betas)^2+lambda*(betas^2))
plot(betas, xlab="beta-values", yR, ylab="y",pch=20)
points(betas[which.min(yR)],yR[which.min(yR)], col="red",pch=20,lwd=4)

# Equ 6.14
abline(v=yhat/(1+lambda),col="red") 

# We can see that the line given by Eq. 6.14 passes through the same point as given by Eq. 6.12


#(b) -  Consider (6.13) with p = 1. For some choice of y1 and λ > 0, plot (6.13) as a function of β1. 
# Your plot should confirm that (6.13) is solved by(6.15).
lambda = 2
yhat1 = 4
betas = seq(from=-5, to= 5,0.1)

# Equ 6.13
yR = ((yhat1 - betas)^2 + lambda * abs(betas))
plot(betas, xlab="beta-values", yR, ylab="y",pch=20)
points(betas[which.min(yR)],yR[which.min(yR)], col="red",pch=20)

# Equ 6.15
abline(v=yhat1-(lambda/2),col="red",lwd=3)

# We can see that the line given by Eq. 6.15 passes through the same point as given by Eq. 6.13