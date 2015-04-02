# Section 8.4, Page 332, question 1

par(xpd = NA)
plot(NA, NA, type = "n", xlim = c(0, 200), ylim = c(0, 200), xlab = "X", ylab = "Y")
# t1
lines(x = c(40, 40), y = c(0, 205))
text(x = 40, y = 205, labels = c("t1"), col = "red")
# t2
lines(x = c(0, 40), y = c(75,75))
text(x = -3, y = 75, labels = c("t2"), col = "red")
# t3: x = 75; (75,0) (75, 100)
lines(x = c(100, 100), y = c(0, 200))
text(x = 100, y = 205, labels = c("t3"), col = "red")
# t4: x = 20; (20,0) (20, 75)
lines(x = c(100, 40), y = c(100, 100))
text(x = 105, y = 100, labels = c("t4"), col = "red")

lines(x = c(100, 40), y = c(150, 150))
text(x = 105, y = 150, labels = c("t5"), col = "red")


text(x = (40 + 75)/2, y = 50, labels = c("R1"))
text(x = 20, y = (75)/2, labels = c("R2"))
text(x = 125, y = 120, labels = c("R3"))
text(x = (75 + 100)/2, y = 125, labels = c("R4"))
text(x = 30, y = 150, labels = c("R5"))
text(x = 75, y = 175, labels = c("R6"))

