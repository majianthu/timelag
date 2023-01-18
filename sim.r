library(copent)

l = 500; lag1 = 4
te2 = matrix(0,lag1,8)
for(lag in 1:lag1){
  x = rep(0,l+lag)
  y = rep(0,l+lag)
  for(i in 1:l) {
    # x[i + 1] = rnorm(1, 0, 0.001) # simulation 1
    # x[i + 1] = rnorm(1, 0, 0.001) + sin(2 * pi * i / l) # simulation 2
    x[i + 1] = rnorm(1, 0, 0.001) + x[i] # simulation 3
    y[i + lag + 1] = x[i + 1] + rnorm(1, 0, 0.001)
  }
  x = x[1:l]
  y = y[1:l]
  
  for (i in 1:8) {
    te2[lag, i] = transent(y, x, i)
  }
} #lag
x11();
plot(te2[1,],xlab = "lag", ylab = "TE", ylim = c(min(te2),max(te2)),col=1);lines(te2[1,],col=1)
points(te2[2,],col=2);lines(te2[2,],col=2)
points(te2[3,],col=3);lines(te2[3,],col=3)
points(te2[4,],col=4);lines(te2[4,],col=4)
legend(7,max(te2),legend = c("lag=1","lag=2","lag=3","lag=4"),col = 1:4,pch = "-")

x11(width = 10, height = 4)
plot(x, ylab = "X,Y", col = 2, pch = 20); lines(x,col=2)
points(y, col = 3, pch = 20); lines(y,col=3)
legend(0, max(x), legend = c("X","Y"), col = c(2,3), pch = "-")
