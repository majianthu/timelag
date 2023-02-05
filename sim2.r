library(copent)

l = 500; lag1 = 4; alpha = 0.2; beta = 0.8
te2 = matrix(0,lag1,8)
for(lag in 1:lag1){
  x = rnorm(l+lag,0,0.1)
  for(i in (lag+1):(lag+l)) {
    x[i] = rnorm(1, 0, 0.1) + alpha * x[i-1] + beta * x[i - lag] # simulation 4
  }
  x = x[(lag+1):(lag+l)]
  for (i in 1:8) {
    te2[lag, i] = transent(x, x, i)
  }
} #lag
x11();
plot(te2[1,],xlab = "lag", ylab = "TE", ylim = c(min(te2),max(te2)),col=1);lines(te2[1,],col=1)
points(te2[2,],col=2);lines(te2[2,],col=2)
points(te2[3,],col=3);lines(te2[3,],col=3)
points(te2[4,],col=4);lines(te2[4,],col=4)
legend(7,max(te2),legend = c("lag=1","lag=2","lag=3","lag=4"),col = 1:4,pch = "-")

x11(width = 10, height = 4)
plot(x, ylab = "X", ylim = c(min(x),max(x)), col = 2, pch = 20); lines(x, col = 2)
