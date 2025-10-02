library(copent)

l = 500; lag1 = 4; alpha = 0.2; beta = 0.8
te2 = matrix(0,lag1,8)
for(lag in 1:lag1){
  y = x = rnorm(l+lag,0,0.1)
  for(i in (lag+1):(lag+l)) {
    x[i] = rnorm(1, 0, 0.1) + alpha * x[i-1] + beta * x[i - lag] # simulation 4
    # y[i] = x[i] + rnorm(1,0,0.001)
    y[i] = x[i]^2 + sin(x[i]) + rnorm(1,0,0.001)
  }
  x = x[(lag+1):(lag+l)]
  for (i in 1:8) {
    te2[lag, i] = transent(y, x, i)
  }
} #lag

x11();
plot(te2[1,],xlab = "lag", ylab = "TE", ylim = c(min(te2),max(te2)),col=1,pch = 1);lines(te2[1,],col=1)
points(te2[2,],col=2,pch=2);lines(te2[2,],col=2)
points(te2[3,],col=3,pch=5);lines(te2[3,],col=3)
points(te2[4,],col=4,pch=22);lines(te2[4,],col=4)
legend(7,max(te2),legend = c("l=1","l=2","l=3","l=4"),col = 1:4,pch = c(1,2,5,22))

x11(width = 15, height = 6)
plot(x, xlab = "i", ylab = "X,Y", ylim = c(min(x,y),max(x,y)), col = 2, pch = 1); lines(x,col=2)
points(y, col = 3, pch = 20); lines(y,col=3)
legend(-15, max(x,y), legend = c("X","Y"), col = c(2,3), pch = c(1,20))
