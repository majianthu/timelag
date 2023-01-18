library(copent)

tetuan1 = read.csv("~/Rworks/sysid/tetouan.csv", nrows = 5*144, skip = 295*144)

telagn <- function(x,y,lag){
  tl1 = rep(0,lag)
  for(i in 1:lag){
    tl1[i] = transent(tetuan1[,x],tetuan1[,y],i*6)
  }
  tl1
}

para1 = c("Temp.","Humidity","Wind Speed","GDflows","Dflows")
zone1 = c("Quads","Smir","Boussafou")
x11(width = 6, height = 9)
par(mfrow = c(5,3))
for(i in 2:6){
  for(j in 7:9){
    title1 = paste(zone1[j-6],"-",para1[i-1],sep = "")
    te1 = telagn(j,i,24)
    plot(te1,xlab = "hour", ylab = "TE",main = title1);lines(te1);
  }
}
