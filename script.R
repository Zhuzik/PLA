library(ggplot2)

n=100
answer<-NULL
probability<-NULL

for(pp in 1:1000) { 

x1<-runif(n, min = -1, max = 1)
x2<-runif(n, min = -1, max = 1)

# line
dot1<-runif(2, min = -1, max = 1)
dot2<-runif(2, min = -1, max = 1)
a<-(dot2[2]-dot1[2])/(dot2[1]-dot1[1])
b<-dot1[2]-a*dot1[1]

y <- as.numeric((x1*a + b) > x2)*2 - 1

qplot(x1,x2, col= as.factor(y)) + geom_abline(intercept = b, slope = a, col="green")

data <- data.frame(x1,x2,y)
w0<-0
w1<-0
w2<-0

iteration <- 1

h<-sign(w1*x1+w2*x2+w0)
z<-as.numeric(h!=data$y)
data$z<-z
frame<-subset(data,data$z==1)
while (nrow(frame)!=0) {
      samp<-sample(nrow(frame))[1]
      w0<-w0+(frame$y[samp])*1
      w1<-w1+(frame$y[samp])*(frame$x1[samp])
      w2<-w2+(frame$y[samp])*(frame$x2[samp])
      iteration <-iteration+1
      h<-sign(w1*x1+w2*x2+w0)
      z<-as.numeric(h!=data$y)
      data$z<-z
      frame<-subset(data,data$z==1)
      }
answer<-c(answer,iteration)

nx1<-runif(1000, min = -1, max = 1)
nx2<-runif(1000, min = -1, max = 1)

ynew<-as.numeric((nx1*a + b) > nx2)*2 - 1
hnew<-sign(w1*nx1+w2*nx2+w0)

prob<-sum(as.numeric((ynew!=hnew)))/1000
probability<-c(probability,prob)
}

sum(answer)/1000
sum(probability)/1000
