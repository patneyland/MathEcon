# FUNCTIONS
y1<-function(b,x){
  b^x
}

y2<-function(b,x){
  log(x, base=b)
}

# VALUES
b=10
x1<-seq(-1,1,by=0.1)
x2<-seq(0.01,10,by=0.01)

# PLOT
plot(x1,y1(b,x1), 
     type="l", main="Logarithmic and exponential functions",
     ylab="y(x)",
     xlab="x",
     ylim=c(min(y2(b,x2)),max(y1(b,x1))),
     xlim=c(min(x1),max(x2)))
lines(x2,y2(b,x2),
      col=2)
abline(h=0, lty=3)
abline(v=0, lty=3)
legend("topright",
       legend=c(expression(paste("y=",10^x)),
                expression(paste("y=",log[10],"x"))), 
       lty=1,
       col=1:2,
       bty="n")

