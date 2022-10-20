# FUNCTIONS
y<-function(b,x){
  b^x
}

# VALUES
x<-seq(-3,3,by=0.1)

# PLOT
plot(x,y(2,x), 
     type="l", main="Exponential functions",
     ylab="y(x)")
abline(h=0, lty=3)
abline(v=0, lty=3)
lines(x,y(exp(1),x),
      col=2)
lines(x,y(3,x),
      col=3)
lines(x,y(4,x),
      col=4)
lines(x,y(0.2,x),
      col=5)
lines(x,y(0.3,x),
      col=6)
lines(x,y(0.4,x),
      col=7)
legend("topleft", 
       legend=c(expression(paste("y=",2^x)),
                expression(paste("y=",e^x)),
                expression(paste("y=",3^x)),
                expression(paste("y=",4^x)),
                expression(paste("y=",0.2^x)),
                expression(paste("y=",0.3^x)),
                expression(paste("y=",0.4^x))), 
       lty=1,
       col=1:7,
       bty="n")

