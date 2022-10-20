# FUNCTIONS
y1<-function(x){
  2*x
}

y1.inv<-function(x){
  0.5*x
}

y2<-function(x){
  sqrt(x)
}

y2.inv<-function(x){
  x^2
}

# VALUES
a1<-seq(-3,3,by=0.01)
a2<-seq(0,3,by=0.01)

# PLOT 1
plot(a1,y1(a1), 
     type="l", main="Functions and their inverses",
     ylab="y",
     xlab="x")
lines(a1,y1.inv(a1),
      type="l",col=2)
abline(h=0, lty=3)
abline(v=0, lty=3)
legend("topleft", 
       legend=c("y=2x",expression(paste("y=",over(1,2), "x"," (inverse function)"))), 
       lty=1,
       col=1:2,
       bty="n")

# PLOT 2
plot(a2,y2(a2), 
     type="l", main="Functions and their inverses",
     ylab="y",
     xlab="x",
     ylim=c(min(a2),max(a2)),
     xlim=c(min(a2),max(a2)))
lines(a2,y2.inv(a2),
      type="l",col=2)
legend("topleft", 
       legend=c(expression(paste("y=",sqrt(x),", ", x,",",y>=0)),expression(paste("y=",x^2, " (inverse function)"))), 
       lty=1,
       col=1:2,
       bty="n")


