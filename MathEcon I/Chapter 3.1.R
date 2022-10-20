# FUNCTIONS
y<-function(x){
  x^3-12*x^2+37*x
}

dy<-function(x){
  3*x^2-24*x+37
}

# VALUES
x<-seq(-1,10,by=0.1)

# PLOT
par(mar = c(5,5,4,5))
plot(x,y(x),
     type="l",
     main="First derivative and the shape of the original function")
par(new=TRUE)
plot(x,dy(x),
     type="l",
     axes=FALSE,
     xlab=NA, 
     ylab=NA,
     col=2)
axis(side=4,col=2,col.axis=2)
mtext(side=4, line=3, 'dy(x)', col=2)
abline(h=0, lty=3)
abline(v=x[round(dy(x))==0][1],
       col=4)
abline(v=x[round(dy(x))==0][2],
       col=4)
text(0.3,90,expression(dy(x)>0%=>%y(x)%up%""))
text(4,90,expression(dy(x)<0%=>%y(x)%down%""))
text(8,90,expression(dy(x)>0%=>%y(x)%up%""))
