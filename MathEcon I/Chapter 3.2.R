# FUNCTIONS
y<-function(x){
  x^3-12*x^2+37*x
}

dy<-function(x){
  3*x^2-24*x+37
}

ddy<-function(x){
  6*x-24
}

# VALUES
x<-seq(-1,10,by=0.1)

# PLOT
layout(matrix(1:3, ncol=1), widths=1, heights=c(2,1.5,2), respect=FALSE)
par(mar=c(0, 4.1, 4.1, 2.1))
plot(x,y(x), 
     type="l", xaxt="n", main="First and second derivatives and the shape of the original function")
abline(v=x[round(dy(x))==0][1],
       col=4)
abline(v=x[round(dy(x))==0][2],
       col=4)
abline(v=x[dy(x)==min(dy(x))],
       col=2)
text(0.3,150,expression(dy(x)>0%=>%y(x)%up%""))
text(0.3,130,expression(ddy(x)<0%=>%dy(x)%down%""))
text(0.3,110,expression("Concave down inc."))
text(0.3,90,expression("(Inc. at dec. rate)"))
text(3.1,150,expression(dy(x)<0%=>%y(x)%down%""))
text(3.1,130,expression(ddy(x)<0%=>%dy(x)%down%""))
text(3.1,110,expression("Concave down dec."))
text(3.1,90,expression("(Dec. at dec. rate)"))
text(5,150,expression(dy(x)<0%=>%y(x)%down%""))
text(5,130,expression(ddy(x)>0%=>%dy(x)%up%""))
text(5,110,expression("Concave up dec."))
text(5,90,expression("(Dec. at inc. rate)"))
text(7.5,150,expression(dy(x)>0%=>%y(x)%up%""))
text(7.5,130,expression(ddy(x)>0%=>%dy(x)%up%""))
text(7.5,110,expression("Concave up inc."))
text(7.5,90,expression("(Inc. at inc. rate)"))
par(mar = c(0, 4.1, 0, 2.1))
plot(x,dy(x), 
     type="l",xaxt="n")
abline(h=0, lty=3)
abline(v=x[round(dy(x))==0][1],
       col=4)
abline(v=x[round(dy(x))==0][2],
       col=4)
abline(v=x[dy(x)==min(dy(x))],
       col=2)
par(mar = c(4.1, 4.1, 0, 2.1))
plot(x,ddy(x), 
     type="l")
abline(h=0, lty=3)
abline(v=x[dy(x)==min(dy(x))],
       col=2)


