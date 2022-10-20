# IMPLICIT FUNCTIONS
x<-seq(-5,5,by=0.1)
y<-seq(-5,5,by=0.1)
z1<-outer(x,y,function(y,x) 2*y^2+x^2)
z2<-outer(x,y,function(y,x) x*y^2+2*y+x^2)

# PLOTS
contour(x,y,z1,
        main=as.expression(bquote("2"~y^2~"+"~x^2~"=a, "~a>=0~" is a constant")),
        xlab="x",
        ylab="y")
contour(x,y,z1,
        levels=10, #a=10
        xlab="x",
        ylab="y",
        col=2,
        add=TRUE)
legend("center",c("a=10"),lty=1,col=2,bty="n")

contour(x,y,z2,
        main=as.expression(bquote("x"~y^2~"+2y+"~x^2~"=a, "~a~" is a constant")),
        xlab="x",
        ylab="y")
contour(x,y,z2,
        levels=0, #a=0
        xlab="x",
        ylab="y",
        add=TRUE,
        col=2)
legend(0,1,c("a=0"),lty=1,col=2,bty="n")

