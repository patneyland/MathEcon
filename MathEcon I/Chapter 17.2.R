# FUNCTION (F: R^2 -> R^1)
f<-function(x,y){
  x^3-y^3+9*x*y
}

# VALUES
x<-seq(0,3,by=0.1)
y<-seq(-3,0,by=0.1)
#x<-seq(-10,10,by=0.1)
#y<-seq(-10,10,by=0.1)

f.val<-matrix(data=NA, nrow=length(x), ncol=length(y))
for(i in 1:length(x)){
  for(j in 1:length(y)){
    f.val[i,j]=f(x[i],y[j])
  }
}

# PLOTS (with critical points of f(x,y))
library(plot3D)
res<-persp3D(x=x, y=y, z=f.val,
        phi=25, theta=20, #Scenario 1: theta=20, Scenario 2: theta=-70
        ticktype="detailed", bty="b2",
        #expand=0.9,
        #col="white", shade=0.5,
        xlab="x", 
        ylab="y", 
        zlab="f(x,y)=x^3-y^3+9*x*y",
        main="")
crit.points<-trans3d(x=c(0,3), y=c(0,-3), z=c(f(0,0),f(3,-3)), pmat=res)
points(crit.points, pch=20, col=2)

library(plotly)
plot_ly(x=x, y=y, z=t(f.val), #transform if f.val's rows (cols) correpond to x (y)
        type="surface",
        showlegend=FALSE,
        showscale=FALSE) %>%  #remove color legend on the side
  add_markers(x=c(0,3), y=c(0,-3), z=c(f(0,0),f(3,-3)),
              marker=list(size=4), color=I("red"), opacity=.5) %>%
  layout(title="",
         scene=list(xaxis=list(title='x'),
                    yaxis=list(title='y'),
                    zaxis=list(title='f(x,y)=x^3-y^3+9*x*y')))



