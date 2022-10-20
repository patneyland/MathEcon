# FUNCTIONS OF MULTIPLE VARIABLES (f: R^2 -> R^1)
f1<-function(x,y){
  x^2+y^2
}

f2<-function(x,y){
  y^2-x^2
}

# VALUES
x<-seq(-10,10,by=0.1)
y<-seq(-10,10,by=0.1)

f1.val<-matrix(data=NA, nrow=length(x), ncol=length(y))
for(i in 1:length(x)){
  for(j in 1:length(y)){
    f1.val[i,j]=f1(x[i],y[j])
  }
}

f2.val<-matrix(data=NA, nrow=length(x), ncol=length(y))
for(i in 1:length(x)){
  for(j in 1:length(y)){
    f2.val[i,j]=f2(x[i],y[j])
  }
}

# PLOTS
# Function 1
library(plot3D)
persp3D(x=x, y=y, z=f1.val,
        phi=25, theta=20, 
        ticktype="detailed", bty="b2",
        #expand=0.9,
        #col="white", shade=0.5,
        xlab="x", 
        ylab="y", 
        zlab="f(x,y)=x^2+y^2")

ribbon3D(z=f1.val[seq(1,nrow(f1.val), length.out=20),],
         along="y",
         space=0.8,
         phi=25, theta=20, 
         ticktype="detailed", bty="b2",
         #expand=0.9,
         #col="white", shade=0.5,
         xlab="x", 
         ylab="y", 
         zlab="f(x,y)=x^2+y^2")

library(RColorBrewer)
my.cols<-rev(brewer.pal(10, "RdYlBu"))
contour(x=x, y=y, z=f1.val,
        nlevels=10, 
        col=my.cols,
        lwd=2,
        xlab="x", 
        ylab="y", 
        main=as.expression(bquote("f(x,y)="~x^2~"+"~y^2)))

# Function 2
persp3D(x=x, y=y, z=f2.val,
        phi=25, theta=20, 
        ticktype="detailed", bty="b2",
        #expand=0.9,
        #col="white", shade=0.5,
        xlab="x", 
        ylab="y", 
        zlab="f(x,y)=y^2-x^2")

ribbon3D(z=f2.val[seq(1,nrow(f2.val), length.out=20),],
         along="y",
         space=0.8,
         phi=25, theta=20, 
         ticktype="detailed", bty="b2",
         #expand=0.9,
         #col="white", shade=0.5,
         xlab="x", 
         ylab="y", 
         zlab="f(x,y)=y^2-x^2")

my.cols<-rev(brewer.pal(10, "RdYlBu"))
contour(x=x, y=y, z=f2.val,
        nlevels=10, 
        col=my.cols,
        lwd=2,
        xlab="x", 
        ylab="y", 
        main=as.expression(bquote("f(x,y)="~y^2~"-"~x^2)))

# FANCY PLOTS
# Function 1
library(plotly)
plot_ly(x=x, y=y, z=t(f1.val), #transform if f1.val's rows (cols) correpond to x (y)
        type="surface",
        showlegend=FALSE,
        showscale=FALSE) %>%   #remove color legend on the side
  add_markers() %>%
  layout(scene=list(xaxis=list(title='x'),
                    yaxis=list(title='y'),
                    zaxis=list(title='f(x,y)=x^2+y^2')))

plot_ly(x=x, y=y, z=t(f1.val),
        type="contour",
        showlegend=TRUE,
        showscale=TRUE)

# Function 2
plot_ly(x=x, y=y, z=t(f2.val),
        type="surface",
        showlegend=FALSE,
        showscale=FALSE) %>% #remove color legend on the side
  add_markers() %>%
  layout(scene=list(xaxis=list(title='x'),
                    yaxis=list(title='y'),
                    zaxis=list(title='f(x,y)=y^2-x^2')))

plot_ly(x=x, y=y, z=t(f2.val),
        type="contour",
        showlegend=TRUE,
        showscale=TRUE)

