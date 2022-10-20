# QUADRATIC FORMS (Q: R^2 -> R^1)
q1<-function(x1,x2){
  x1^2+x2^2
}

q2<-function(x1,x2){
  -x1^2-x2^2
}

q3<-function(x1,x2){
  x1^2-x2^2
}

q4<-function(x1,x2){
  (x1+x2)^2
}

q5<-function(x1,x2){
  -(x1+x2)^2
}

# VALUES
x1<-seq(-10,10,by=0.1)
x2<-seq(-10,10,by=0.1)

q1.val<-matrix(data=NA, nrow=length(x1), ncol=length(x2))
for(i in 1:length(x1)){
  for(j in 1:length(x2)){
    q1.val[i,j]=q1(x1[i],x2[j])
  }
}

q2.val<-matrix(data=NA, nrow=length(x1), ncol=length(x2))
for(i in 1:length(x1)){
  for(j in 1:length(x2)){
    q2.val[i,j]=q2(x1[i],x2[j])
  }
}

q3.val<-matrix(data=NA, nrow=length(x1), ncol=length(x2))
for(i in 1:length(x1)){
  for(j in 1:length(x2)){
    q3.val[i,j]=q3(x1[i],x2[j])
  }
}

q4.val<-matrix(data=NA, nrow=length(x1), ncol=length(x2))
for(i in 1:length(x1)){
  for(j in 1:length(x2)){
    q4.val[i,j]=q4(x1[i],x2[j])
  }
}

q5.val<-matrix(data=NA, nrow=length(x1), ncol=length(x2))
for(i in 1:length(x1)){
  for(j in 1:length(x2)){
    q5.val[i,j]=q5(x1[i],x2[j])
  }
}

# PLOTS
# Quadratic form 1
library(plot3D)
persp3D(x=x1, y=x2, z=q1.val,
        phi=25, theta=20, 
        ticktype="detailed", bty="b2",
        #expand=0.9,
        #col="white", shade=0.5,
        xlab="x1", 
        ylab="x2", 
        zlab="q(x1,x2)=x1^2+x2^2",
        main="Positive definite quadratic form")

library(plotly)
plot_ly(x=x1, y=x2, z=t(q1.val),#transform if q1.val's rows (cols) correpond to x1 (x2)
        type="surface",
        showlegend=FALSE,
        showscale=FALSE) %>%    #remove color legend on the side
  add_markers() %>%
  layout(title="Positive definite quadratic form",
         scene=list(xaxis=list(title='x1'),
                    yaxis=list(title='x2'),
                    zaxis=list(title='q(x1,x2)=x1^2+x2^2')))

# Quadratic form 2
persp3D(x=x1, y=x2, z=q2.val,
        phi=25, theta=20, 
        ticktype="detailed", bty="b2",
        #expand=0.9,
        #col="white", shade=0.5,
        xlab="x1", 
        ylab="x2", 
        zlab="q(x1,x2)=-x1^2-x2^2",
        main="Negative definite quadratic form")

plot_ly(x=x1, y=x2, z=t(q2.val),
        type="surface",
        showlegend=FALSE,
        showscale=FALSE) %>% #remove color legend on the side
  add_markers() %>%
  layout(title="Negative definite quadratic form",
         scene=list(xaxis=list(title='x1'),
                    yaxis=list(title='x2'),
                    zaxis=list(title='q(x1,x2)=-x1^2-x2^2')))

# Quadratic form 3
persp3D(x=x1, y=x2, z=q3.val,
        phi=25, theta=20, 
        ticktype="detailed", bty="b2",
        #expand=0.9,
        #col="white", shade=0.5,
        xlab="x1", 
        ylab="x2", 
        zlab="q(x1,x2)=x1^2-x2^2",
        main="Indefinite quadratic form")

plot_ly(x=x1, y=x2, z=t(q3.val),
        type="surface",
        showlegend=FALSE,
        showscale=FALSE) %>% #remove color legend on the side
  add_markers() %>%
  layout(title="Indefinite quadratic form",
         scene=list(xaxis=list(title='x1'),
                    yaxis=list(title='x2'),
                    zaxis=list(title='q(x1,x2)=x1^2-x2^2')))

# Quadratic form 4
persp3D(x=x1, y=x2, z=q4.val,
        phi=25, theta=20, 
        ticktype="detailed", bty="b2",
        #expand=0.9,
        #col="white", shade=0.5,
        xlab="x1", 
        ylab="x2", 
        zlab="q(x1,x2)=(x1+x2)^2",
        main="Positive semidefinite quadratic form")

plot_ly(x=x1, y=x2, z=t(q4.val),
        type="surface",
        showlegend=FALSE,
        showscale=FALSE) %>% #remove color legend on the side
  add_markers() %>%
  layout(title="Positive semidefinite quadratic form",
         scene=list(xaxis=list(title='x1'),
                    yaxis=list(title='x2'),
                    zaxis=list(title='q(x1,x2)=(x1+x2)^2')))

# Quadratic form 5
persp3D(x=x1, y=x2, z=q5.val,
        phi=25, theta=20, 
        ticktype="detailed", bty="b2",
        #expand=0.9,
        #col="white", shade=0.5,
        xlab="x1", 
        ylab="x2", 
        zlab="q(x1,x2)=-(x1+x2)^2",
        main="Negative semidefinite quadratic form")

plot_ly(x=x1, y=x2, z=t(q5.val),
        type="surface",
        showlegend=FALSE,
        showscale=FALSE) %>% #remove color legend on the side
  add_markers() %>%
  layout(title="Negative semidefinite quadratic form",
         scene=list(xaxis=list(title='x1'),
                    yaxis=list(title='x2'),
                    zaxis=list(title='q(x1,x2)=-(x1+x2)^2')))


