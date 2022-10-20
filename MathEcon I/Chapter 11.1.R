# TWO-DIMENSIONAL CASE

# SCENARIO 1: LINEAR INDEPENDENCE
# Vector 1, with initial point (0,0)
v1<-c(1,3)
x1.v1<-v1[1]
x2.v1<-v1[2]

# Vector 2, with initial point (0,0)
v2<-c(-1,6)
x1.v2<-v2[1]
x2.v2<-v2[2]

# # SCENARIO 2: LINEAR DEPENDENCE - SEVERE
# # Vector 1, with initial point (0,0)
# v1<-c(1,3)
# x1.v1<-v1[1]
# x2.v1<-v1[2]
# 
# # Vector 2, with initial point (0,0)
# v2<-c(2,6)
# x1.v2<-v2[1]
# x2.v2<-v2[2]

# # SCENARIO 3: LINEAR DEPENDENCE - MODERATE
# # Vector 1, with initial point (0,0)
# v1<-c(1,3)
# x1.v1<-v1[1]
# x2.v1<-v1[2]
# 
# # Vector 2, with initial point (0,0)
# v2<-c(2,10)
# x1.v2<-v2[1]
# x2.v2<-v2[2]

# r1*v1=r1*(x1,x2)
r1x1.v1<-function(x1,r1){
  r1*x1
}

r1x2.v1<-function(x2,r1){
  r1*x2
}

# r2*v2=r2*(x1,x2)
r2x1.v2<-function(x1,r2){
  r2*x1
}

r2x2.v2<-function(x2,r2){
  r2*x2
}

# r1 in R
r1<-seq(-5,7,by=0.1)

# r2 in R
r2<-seq(-7,5,by=0.1)

# The set V spanned by (v1,v2): L(v1,v2)={r1*v1+r2*v2: r1,r2 in R}
L.x1<-matrix(data=NA, nrow=length(r1x1.v1(x1.v1,r1)), ncol=length(r2x1.v2(x1.v2,r1)))
L.x2<-matrix(data=NA, nrow=length(r1x2.v1(x2.v1,r1)), ncol=length(r2x2.v2(x2.v2,r1)))
for(i in 1:length(r1x1.v1(x1.v1,r1))){
  for(j in 1:length(r2x1.v2(x1.v2,r1))){
    L.x1[i,j]=r1x1.v1(x1.v1,r1)[i]+r2x1.v2(x1.v2,r1)[j]
    L.x2[i,j]=r1x2.v1(x2.v1,r1)[i]+r2x2.v2(x2.v2,r1)[j]
  }
}

# Plot
plot(NA,
     ylim=c(min(min(r1x2.v1(x2.v1,r1)),min(r2x2.v2(x2.v2,r1)))-20,
            max(max(r1x2.v1(x2.v1,r1)),max(r2x2.v2(x2.v2,r1)))+20),
     xlim=c(min(min(r1x1.v1(x1.v1,r1)),min(r2x1.v2(x1.v2,r1)))-10,
            max(max(r1x1.v1(x1.v1,r1)),max(r2x1.v2(x1.v2,r1)))+10),
     main=expression(paste("V=L[",v[1],",",v[2],"]",""%==%"","{",r[1],v[1],"+",r[2],v[2],": ",r[1],",",r[2]%in%R^1,"}, ",V%subset%R^2)),
     ylab=expression(paste(x[2])),
     xlab=expression(paste(x[1])))
abline(v=0,lty=3)
abline(h=0,lty=3)
points(x1.v1,x2.v1,pch=20)
legend("bottomleft", 
       legend=c(as.expression(bquote(v[1]==~(.(x1.v1)~","~.(x2.v1)))),
                NA,
                NA,
                NA,
                NA,
                NA,
                NA),
       pch=c(20,NA,NA,NA,NA,NA,NA),
       pt.cex=c(1,NA,NA,NA,NA,NA,NA),
       col=c(1,2,1,NA,2,NA,scales::alpha('gray',.4)),
       bty="n")
points(x1.v2,x2.v2,pch=20,col=2)
legend("bottomleft", 
       legend=c(NA,
                as.expression(bquote(v[2]==~(.(x1.v2)~","~.(x2.v2)))),
                NA,
                NA,
                NA,
                NA,
                NA),
       pch=c(NA,20,NA,NA,NA,NA,NA),
       pt.cex=c(NA,1,NA,NA,NA,NA,NA),
       col=c(1,2,1,NA,2,NA,scales::alpha('gray',.4)),
       bty="n")
lines(r1x1.v1(x1.v1,r1),r1x2.v1(x2.v1,r1),
      type="l")
legend("bottomleft", 
       legend=c(NA,
                NA,
                as.expression(bquote(r[1]~v[1]~", "~r[1]%in%~"["~.(min(r1))~","~.(max(r1))~"]")),
                NA,
                NA,
                NA,
                NA),
       lty=c(NA,NA,1,NA,NA,NA,NA),
       col=c(1,2,1,NA,2,NA,scales::alpha('gray',.4)),
       bty="n")
legend("bottomleft", 
       legend=c(NA,
                NA,
                NA,
                as.expression(bquote("      "~""%==%""~"L["~v[1]~"]"~""%==%""~"{"~r[1]~v[1]~": "~r[1]%in%R^1~"}")),
                NA,
                NA,
                NA),
       bty="n")
lines(r2x1.v2(x1.v2,r1),r2x2.v2(x2.v2,r1),
      col=2)
legend("bottomleft",
       legend=c(NA,
                NA,
                NA,
                NA,
                as.expression(bquote(r[2]~v[2]~", "~r[2]%in%~"["~.(min(r2))~","~.(max(r2))~"]")),
                NA,
                NA),
       lty=c(NA,NA,NA,NA,1,NA,NA),
       col=c(1,2,1,NA,2,NA,scales::alpha('gray',.4)),
       bty="n")
legend("bottomleft",
       legend=c(NA,
                NA,
                NA,
                NA,
                NA,
                as.expression(bquote("      "~""%==%""~"L["~v[2]~"]"~""%==%""~"{"~r[2]~v[2]~": "~r[2]%in%R^1~"}")),
                NA),
       bty="n")
lines(L.x1,L.x2,col=scales::alpha('gray',.4))
legend("bottomleft",
       legend=c(NA,
                NA,
                NA,
                NA,
                NA,
                NA,
                ifelse(det(matrix(c(v1,v2),ncol=length(v1),byrow=FALSE))==0,
                       as.expression(bquote("V=L["~v[1]~","~v[2]~"]=L["~v[2]~"]")),
                       as.expression(bquote("V=L["~v[1]~","~v[2]~"]")))),
       pch=c(NA,NA,NA,NA,NA,NA,15),
       pt.cex=c(NA,NA,NA,NA,NA,NA,2),
       col=c(1,2,1,NA,2,NA,scales::alpha('gray',.4)),
       bty="n")
legend("bottomright",
       legend=c(as.expression(bquote("A=("~v[1]~","~v[2]~"): matrix A with columns "~v[1]~","~v[2])),
                as.expression(bquote("det(A)="~.(det(matrix(c(v1,v2),ncol=length(v1),byrow=FALSE))))),
                ifelse(det(matrix(c(v1,v2),ncol=length(v1),byrow=FALSE))==0,
                       "A is singular; non-invertible; columns",
                       "A is nonsingular; invertible; columns"),
                ifelse(det(matrix(c(v1,v2),ncol=length(v1),byrow=FALSE))==0,
                       "   of A are linearly dependent",
                       "   of A are linearly independent")),
       bty="n")


