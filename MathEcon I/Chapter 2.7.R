# FUNCTIONS
y<-function(x){
  100-(x-2)^2
}

tangent.line<-function(x1,x0){
  -2*(x0-2)*(x1-x0)+y(x0)
}

# VALUES
x<-seq(-10,10,0.1)
x0<--5
x1<-0

# PLOT
plot(x,y(x), 
     type="l",
     ylim=c(-40,120),
     main="Approximating y(x) by differentials")
# Starting point
points(x0,y(x0), 
       col=1, pch=20)
segments(x0, -50, x0, y(x0), 
         lty=3)
segments(-15, y(x0), x0, y(x0), 
         lty=3)
text(x0+3,y(x0),
     as.expression(bquote("(x0, y(x0))"==(.(x0,)~","~.(y(x0))))))
# New point (which arise due to some change)
points(x1,y(x1), 
       col=4, pch=20)
segments(x1, -50, x1, y(x1), 
         lty=3, col=4)
segments(-15, y(x1), x1, y(x1), 
         lty=3, col=4)
text(x1+3,y(x1),
     as.expression(bquote("(x1, y(x1))"==(.(x1,)~","~.(y(x1))))),
     col=4)
# Tangent line to starting point (x0, y(x0))
lines(seq(-10,0,0.1),tangent.line(seq(-10,0,0.1),x0),
      col=2)
# Main idea of approximating by differentials
# Use the tangent line to (x0, y(x0)) to predict the value of y() after change, i.e., (x1, y(x1))
points(x1,tangent.line(x1,x0),
       col=2, pch=20)
segments(x1, y(x1), x1, tangent.line(x1,x0), 
         lty=3, col=2)
segments(-15, tangent.line(x1,x0), x1, tangent.line(x1,x0), 
         lty=3, col=2)
text(x1+5,tangent.line(x1,x0),
     as.expression(bquote("(x1, y'(x0)(x1-x0)+y(x0))"==(.(x1,)~","~.(tangent.line(x1,x0))))),
     col=2)


# LOOP TO SHOW IMPROVEMENT IN APPROXIMATION BY DIFFERENTIALS
# VALUES
x<-seq(-10,10,0.1)
x0<--5
x1<-sort(seq(x0,0,by=0.5),decreasing=TRUE)

# PLOT
for(i in 1:length(x1)){
  plot(x,y(x), 
       type="l",
       ylim=c(-40,120),
       main="Approximating y(x) by differentials")
  # Starting point
  points(x0,y(x0), 
         col=1, pch=20)
  segments(x0, -50, x0, y(x0), 
           lty=3)
  segments(-15, y(x0), x0, y(x0), 
           lty=3)
  text(x0+3,y(x0),as.expression(bquote("(x0, y(x0))"==(.(x0,)~","~.(y(x0))))))
  # New point (which arise due to some change)
  points(x1[i],y(x1[i]), 
         col=4, pch=20)
  segments(x1[i], -50, x1[i], y(x1[i]), 
           lty=3, col=4)
  segments(-15, y(x1[i]), x1[i], y(x1[i]), 
           lty=3, col=4)
  text(x1[i]+3.5,y(x1[i]),
       as.expression(bquote("(x1, y(x1))"==(.(x1[i],)~","~.(y(x1[i]))))),
       col=4)
  # Tangent line to starting point (x0, y(x0))
  lines(seq(-10,0,0.1),tangent.line(seq(-10,0,0.1),x0),
        col=2)
  # Main idea of approximating by differentials
  # Use the tangent line to (x0, y(x0)) to predict the value of y() after change, i.e., (x1, y(x1))
  points(x1[i],tangent.line(x1[i],x0),
         col=2, pch=20)
  segments(x1[i], y(x1[i]), x1[i], tangent.line(x1[i],x0), 
           lty=3, col=2)
  segments(-15, tangent.line(x1[i],x0), x1[i], tangent.line(x1[i],x0), 
           lty=3, col=2)
  text(x1[i]+5,tangent.line(x1[i],x0),
       as.expression(bquote("(x1, y'(x0)(x1-x0)+y(x0))"==(.(x1[i],)~","~.(tangent.line(x1[i],x0))))),
       col=2)
  
  # Prompt
  readline(prompt="Press [enter] to continue")
}





