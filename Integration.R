

integrateODE()


w <- function(t){
  q0 <- 1
  q1 <- 0.0315
  q2 <- -0.00062
  qt <- q0 + q1*t + q2*t^2
  w <- 50000
  wt <- qt*w
  return(wt)
}

t <- seq(0,55)
g <- 0.203726
r <- 0.035
s <- 0.1





top <- function(t){
  (s*w(0)+g*(w(t)-w(0)))*exp(-(r*t))
}

bot <- function(t){
  exp(-r*t)
}


integrate(w, 0, 50, subdivisions = 200)


num <- integrate(top, 0, 40)
den <- integrate(bot, 40, 55)
num
den

A <- 165490/2.877749
A

