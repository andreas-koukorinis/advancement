library(grDevices)
library(plotrix)

simulation.results <- function(mu.x, sigma.x, NNN){
  
  es = rnorm(n = NNN+1, mean = 0, sd=sigma.x)
  
  xs = rep(NA, NNN+1)
  
  xs[1] = 0
  
  dt = 1/NNN
  
  for(i in seq(2,NNN+1)){
    
    xs[i] = xs[i-1] + mu.x*dt + sqrt(dt) * es[i]
    
  }
  
  out = NULL
  
  out$xmax = max(xs)
  out$xmin = min(xs)
  out$xs = xs
  
  return(out)
}

generate.data <- function(R, mu.x, sigma.x, NNN){
  
  data = vector(mode="list", length=R)
  for(r in seq(1,R)){
    
    simulation = simulation.results(mu.x = mu.x, sigma.x=sigma.x, NNN=NNN)
    
    input = NULL
    input$xmax = simulation$xmax
    input$xmin = simulation$xmin
    input$ymax = simulation$ymax
    input$ymin = simulation$ymin
    input$xs = simulation$xs
    input$ys = simulation$ys
    
    data[[r]] = input
  }
  
  return(data)
  
}

mu = 0
sigma = 1
data = generate.data(R = 1, mu.x = mu, sigma.x = sigma, 1000)
simulation = data[[1]]

y.t.minus.1 = 0
a.t = simulation$xmin
b.t = simulation$xmax
y.t = simulation$xs[length(simulation$xs)]
t = 1

y = seq(-1*max( abs(c(3*a.t, 3*b.t)) ), max( abs(c(3*a.t, 3*b.t)) ), length.out = 100)

zeta <- function(y){  
  out = 2*a.t - y
  return(out)
}

xi <- function(y){
  out = 2*b.t - y
  return(out)
}

d.1.n <- function(n){
  out = y.t.minus.1 + 2*n*(b.t-a.t)
  return(out)
}

d.2.n <- function(n){
  out = -y.t.minus.1 + 2*a.t - 2*n*(b.t-a.t)
  return(out)
}



pdf("C:/Advancement/presentation/section-2-figures/ic.pdf", 4, 4)
plot(y, g.0, ylab = "", xlab = "", type = "n", col = "red", lwd = 2, ylim = c(1.1*min(c(g.0, r.0, g.1)), 1.1*max(c(g.0,r.0,g.1))),
     xaxt = "n", yaxt = "n")

abline(v=c(a.t, b.t), lwd = 2, lty = "dashed")
abline(h=0, lwd = 1)

segments(x0 = 0, y0 = 0, x1 = 0, y1 = max(g.0)*1.1, lwd = 2)
points(x = 0, y = max(g.0)*1.1, lwd = 5, pch = 18, col = "black")
mtext(text = c(expression(a[t]), expression(b[t])), side = 1, at = c(a.t, b.t))
dev.off()




pdf("C:/Advancement/presentation/section-2-figures/g0.pdf", 4, 4)
g.0 = dnorm(x=y, mean=y.t.minus.1, sd=sigma)
plot(y, g.0, ylab = "", xlab = "", type = "l", col = "red", lwd = 2, ylim = c(1.1*min(c(g.0, r.0, g.1)), 1.1*max(c(g.0,r.0,g.1))),
     xaxt = "n", yaxt = "n")
abline(v=c(a.t, b.t), lwd = 2, lty = "dashed")
abline(h=0, lwd = 1)

segments(x0 = 0, y0 = 0, x1 = 0, y1 = max(g.0)*1.1, lwd = 2)
points(x = 0, y = max(g.0)*1.1, lwd = 5, pch = 18, col = "black")
mtext(text = c(expression(a[t]), expression(b[t])), side = 1, at = c(a.t, b.t))
dev.off()




pdf("C:/Advancement/presentation/section-2-figures/g0r0.pdf", 4, 4)
g.0 = dnorm(x=y, mean=y.t.minus.1, sd=sigma)
r.0 = -1*dnorm(x=y, mean=zeta(y.t.minus.1), sd=sigma)

plot(y, g.0, ylab = "", xlab = "", type = "l", col = "red", lwd = 2, ylim = c(1.1*min(c(g.0, r.0, g.1)), 1.1*max(c(g.0,r.0,g.1))),
     xaxt = "n", yaxt = "n")
abline(v=c(a.t, b.t), lwd = 2, lty = "dashed")
abline(h=0, lwd = 1)
lines(y, r.0, lwd = 2, col = "green")
segments(x0 = c(0, zeta(y.t.minus.1)), y0 = c(0,0), x1 = c(0,zeta(y.t.minus.1)), y1 = c( max(g.0)*1.1, -1*max(g.0)*1.1 ), lwd = 2)
points(x = c(0,zeta(y.t.minus.1)), y = c( max(g.0)*1.1, -1*max(g.0)*1.1 ), lwd = 5, pch = 18, col = "black")
mtext(text = c(expression(a[t]), expression(b[t])), side = 1, at = c(a.t, b.t))
dev.off()




pdf("C:/Advancement/presentation/section-2-figures/g1.pdf", 4, 4)
g.0 = dnorm(x=y, mean=y.t.minus.1, sd=sigma)
r.0 = -1*dnorm(x=y, mean=zeta(y.t.minus.1), sd=sigma)
g.1 = g.0 + r.0

plot(y, g.1, ylab = "", xlab = "", type = "l", col = "red", lwd = 2, ylim = c(1.1*min(c(g.0, r.0, g.1)), 1.1*max(c(g.0,r.0,g.1))),
     xaxt = "n", yaxt = "n")
abline(v=c(a.t, b.t), lwd = 2, lty = "dashed")
abline(h=0, lwd = 1)

segments(x0 = c(0, zeta(y.t.minus.1)), y0 = c(0,0), x1 = c(0,zeta(y.t.minus.1)), y1 = c( max(g.0)*1.1, -1*max(g.0)*1.1 ), lwd = 2)
points(x = c(0,zeta(y.t.minus.1)), y = c( max(g.0)*1.1, -1*max(g.0)*1.1 ), lwd = 5, pch = 18, col = "black")

mtext(text = c(expression(a[t]), expression(b[t])), side = 1, at = c(a.t, b.t))
dev.off()




pdf("C:/Advancement/presentation/section-2-figures/g1r1.pdf", 4, 4)

g.1 = dnorm(x=y, mean=y.t.minus.1, sd=sigma) + -1*dnorm(x=y, mean=zeta(y.t.minus.1), sd = sigma)
r.1 = -1*dnorm(x=y, mean=xi(y.t.minus.1), sd=sigma) + dnorm(x=y, mean=xi( zeta(y.t.minus.1) ), sd = sigma)
g.2 = g.1 + r.1

plot(y, g.1, ylim = c(1.1*min(c(g.0, r.0, g.1)), 1.1*max(c(g.0,r.0,g.1))), 
     ylab = "",
     xlab = "",
     type = "l",
     col = "red",
     lwd = 2,
     xaxt='n',
     yaxt='n')

abline(v=c(a.t, b.t), lwd = 2, lty = "dashed")
abline(h=0, lwd = 1)

lines(y,r.1, lwd = 2, col = "green")
#lines(y,g.2, lwd = 2, col = "black")
mtext(text = c(expression(a[t]), expression(b[t])), side = 1, at = c(a.t, b.t))

segments(x0 = c(y.t.minus.1, zeta(y.t.minus.1), xi(y.t.minus.1), xi( zeta(y.t.minus.1) )), 
         y0 = c(0,0,0,0), 
         x1 = c(y.t.minus.1, zeta(y.t.minus.1), xi(y.t.minus.1), xi( zeta(y.t.minus.1) )), 
         y1 = c( max(g.0)*1.1, -1*max(g.0)*1.1, -1*max(g.0)*1.1, max(g.0)*1.1 ), 
         lwd = 2)

points(x = c(y.t.minus.1, zeta(y.t.minus.1), xi(y.t.minus.1), xi( zeta(y.t.minus.1) )), 
       y = c( max(g.0)*1.1, -1*max(g.0)*1.1, -1*max(g.0)*1.1, max(g.0)*1.1 ), 
       lwd = 2,
       pch = 18,
        col = "black")
dev.off()




pdf("C:/Advancement/presentation/section-2-figures/g2.pdf", 4, 4)

g.1 = dnorm(x=y, mean=y.t.minus.1, sd=sigma) + -1*dnorm(x=y, mean=zeta(y.t.minus.1), sd = sigma)
r.1 = -1*dnorm(x=y, mean=xi(y.t.minus.1), sd=sigma) + dnorm(x=y, mean=xi( zeta(y.t.minus.1) ), sd = sigma)
g.2 = g.1 + r.1

plot(y, g.2, ylim = c(1.1*min(c(g.0, r.0, g.1)), 1.1*max(c(g.0,r.0,g.1))), 
     ylab = "",
     xlab = "",
     type = "l",
     col = "red",
     lwd = 2,
     xaxt='n',
     yaxt='n')

abline(v=c(a.t, b.t), lwd = 2, lty = "dashed")
abline(h=0, lwd = 1)

#lines(y,r.1, lwd = 2, col = "green")
#lines(y,g.2, lwd = 2, col = "red")
mtext(text = c(expression(a[t]), expression(b[t])), side = 1, at = c(a.t, b.t))

segments(x0 = c(y.t.minus.1, zeta(y.t.minus.1), xi(y.t.minus.1), xi( zeta(y.t.minus.1) )), 
         y0 = c(0,0,0,0), 
         x1 = c(y.t.minus.1, zeta(y.t.minus.1), xi(y.t.minus.1), xi( zeta(y.t.minus.1) )), 
         y1 = c( max(g.0)*1.1, -1*max(g.0)*1.1, -1*max(g.0)*1.1, max(g.0)*1.1 ), 
         lwd = 2)

points(x = c(y.t.minus.1, zeta(y.t.minus.1), xi(y.t.minus.1), xi( zeta(y.t.minus.1) )), 
       y = c( max(g.0)*1.1, -1*max(g.0)*1.1, -1*max(g.0)*1.1, max(g.0)*1.1 ), 
       lwd = 2,
       pch = 18,
       col = "black")
dev.off()




pdf("C:/Advancement/presentation/section-2-figures/g3.pdf", 4,4)

g.3 = dnorm(x=y, mean = y.t.minus.1, sd=sigma) + 
  -1*dnorm(x=y, mean = xi(y.t.minus.1), sd=sigma) + 
  -1*dnorm(x=y, mean = zeta(y.t.minus.1), sd=sigma) +
  dnorm(x=y,mean=xi(zeta(y.t.minus.1)),sd=sigma)+
  dnorm(x=y,mean=zeta(xi(y.t.minus.1)),sd=sigma)+
  -1*dnorm(x=y,mean=zeta(xi(zeta(y.t.minus.1))),sd=sigma)+
  -1*dnorm(x=y,mean=xi(zeta(xi(y.t.minus.1))),sd=sigma)

plot(y, g.3, ylim = c(1.1*min(c(g.0, r.0, g.1)), 1.1*max(c(g.0,r.0,g.1))),
     type = "l", 
     lwd = 2, 
     col = "red", 
     ylab = "", 
     xlab = "", 
     xaxt = 'n', 
     yaxt = 'n')
abline(v=c(a.t, b.t), lwd = 2, lty = "dashed")
abline(h=0, lwd = 1)
mtext(text = c(expression(a[t]), expression(b[t])), side = 1, at = c(a.t, b.t))

segments(x0 = c(y.t.minus.1, xi(y.t.minus.1), zeta(y.t.minus.1), xi( zeta(y.t.minus.1) ), zeta(xi(y.t.minus.1)), zeta(xi(zeta(y.t.minus.1))), xi(zeta(xi(y.t.minus.1)))), 
         y0 = rep(0,7), 
         x1 = c(y.t.minus.1, xi(y.t.minus.1), zeta(y.t.minus.1), xi( zeta(y.t.minus.1) ), zeta(xi(y.t.minus.1)), zeta(xi(zeta(y.t.minus.1))), xi(zeta(xi(y.t.minus.1)))), 
         y1 = c( max(g.0)*1.1, -1*max(g.0)*1.1, -1*max(g.0)*1.1, max(g.0)*1.1, max(g.0)*1.1, -1*max(g.0)*1.1, -1*max(g.0)*1.1), 
         lwd = 2)

points(x = c(y.t.minus.1, xi(y.t.minus.1), zeta(y.t.minus.1), xi( zeta(y.t.minus.1) ), zeta(xi(y.t.minus.1)), zeta(xi(zeta(y.t.minus.1))), xi(zeta(xi(y.t.minus.1)))), 
       y = c( max(g.0)*1.1, -1*max(g.0)*1.1, -1*max(g.0)*1.1, max(g.0)*1.1, max(g.0)*1.1, -1*max(g.0)*1.1, -1*max(g.0)*1.1), 
       lwd = 2,
       pch = 18,
       col = "black")

dev.off()
