N = 5

x = seq(0, 1, length.out = 100)

Bernstein <- function(x, n, N){
  
  return(choose(N,n) * x^n * (1-x)^(N-n))
  
}

pdf("C:/Advancement/presentation/section-4-figures/bernstein.pdf", width = 4, height = 4)
plot(x, Bernstein(x = x, n = 0, N = N), type = "l", lwd = 2,
     ylab = "",
      xlab = "")

for( n in seq(1,N)){
  lines(x, Bernstein(x=x, n=n, N=N), lwd = 2, col = rgb(n/N, green=0, blue=0) )
}
dev.off()