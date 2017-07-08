source("C:/Classes/analytic fokker-plank/trig expansion approach/univariate-model-functions.R")

NNy = 2^(seq(2,6))

kk = seq(4,8, length.out = 2)

R = 10
mu = 1
sigma = 1

data = generate.data(R = 10, mu = 1, sigma = 1, NNN = 23400)

true.derivatives.table = array(dim = c(R, 1))

for(r in seq(1,R)){
  simulation = data[[r]] 
  
  a.t = simulation$ymin
  b.t = simulation$ymax
  y = simulation$y
  for(Ny in NNy){
    
    true.derivatives.table[ r, 1] = log.second.order.analytic.derivative.trig.expansion(t=1,mu = mu,sigma = sigma, a.t = a.t, b.t = b.t, y = y, y.ic = 0, Ny = 200)
  
  }
}


numerical.derivatives.tables = array(dim = c(R, length(NNy)+3, length(kk)),
               dimnames = list(NULL,
                               c( "high", "low", "close", paste("$n=", NNy, "$", sep="") ),
                               paste("dx.factor=1/2^", kk, sep="")))

rel.error.numerical.derivatives.tables = array(dim = c(R, length(NNy)+3, length(kk)),
                                     dimnames = list(NULL,
                                                     c( "high", "low", "close", paste("$n=", NNy, "$", sep="") ),
                                                     paste("dx.factor=1/2^", kk, sep="")))


for(r in seq(1,R)){
  simulation = data[[r]] 
  
  a.t = simulation$ymin
  b.t = simulation$ymax
  y = simulation$y
    
  for(Ny in NNy){
    for(k in kk){
      
      numerical.derivatives.tables[ r, seq(1,3), which(kk==k)] = round( c(b.t, a.t, y[length(y)]) , 4)
      #numerical.derivatives.tables[ r, 3+which(NNy==Ny), which(kk==k) ] = round( log.second.order.numerical.derivative.trig.expansion(t=1,mu = mu,sigma = sigma, a.t = a.t, b.t = b.t, y = y, y.ic = 0, Ny = Ny, m = k), 4)
      numerical.derivatives.tables[ r, 3+which(NNy==Ny), which(kk==k) ] = log.second.order.numerical.derivative.trig.expansion(t=1,mu = mu,sigma = sigma, a.t = a.t, b.t = b.t, y = y, y.ic = 0, Ny = Ny, m = k)
      rel.error.numerical.derivatives.tables[r, 3+which(NNy==Ny), which(kk==k)] = (numerical.derivatives.tables[ r, 3+which(NNy==Ny), which(kk==k) ] - true.derivatives.table[r,1])/true.derivatives.table[r,1]
    }
  }
}

write.csv(numerical.derivatives.tables[,,1], file = "C:/Advancement/presentation/section-2-figures/table-m-4.csv")
write.csv(numerical.derivatives.tables[,,2], file = "C:/Advancement/presentation/section-2-figures/table-m-8.csv")

write.csv(rel.error.numerical.derivatives.tables[,,1], file = "C:/Advancement/presentation/section-2-figures/rel-error-table-m-4.csv")
write.csv(rel.error.numerical.derivatives.tables[,,2], file = "C:/Advancement/presentation/section-2-figures/rel-error-table-m-8.csv")






analytic.derivatives.table = array(dim = c(R, 3+length(NNy), 1),
               dimnames = list(NULL,
                               c( "high", "low", "close", paste("$n=", NNy, "$", sep="")),
                               c("analytic") ) )

rel.error.analytic.derivatives.table = array(dim = c(R, length(NNy)+3, 1),
                                               dimnames = list(NULL,
                                                               c( "high", "low", "close", paste("$n=", NNy, "$", sep="") ),
                                                               NULL))

for(r in seq(1,R)){
  simulation = data[[r]] 
  
  a.t = simulation$ymin
  b.t = simulation$ymax
  y = simulation$y
  for(Ny in NNy){
      
    analytic.derivatives.table[ r, seq(1,3), 1] = round( c(b.t, a.t, y), 4 )
    #analytic.derivatives.table[ r, 3+which(NNy==Ny), 1 ] = round( log.second.order.analytic.derivative.trig.expansion(t=1,mu = mu,sigma = sigma, a.t = a.t, b.t = b.t, y = y, y.ic = 0, Ny = Ny), 4)
    analytic.derivatives.table[ r, 3+which(NNy==Ny), 1 ] = log.second.order.analytic.derivative.trig.expansion(t=1,mu = mu,sigma = sigma, a.t = a.t, b.t = b.t, y = y, y.ic = 0, Ny = Ny)
    rel.error.analytic.derivatives.table[ r, 3+which(NNy==Ny), 1 ] = (analytic.derivatives.table[ r, 3+which(NNy==Ny), 1 ] - true.derivatives.table[r,1])/true.derivatives.table[r,1]
  }
}

write.csv(analytic.derivatives.table[,,1], file = "C:/Advancement/presentation/section-2-figures/table-analytic.csv")
write.csv(rel.error.analytic.derivatives.table[,,1], file = "C:/Advancement/presentation/section-2-figures/rel-error-table-analytic.csv")
