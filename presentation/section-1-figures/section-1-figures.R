nflx.data = read.csv("C:/Advancement/chapter-1-introduction/NFLX-daily-data.csv") 
nflx.index1 = seq(2, length(nflx.data[,"Close"]))
nflx.index2 = seq(1, length(nflx.data[,"Close"])-1)
nflx.log.returns = log( nflx.data[nflx.index2,"Close"]) - log( nflx.data[nflx.index1,"Close"]) 

nflx.index2 = seq(1, length(nflx.data[,"Close"]))
nflx.log.returns = log( nflx.data[nflx.index2,"Close"]) - log( nflx.data[nflx.index2,"Open"]) 

pdf("C:/Advancement/presentation/section-1-figures/Netflix-daily-squared-log-returns.pdf", width = 4, height = 4)
plot(as.Date( nflx.data[seq(1,length(nflx.data[,"Close"])), "Date"] ), nflx.log.returns^2, type = "l", 
     xlab = "", 
     ylab = "")
dev.off()

mm=mean( nflx.log.returns )
vv = var(nflx.log.returns)

pdf("C:/Advancement/presentation/section-1-figures/Netflix-daily-squared-log-returns-constant-vol.pdf", width = 4, height = 4)
plot( as.Date( nflx.data[seq(1,length(nflx.data[,"Close"])), "Date"] ),
      rnorm(n = length(as.Date( nflx.data[seq(1,length(nflx.data[,"Close"])), "Date"] )), mean = mm, sd = sqrt(vv) )^2, 
      type = "l",
      xlab = "",
      ylab = "",
      ylim = c(min(nflx.log.returns^2), max(nflx.log.returns^2)))
dev.off()
