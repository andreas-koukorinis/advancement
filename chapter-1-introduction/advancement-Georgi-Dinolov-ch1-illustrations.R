sp.data = read.csv("C:/Advancement/chapter-1-introduction/SP-daily-data.csv", header = T)
sp.index2 = seq(2, length(sp.data[,"Close"]))
sp.index1 = seq(1, length(sp.data[,"Close"])-1)
sp.log.returns = log( sp.data[sp.index2,"Close"] / sp.data[sp.index1,"Close"])

sp.index2 = seq(1, length(sp.data[,"Close"]))
sp.log.returns = log( sp.data[sp.index2,"Close"] / sp.data[sp.index2,"Open"])

####

nflx.data = read.csv("C:/Advancement/chapter-1-introduction/NFLX-daily-data.csv") 
nflx.index1 = seq(2, length(nflx.data[,"Close"]))
nflx.index2 = seq(1, length(nflx.data[,"Close"])-1)
nflx.log.returns = log( nflx.data[nflx.index2,"Close"]) - log( nflx.data[nflx.index1,"Close"]) 

nflx.index2 = seq(1, length(nflx.data[,"Close"]))
nflx.log.returns = log( nflx.data[nflx.index2,"Close"]) - log( nflx.data[nflx.index2,"Open"]) 


####

twtr.data = read.csv("C:/Advancement/chapter-1-introduction/TWTR-daily-data.csv", header = T)
twtr.index2 = seq(2, length(twtr.data[,"Close"]))
twtr.index1 = seq(1, length(twtr.data[,"Close"])-1)
twtr.log.returns = log( twtr.data[twtr.index2,"Close"] / twtr.data[twtr.index1,"Close"])

####

f.data = read.csv("C:/Advancement/chapter-1-introduction/F-daily-data.csv", header = T)
f.index2 = seq(2, length(f.data[,"Close"]))
f.index1 = seq(1, length(f.data[,"Close"])-1)
f.log.returns = log( f.data[f.index2,"Close"] / f.data[f.index1,"Close"])

# boxplot((f.log.returns[nflx.log.returns > 0])^2, (f.log.returns[nflx.log.returns < 0])^2)
# boxplot(abs(f.log.returns[nflx.log.returns > 0]), abs(f.log.returns[nflx.log.returns < 0]))

####

appl.data = read.csv("C:/Advancement/chapter-1-introduction/APPL-daily-data.csv", header = T)
appl.index2 = seq(2, length(appl.data[,"Close"]))
appl.index1 = seq(1, length(appl.data[,"Close"])-1)
appl.log.returns = log( appl.data[appl.index2,"Close"] / appl.data[appl.index1,"Close"])

# boxplot((appl.log.returns[nflx.log.returns > 0])^2, (appl.log.returns[nflx.log.returns < 0])^2)
# boxplot(abs(appl.log.returns[nflx.log.returns > 0]), abs(appl.log.returns[nflx.log.returns < 0]))

####

hlf.data = read.csv("C:/Advancement/chapter-1-introduction/HLF-daily-data.csv", header = T)
hlf.index2 = seq(2, length(hlf.data[,"Close"]))
hlf.index1 = seq(1, length(hlf.data[,"Close"])-1)
hlf.log.returns = log( hlf.data[hlf.index2,"Close"] / hlf.data[hlf.index1,"Close"])

# boxplot((hlf.log.returns[nflx.log.returns > 0])^2, (hlf.log.returns[nflx.log.returns < 0])^2)
# boxplot(abs(hlf.log.returns[nflx.log.returns > 0]), abs(hlf.log.returns[nflx.log.returns < 0]))

pdf("C:/Advancement/chapter-1-introduction/SP500-daily-squared-log-returns.pdf", width = 4, height = 5)
plot( as.Date( sp.data[seq(1,length(sp.data[,"Close"])), "Date"] ), sp.log.returns^2, type = "l", 
      xlab = "", 
      ylab = "log.returns^2")
dev.off()

mm=mean( sp.log.returns )
vv = var(sp.log.returns)

pdf("C:/Advancement/chapter-1-introduction/SP500-daily-squared-log-returns-constant-vol.pdf", width = 4, height = 5)
plot( as.Date( sp.data[seq(1,length(sp.data[,"Close"])), "Date"] ),
      rnorm(n = length(as.Date( sp.data[seq(1,length(sp.data[,"Close"])), "Date"] )), mean = mm, sd = sqrt(vv) )^2, 
      type = "l",
      xlab = "",
      ylab = "log.returns^2",
      ylim = c(min(sp.log.returns^2), max(sp.log.returns^2)))
dev.off()

pdf("C:/Advancement/chapter-1-introduction/Netflix-daily-squared-log-returns.pdf", width = 4, height = 5)
plot(as.Date( nflx.data[seq(1,length(nflx.data[,"Close"])), "Date"] ), nflx.log.returns^2, type = "l", 
     xlab = "", 
     ylab = "log.returns^2")
dev.off()

mm=mean( nflx.log.returns )
vv = var(nflx.log.returns)

pdf("C:/Advancement/chapter-1-introduction/Netflix-daily-squared-log-returns-constant-vol.pdf", width = 4, height = 5)
plot( as.Date( nflx.data[seq(1,length(nflx.data[,"Close"])), "Date"] ),
      rnorm(n = length(as.Date( nflx.data[seq(1,length(nflx.data[,"Close"])), "Date"] )), mean = mm, sd = sqrt(vv) )^2, 
      type = "l",
      xlab = "",
      ylab = "log.returns^2",
      ylim = c(min(nflx.log.returns^2), max(nflx.log.returns^2)))
dev.off()