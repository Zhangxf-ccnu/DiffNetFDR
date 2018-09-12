source("generate_data.R")
source("evaluation_metric.R")

library("DiffNetFDR")

n = 200
p = c(100,200,400)
model = "SF"
tau = 0.3

alpha = seq(0.05,0.5, 0.05)

rtimes = 100


power.pcor = matrix(NA, length(alpha), length(p))
power.pmat = matrix(NA, length(alpha), length(p))
FDP.pcor =  matrix(NA, length(alpha), length(p))
FDP.pmat =  matrix(NA, length(alpha), length(p))

for (i in 1:length(p)){
  cat("i=",i)
  power1 = matrix(NA, length(alpha), rtimes)
  power2 = matrix(NA, length(alpha), rtimes)
  FDP1 =  matrix(NA, length(alpha), rtimes)
  FDP2 =  matrix(NA, length(alpha), rtimes)

  for (r in 1:rtimes){

    cat("r=",r)
    dat = generate_data(p=p[i], n=n, model=model)
    out1 = DiffNet.FDR(dat$X, dat$group, alpha = alpha, test.type = "pcor")
    out2 = DiffNet.FDR(dat$X, dat$group, alpha = alpha, test.type = "pmat")

    for (j in 1:length(alpha)){
      power1[j,r] = evaluation_metric(dat$rho[[1]]-dat$rho[[2]], out1[[j]]$Diff.edge)$power
      FDP1[j,r] = evaluation_metric(dat$rho[[1]]-dat$rho[[2]], out1[[j]]$Diff.edge)$FDP

      power2[j,r] = evaluation_metric(dat$Omega[[1]]-dat$Omega[[2]], out2[[j]]$Diff.edge)$power
      FDP2[j,r] = evaluation_metric(dat$Omega[[1]]-dat$Omega[[2]], out2[[j]]$Diff.edge)$FDP

    }

  }


  power.pcor[,i] = rowMeans(power1)
  FDP.pcor[,i] = rowMeans(FDP1)

  power.pmat[,i] = rowMeans(power2)
  FDP.pmat[,i] = rowMeans(FDP2)

}


save.image("demo_simulation_SF_n200.rda")


load("demo_simulation_SF_n200.rda")

#par(mfrow = c(2, 3), mai=c(2,0.5,0.5,0.5), oma=c(2,2,2,2), mar=c(0.5,1,0.5,1))
par(mfrow = c(2, 3))
plot(alpha, FDP.pcor[,1], col = "blue", lwd=2, type = "o",
     xlim = c(-0.005, 0.505), ylim = c(-0.005, 0.505),
     xlab =  "alpha value",  ylab = "fdr", cex.lab=1.5, pch=19)
lines(alpha,alpha, col = "red",lwd=2,  type = "l")

plot(alpha, FDP.pcor[,2], col = "blue", lwd=2, type = "o",
     xlim = c(-0.005, 0.505), ylim = c(-0.005, 0.505),
     xlab =  "alpha value",  ylab = "fdr", cex.lab=1.5, pch=19)
lines(alpha,alpha, col = "red",lwd=2,  type = "l")


plot(alpha, FDP.pcor[,3], col = "blue", lwd=2, type = "o",
     xlim = c(-0.005, 0.505), ylim = c(-0.005, 0.505),
     xlab =  "alpha value",  ylab = "fdr", cex.lab=1.5, pch=19)
lines(alpha,alpha, col = "red",lwd=2,  type = "l")

plot(alpha, power.pcor[,1], col = "blue", lwd=2, type = "o",
     xlim = c(-0.005, 0.505), ylim = c(-0.005, 0.505),
     xlab =  "alpha value",  ylab = "power", cex.lab=1.5, pch=19)

plot(alpha, power.pcor[,2], col = "blue", lwd=2, type = "o",
     xlim = c(-0.005, 0.505), ylim = c(-0.005, 0.505),
     xlab =  "alpha value",  ylab = "power", cex.lab=1.5, pch=19)

plot(alpha, power.pcor[,3], col = "blue", lwd=2, type = "o",
     xlim = c(-0.005, 0.505), ylim = c(-0.005, 0.505),
     xlab =  "alpha value",  ylab = "power", cex.lab=1.5, pch=19)















par(mfrow = c(2, 3))
plot(alpha, FDP.pmat[,1], col = "blue", lwd=2, type = "o",
     xlim = c(-0.005, 0.505), ylim = c(-0.005, 0.505),
     xlab =  "alpha value",  ylab = "fdr", cex.lab=1.5, pch=19)
lines(alpha,alpha, col = "red",lwd=2,  type = "l")

plot(alpha, FDP.pmat[,2], col = "blue", lwd=2, type = "o",
     xlim = c(-0.005, 0.505), ylim = c(-0.005, 0.505),
     xlab =  "alpha value",  ylab = "fdr", cex.lab=1.5, pch=19)
lines(alpha,alpha, col = "red",lwd=2,  type = "l")


plot(alpha, FDP.pmat[,3], col = "blue", lwd=2, type = "o",
     xlim = c(-0.005, 0.505), ylim = c(-0.005, 0.505),
     xlab =  "alpha value",  ylab = "fdr", cex.lab=1.5, pch=19)
lines(alpha,alpha, col = "red",lwd=2,  type = "l")

plot(alpha, power.pmat[,1], col = "blue", lwd=2, type = "o",
     xlim = c(-0.005, 0.505), ylim = c(-0.005, 0.505),
     xlab =  "alpha value",  ylab = "power", cex.lab=1.5, pch=19)

plot(alpha, power.pmat[,2], col = "blue", lwd=2, type = "o",
     xlim = c(-0.005, 0.505), ylim = c(-0.005, 0.505),
     xlab =  "alpha value",  ylab = "power", cex.lab=1.5, pch=19)

plot(alpha, power.pmat[,3], col = "blue", lwd=2, type = "o",
     xlim = c(-0.005, 0.505), ylim = c(-0.005, 0.505),
     xlab =  "alpha value",  ylab = "power", cex.lab=1.5, pch=19)


