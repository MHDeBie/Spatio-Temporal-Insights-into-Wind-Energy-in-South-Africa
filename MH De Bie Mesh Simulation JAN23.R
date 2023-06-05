### MESH SIMULATION
rm(list = ls(all.names = TRUE)) 
options(max.print = .Machine$integer.max)
gc() 


#install.packages("installr")
#install.packages("vctrs")
#install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)

library(INLA)


#THIS IS JUST WHERE I TEST IF INLA IS WORKING
# n = 100; a = 1; b = 1; tau = 100
# z = rnorm(n)
# eta = a + b*z
# 
# scale = exp(rnorm(n))
# prec = scale*tau
# y = rnorm(n, mean = eta, sd = 1/sqrt(prec))
# 
# 
# data = list(y=y, z=z)
# formula = y ~ 1+z
# result = inla(formula, family = "gaussian", data = data)
# 
# summary(result)

library(leaflet)
library(ggplot2)
library(sf)
library("rnaturalearth")
library("rnaturalearthdata")
#library(fields)
library(readr)

###INLA UNIT SQUARE SIMULATION
#CREATE A FUNCTION FOR THE MODE
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}



data("SPDEtoy")
coords <- as.matrix(SPDEtoy[,1:2])
pl.dom <- cbind(c(0,1,1,0.7,0), c(0,0,0.7,1,1))

#SIMPLE SQUARE MESH
#TRUE PARM VALUES
beta0 <- 10
sigma2e <- 0.3
sigma2x <- 5
kappa <- 7
nu <- 1


nmesh=10
set.seed(123)
maxedge1 <- runif(nmesh, min = 0.020, max = 0.1)
maxedge1=as.vector(maxedge1)
maxedge1=sort(maxedge1)
maxedge1
set.seed(123)
maxedge2 <- runif(nmesh, min = 0.025, max = 0.2)
maxedge2=as.vector(maxedge2)
maxedge2=sort(maxedge2)
maxedge2


mesh1 <- inla.mesh.2d(coords,pl.dom, max.edge=c(maxedge1[nmesh], maxedge2[nmesh]), cutoff = 0.1)
plot(mesh1)
points(coords, col = "red", pch = 16)
mesh2 <- inla.mesh.2d(coords,pl.dom, max.edge=c(maxedge1[nmesh-1], maxedge2[nmesh-1]), cutoff = 0.1)
plot(mesh2)
points(coords, col = "red", pch = 16)
mesh3 <- inla.mesh.2d(coords,pl.dom, max.edge=c(maxedge1[nmesh-2], maxedge2[nmesh-2]), cutoff = 0.08)
plot(mesh3)
points(coords, col = "red", pch = 16)
mesh4 <- inla.mesh.2d(coords,pl.dom, max.edge=c(maxedge1[nmesh-3], maxedge2[nmesh-3]), cutoff = 0.06)
plot(mesh4)
points(coords, col = "red", pch = 16)
mesh5 <- inla.mesh.2d(coords,pl.dom, max.edge=c(maxedge1[nmesh-4], maxedge2[nmesh-4]), cutoff = 0.04)
plot(mesh5)
points(coords, col = "red", pch = 16)
mesh6 <- inla.mesh.2d(coords,pl.dom, max.edge=c(maxedge1[nmesh-5], maxedge2[nmesh-5]), cutoff = 0.02)
plot(mesh6)
points(coords, col = "red", pch = 16)
mesh7 <- inla.mesh.2d(coords,pl.dom, max.edge=c(maxedge1[nmesh-6], maxedge2[nmesh-6]), cutoff = 0.02)
plot(mesh7)
points(coords, col = "red", pch = 16)
mesh8 <- inla.mesh.2d(coords,pl.dom, max.edge=c(maxedge1[nmesh-7], maxedge2[nmesh-7]), cutoff = 0.02)
plot(mesh8)
points(coords, col = "red", pch = 16)
mesh9 <- inla.mesh.2d(coords,pl.dom, max.edge=c(maxedge1[nmesh-8], maxedge2[nmesh-8]), cutoff = 0.02)
plot(mesh9)
points(coords, col = "red", pch = 16)
mesh10 <- inla.mesh.2d(coords,pl.dom, max.edge=c(maxedge1[nmesh-9], maxedge2[nmesh-9]), cutoff = 0.02)
plot(mesh10)
points(coords, col = "red", pch = 16)




y=SPDEtoy[,3]
lrf <- lres <- l.dat <- l.spde <- l.a <- list()
for (k in 1:nmesh) {
  l.a[[k]] <- inla.spde.make.A(get(paste('mesh', k, sep='')), loc=coords)
  l.spde[[k]] <- inla.spde2.matern(get(paste('mesh', k, sep='')), alpha=2)
  l.dat[[k]] <- list(y=SPDEtoy$y , i=1:ncol(l.a[[k]]),
                     m=rep(1, ncol(l.a[[k]])))
  lres[[k]] <- inla(y ~ 0 + m + f(i, model=l.spde[[k]]),
                    data=l.dat[[k]], control.predictor=list(A=l.a[[k]]), verbose = T)
  lrf[[k]] <- inla.spde2.result(lres[[k]], 'i', l.spde[[k]], do.transf=TRUE)
}

#NUMBER OF VERTICES
nvert=c(mesh1$n, mesh2$n, mesh3$n, mesh4$n, mesh5$n, mesh6$n, mesh7$n, mesh8$n,
        mesh9$n,mesh10$n)
nvert

#COMPUTING RUNNING TIME
CPU_time=round(sapply(lres, function(x) x$cpu[2]), 2)
NUM_mesh=c(1:nmesh)
CPU_time
cpu_plt=as.data.frame(cbind(NUM_mesh,CPU_time,nvert))



rcols <- rainbow(nmesh)
#par(mfrow=c(2,3), mar=c(2.5,2.5,1,.5), mgp=c(1.5,.5,0), las=1)

#BETA0
xrange <- range(sapply(lres, function(x) range(x$marginals.fix[[1]][,1])))
yrange <- range(sapply(lres, function(x) range(x$marginals.fix[[1]][,2])))
plot(lres[[1]]$marginals.fix[[1]], type='l', xlim=xrange, ylim=yrange,
     xlab=expression(beta[0]), ylab='Density')
for (k in 1:nmesh)
  lines(lres[[k]]$marginals.fix[[1]], col=rcols[k], lwd=2)
abline(v=beta0, lty=2, lwd=2, col=3)
legend('topright', c(paste('mesh', 1:nmesh, sep=''), 'True'),
       lty=c(rep(1,nmesh), 2, 3), lwd=rep(2, nmesh), col=c(rcols,3,3), bty='n')

res_beta0=matrix(nrow = nmesh, ncol = 1, NA)
for (k in 1:nmesh) {
  res_beta0[k]=as.numeric(abs(beta0-lres[[k]]$summary.fixed$mode))
}
res_beta0=as.data.frame(res_beta0)

plot(res_beta0[[1]], type='l', xlab= "Mesh Number", ylab="Deviation between estimate and actual",
     main= expression(beta[0]))
lines(res_beta0$V1, col="red", lwd=3)

#SIGMA2e
s2.marg <- lapply(lres, function(m)
  inla.tmarginal(function(x) 1/x, m$marginals.hy[[1]]))
xrange <- range(sapply(s2.marg, function(x) range(x[,1])))
yrange <- range(sapply(s2.marg, function(x) range(x[,2])))
plot.default(s2.marg[[1]], type='l', xlim=xrange, ylim=yrange,
             xlab=expression(sigma[e]^2), ylab='Density')
for (k in 1:nmesh)
  lines(s2.marg[[k]], col=rcols[k], lwd=2)
abline(v=sigma2e, lty=2, lwd=2, col=3)
legend('topright', c(paste('mesh', 1:nmesh, sep=''), 'True'),
       lty=c(rep(1,nmesh), 2, 3), lwd=rep(2, nmesh), col=c(rcols,3,3), bty='n')

res_sigmaE=matrix(nrow = nmesh, ncol = 1, NA)
for (k in 1:nmesh) {
  res_sigmaE[k]=as.numeric(abs(sigma2e-getmode(s2.marg[[k]])))
}
res_sigmaE=as.data.frame(res_sigmaE)

plot(res_sigmaE[[1]], type='l', xlab= "Mesh Number", ylab="Deviation between estimate and actual",
     main= expression(sigma[e]^2))
lines(res_sigmaE$V1, col="blue", lwd=3)

#SIGMA2x
xrange <- range(sapply(lrf, function(r) range(r$marginals.variance.nominal[[1]][,1])))
yrange <- range(sapply(lrf, function(r) range(r$marginals.variance.nominal[[1]][,2])))
plot(lrf[[1]]$marginals.variance.nominal[[1]], type='l',
     xlim=xrange, ylim=yrange, xlab=expression(sigma[x]^2), ylab='Density')
for (k in 1:nmesh)
  lines(lrf[[k]]$marginals.variance.nominal[[1]], col=rcols[k], lwd=2)
abline(v=sigma2x, lty=2, lwd=2, col=3)
legend('topright', c(paste('mesh', 1:nmesh, sep=''), 'True'),
       lty=c(rep(1,nmesh), 2, 3), lwd=rep(2, nmesh), col=c(rcols,3,3), bty='n')

res_sigmaX=matrix(nrow = nmesh, ncol = 1, NA)
for (k in 1:nmesh) {
  res_sigmaX[k]=as.numeric(abs(sigma2x-getmode(lrf[[k]]$marginals.variance.nominal[[1]])))
}
res_sigmaX=as.data.frame(res_sigmaX)
plot(res_sigmaX[[1]], type='l', xlab= "Mesh Number", ylab="Deviation between estimate and actual",
     main= expression(sigma[x]^2))
lines(res_sigmaX$V1, col="green", lwd=3)

#KAPPA
xrange <- range(sapply(lrf, function(r) range(r$marginals.kappa[[1]][,1])))
yrange <- range(sapply(lrf, function(r) range(r$marginals.kappa[[1]][,2])))
plot(lrf[[1]]$marginals.kappa[[1]], type='l',
     xlim=xrange, ylim=yrange, xlab=expression(kappa), ylab='Density')
for (k in 1:nmesh)
  lines(lrf[[k]]$marginals.kappa[[1]], col=rcols[k], lwd=2)
abline(v=kappa, lty=2, lwd=2, col=3)
legend('topright', c(paste('mesh', 1:nmesh, sep=''), 'True'),
       lty=c(rep(1,nmesh), 2, 3), lwd=rep(2, nmesh), col=c(rcols,3,3), bty='n')

res_kappa=matrix(nrow = nmesh, ncol = 1, NA)
for (k in 1:nmesh) {
  res_kappa[k]=as.numeric(kappa-getmode(abs(lrf[[k]]$marginals.kappa[[1]])))
}
res_kappa=as.data.frame(res_kappa)
plot(res_kappa[[1]], type='l', xlab= "Mesh Number", ylab="Deviation between estimate and actual",
     main= expression(kappa))
lines(res_kappa$V1, col="purple", lwd=3)

#NOMINAL RANGE
xrange <- range(sapply(lrf, function(r) range(r$marginals.range.nominal[[1]][,1])))
yrange <- range(sapply(lrf, function(r) range(r$marginals.range.nominal[[1]][,2])))
plot(lrf[[1]]$marginals.range.nominal[[1]], type='l',
     xlim=xrange, ylim=yrange, xlab='nominal range', ylab='Density')
for (k in 1:nmesh)
  lines(lrf[[k]]$marginals.range.nominal[[1]], col=rcols[k], lwd=2)
abline(v=sqrt(8)/kappa, lty=2, lwd=2, col=3)
legend('topright', c(paste('mesh', 1:nmesh, sep=''), 'True'),
       lty=c(rep(1,nmesh), 2, 3), lwd=rep(2, nmesh), col=c(rcols,3,3), bty='n')

res_nr=matrix(nrow = nmesh, ncol = 1, NA)
true_nr=sqrt(8)/kappa
for (k in 1:nmesh) {
  res_nr[k]=as.numeric(true_nr-getmode(abs(lrf[[k]]$marginals.range.nominal[[1]])))
}
res_nr=as.data.frame(res_nr)
plot(res_nr[[1]], type='l', xlab= "Mesh Number", ylab="Deviation between estimate and actual",
     main= "Nominal Range")
lines(res_nr$V1, col="darkblue", lwd=3)

#TAU
xrange <- range(sapply(lrf, function(r) range(r$marginals.tau[[1]][,1])))
yrange <- range(sapply(lrf, function(r) range(r$marginals.tau[[1]][,2])))
plot(lrf[[1]]$marginals.tau[[1]], type='l',
     xlim=xrange, ylim=yrange, xlab=expression(tau), ylab='Density')
for (k in 1:nmesh)
  lines(lrf[[k]]$marginals.tau[[1]], col=rcols[k], lwd=2)
legend('topright', c(paste('mesh', 1:nmesh, sep=''), 'True'),
       lty=c(rep(1,nmesh), 2, 3), lwd=rep(2, nmesh), col=c(rcols,3,3), bty='n')

#COMPUTATION TIME AND NUMBER OF NODES
  par(mar = c(5, 4, 4, 4) + 0.3)              
  plot(x=cpu_plt$NUM_mesh, y=cpu_plt$CPU_time, col = "red",
       xlab="Mesh Number", ylab="CPU Time in Seconds", main="Graph of CPU time vs number of Vertices")
  lines(cpu_plt$CPU_time, col="red", lwd=3)
  par(new = TRUE)                             
  plot(x=cpu_plt$NUM_mesh, y=cpu_plt$nvert, col = "blue",              
       axes = FALSE, xlab = "", ylab = "")
  lines(cpu_plt$nvert, col="blue", lwd=3)
  axis(side = 4, at = pretty(range(cpu_plt$nvert)))      
  mtext("Number of Vertices", side = 4, line = 3)
  legend("topleft", legend=c("CPU Time", "Number of Vertices"),
         col=c("red", "blue"), lty=c(rep(1,2), 2, 3), lwd=rep(2, 2))




#RESULT VISUALISATION
all_res=as.data.frame(cbind(NUM_mesh,maxedge1,maxedge2,nvert,CPU_time))

dev_res=as.data.frame(cbind(NUM_mesh,nvert,CPU_time, 
                            res_beta0, res_sigmaE, 
                            res_sigmaX, res_kappa,res_nr))
cn=c("Mesh","Vertices", "CPU Time", "Deviations Beta0", "Deviations SigmaE",
     "Deviations SigmaX", "Deviations Kappa", "Deviations Nominal Range")
names(dev_res)=cn
min(dev_res$`Deviations Beta0`)
which.min(dev_res$`Deviations Beta0`)
min(dev_res$`Deviations SigmaE`)
which.min(dev_res$`Deviations SigmaE`)
min(dev_res$`Deviations SigmaX`)
which.min(dev_res$`Deviations SigmaX`)
min(dev_res$`Deviations Kappa`)
which.min(dev_res$`Deviations Kappa`)
min(dev_res$`Deviations Nominal Range`)
which.min(dev_res$`Deviations Nominal Range`)

which.max(dev_res$`Deviations Beta0`)
which.max(dev_res$`Deviations SigmaE`)
which.max(dev_res$`Deviations SigmaX`)
which.max(dev_res$`Deviations Kappa`)
which.max(dev_res$`Deviations Nominal Range`)















