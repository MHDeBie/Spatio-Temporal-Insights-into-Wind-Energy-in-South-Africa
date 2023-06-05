###MH De Bie Masters 2023
###Practial Model Mesh Selection

rm(list = ls(all.names = TRUE)) 
options(max.print = .Machine$integer.max)
gc() 



library(INLA)

######### SA map with station locations
library(leaflet)
library(ggplot2)
library(sf)
library("rnaturalearth")
library("rnaturalearthdata")
library(fields)
library(readr)
library(raster)
library(modeest)


set.seed(123)
world <- ne_countries(scale = "medium", returnclass = "sf" )
world <- world[world$name == "South Africa",]

Latitude = c(-28.583331, -31.47069 , 	-31.639122, -32.8999964 ,-34.4666648 ,-32.374331836 ,-33.2166658 ,-34.0027,-31.18736 ,-32.33083 )
Longitude = c(16.4833314,19.77601,18.5285, 17.9833294,  19.8999964, 20.806413441, 22.0333332, 24.7440,24.94991, 28.14981)

Site=c("WM01","WM02","WM03","WM04","WM05","WM06","WM07","WM08","WM09","WM10")
site_locat <- data.frame(Site, Latitude, Longitude)

leaflet(world) %>% addTiles() %>%
  addMarkers(lng = site_locat[,3], lat = site_locat[,2], label = site_locat[,1])


#Model
setwd("C:/Users/matth/OneDrive/Documents/1 UNI STUFF/Masters Research")
sample_Q=read_csv("C:\\Users\\matth\\OneDrive\\Documents\\1 UNI STUFF\\Masters Research\\R Studio Data Import\\Masters Research 2022 Refined Data\\Stratified Small Sample Stratified CORRECTED .csv")


##Copy for Matthew code

#run the code or not
n=nrow(sample_Q)
p=ncol(sample_Q)
sample_Q$Wind_Speed=as.numeric(sample_Q$Wind_Speed)
str(sample_Q)

nn=nrow(sample_Q)
pp=ncol(sample_Q)
sample_Q=sample_Q[,2:pp]



#Preparing a Spatial Component.
#Latitude Component
latitude = sample_Q$Station_ID
latitude[latitude == "WM01"] = -28.583331
latitude[latitude == "WM02"] = -31.47069
latitude[latitude == "WM03"] = -31.639122
latitude[latitude == "WM04"] = -32.8999964
latitude[latitude == "WM05"] = -34.4666648 
latitude[latitude == "WM06"] = -32.374331836 
latitude[latitude == "WM07"] = -33.2166658 
latitude[latitude == "WM08"] = -34.0027
latitude[latitude == "WM09"] = -31.18736
latitude[latitude == "WM10"] = -32.33083
sample_Q$latitude=latitude

#Longitude Component
longitude = sample_Q$Station_ID
longitude[longitude == "WM01"] = 16.4833314
longitude[longitude == "WM02"] = 19.77601
longitude[longitude == "WM03"] = 18.5285
longitude[longitude == "WM04"] = 17.9833294
longitude[longitude == "WM05"] = 19.8999964
longitude[longitude == "WM06"] = 20.806413441
longitude[longitude == "WM07"] = 22.0333332
longitude[longitude == "WM08"] = 24.7440
longitude[longitude == "WM09"] = 24.94991
longitude[longitude == "WM10"] = 28.14981
sample_Q$longitude=longitude


#Creating Meshes for Practical Selection
#This Spatial Attempt might work
str(sample_Q)
kk=ncol(sample_Q)

sample_Q[, (kk-1):kk] <- sapply(sample_Q[, (kk-1):kk], as.numeric)
sample_Q$lat_jit=jitter(sample_Q$latitude, factor = 1, amount = 0.65)
sample_Q$lon_jit=jitter(sample_Q$longitude, factor = 1, amount = 0.65)
# show map with Latitude 200 as center
maps::map( xlim = c(10, 40), ylim = c(-40,-20), fill = FALSE)
points(sample_Q$lon_jit, sample_Q$lat_jit, 
       col = "darkblue", cex = 2, pch = 20)
# add axes
maps::map.axes()
cc=ncol(sample_Q)

coords <- cbind( as.matrix(sample_Q$lon_jit), as.matrix(sample_Q$lat_jit) )
pl.dom <- cbind(c(15, 35), c(-35,-20))
nmesh=8

#MESH_A
mesh1 <- inla.mesh.2d(loc.domain = pl.dom,coords, max.edge = c(1, 1), offset = c(0.3, 0.3), cutoff = c(0.95))
plot(mesh1)
points(coords, col = "red", pch = 16)
#MESHCB
mesh2 <- inla.mesh.2d(loc.domain = pl.dom,coords, max.edge = c(0.9, 0.9), offset = c(0.2, 0.2), cutoff = c(0.90))
plot(mesh2)
points(coords, col = "red", pch = 16)
#MESH_C
mesh3 <- inla.mesh.2d(loc.domain = pl.dom,coords, max.edge = c(0.75, 0.75), offset = c(0.15, 0.15), cutoff = c(0.75))
plot(mesh3)
points(coords, col = "red", pch = 16)
#MESH_D
mesh4 <- inla.mesh.2d(loc.domain = pl.dom,coords, max.edge = c(0.725, 0.725), offset = c(0.15, 0.15), cutoff = c(0.725))
plot(mesh4)
points(coords, col = "red", pch = 16)
#MESH_E
mesh5 <- inla.mesh.2d(loc.domain = pl.dom,coords, max.edge = c(0.55, 0.55), offset = c(0.15, 0.15), cutoff = c(0.55))
plot(mesh5)
points(coords, col = "red", pch = 16)
#MESH_F
mesh6 <- inla.mesh.2d(loc.domain = pl.dom,coords, max.edge = c(0.50, 0.50), offset = c(0.15, 0.15), cutoff = c(0.50))
plot(mesh6)
points(coords, col = "red", pch = 16)
#MESH_G
mesh7 <- inla.mesh.2d(loc.domain = pl.dom,coords, max.edge = c(0.45, 0.45), offset = c(0.15, 0.15), cutoff = c(0.45))
plot(mesh7)
points(coords, col = "red", pch = 16)
#MESH_H
mesh8 <- inla.mesh.2d(loc.domain = pl.dom,coords, max.edge = c(0.40, 0.40), offset = c(0.15, 0.15), cutoff = c(0.40))
plot(mesh8)
points(coords, col = "red", pch = 16)

nvert=c(mesh1$n, mesh2$n, mesh3$n, mesh4$n, mesh5$n, mesh6$n, mesh7$n, mesh8$n)
nvert




y=as.numeric(sample_Q$Wind_Speed)
alt=as.matrix(sample_Q$Altitude)
f_month=as.numeric(as.matrix(sample_Q$f_month))
c_month=as.numeric(as.matrix(sample_Q$c_month))
cos_d=as.matrix(sample_Q$cos_direct)
sin_d=as.matrix(sample_Q$sin_direct)
Intercept=rep(1,nrow(sample_Q))
covars=as.matrix(cbind(sample_Q$lon_jit, 
                           sample_Q$lat_jit,
                           alt,
                           f_month,
                           c_month,
                           cos_d,
                           sin_d))
cn=c("long","lat","altitude", "f_month", "c_month", "cos_d", "sin_d")
names(covars)=cn
Nsub=nrow(sample_Q)
rcols <- rainbow(nmesh)


f.s <- y ~ -1 + Intercept + f(altitude, model = "rw2")+ f(f_month, model = "ar1")+f(c_month, model = "ar1")+ cos_d + sin_d + f(i, model=l.spde[[k]]) 


lrf <- lres <- l.dat <- l.spde <- l.a <- mesh.index <-dat <- list()
for (k in 1:nmesh) {
  l.a[[k]] <- inla.spde.make.A(get(paste('mesh', k, sep='')), loc=coords)
  l.spde[[k]] <- inla.spde2.matern(get(paste('mesh', k, sep='')), alpha=2)
  mesh.index[[k]] <- inla.spde.make.index(name = "field", n.spde = l.spde[[k]]$n.spde)
  l.dat[[k]] <- inla.stack(
                        data = list(resp = y),
                        A = list(l.a[[k]], 1,1,1,1,1,1), 
                        effects = list(i = 1:l.spde[[k]]$n.spde,
                        Intercept = Intercept,
                        altitude=alt,
                        f_month=f_month,
                        c_month=c_month,
                        cos_d=cos_d,
                        sin_d=sin_d),
                        tag = 'est')
  dat[[k]] <- inla.stack.data(l.dat[[k]])
  lres[[k]] <- inla(f.s, family = "Weibull", data = dat[[k]], verbose = TRUE, control.inla = list(cmin=0),
                    control.predictor=list(A=inla.stack.A(l.dat[[k]]),compute=TRUE),
                    control.compute = list(dic=TRUE, waic=TRUE,cpo=TRUE),
                    inla.mode = "experimental")
  lrf[[k]] <- inla.spde2.result(lres[[k]], 'i', l.spde[[k]], do.transf=TRUE)
}

#NUMBER OF VERTICES
nvert=c(mesh1$n, mesh2$n, mesh3$n, mesh4$n, mesh5$n, mesh6$n, mesh7$n, mesh8$n)
nvert

#COMPUTING RUNNING TIME
CPU_time=round(sapply(lres, function(x) x$cpu[2]), 2)
NUM_mesh=c(1:nmesh)
CPU_time
cpu_plt=as.data.frame(cbind(NUM_mesh,CPU_time,nvert))


mesh_trk=c("-A", "-B","-C","-D","-E","-F","-G","-H")
mesh_trk=as.matrix(mesh_trk)


#par(mfrow=c(2,3), mar=c(2.5,2.5,1,.5), mgp=c(1.5,.5,0), las=1)

#BETA0
xrange <- range(sapply(lres, function(x) range(x$marginals.fix[[1]][,1])))
yrange <- range(sapply(lres, function(x) range(x$marginals.fix[[1]][,2])))
plot(lres[[1]]$marginals.fix[[1]], type='l', xlim=xrange, ylim=yrange,
     xlab=expression(beta[0]), ylab='Density')
for (k in 1:nmesh)
  lines(lres[[k]]$marginals.fix[[1]], col=rcols[k], lwd=2)
legend('topright', c(paste('mesh', mesh_trk, sep=''), 'True'),
       lty=c(rep(1,nmesh), 2, 3), lwd=rep(2, nmesh), col=c(rcols,3,3), bty='n')



#SIGMA2e
s2.marg <- lapply(lres, function(m)
  inla.tmarginal(function(x) 1/x, m$marginals.hy[[1]]))
xrange <- range(sapply(s2.marg, function(x) range(x[,1])))
yrange <- range(sapply(s2.marg, function(x) range(x[,2])))
plot.default(s2.marg[[1]], type='l', xlim=xrange, ylim=yrange,
             xlab=expression(sigma[e]^2), ylab='Density')
for (k in 1:nmesh)
  lines(s2.marg[[k]], col=rcols[k], lwd=2)
legend('topright', c(paste('mesh', mesh_trk, sep=''), 'True'),
       lty=c(rep(1,nmesh), 2, 3), lwd=rep(2, nmesh), col=c(rcols,3,3), bty='n')


#SIGMA2x
xrange <- range(sapply(lrf, function(r) range(r$marginals.variance.nominal[[1]][,1])))
yrange <- range(sapply(lrf, function(r) range(r$marginals.variance.nominal[[1]][,2])))
plot(lrf[[1]]$marginals.variance.nominal[[1]], type='l',
     xlim=xrange, ylim=yrange, xlab=expression(sigma[x]^2), ylab='Density')
for (k in 1:nmesh)
  lines(lrf[[k]]$marginals.variance.nominal[[1]], col=rcols[k], lwd=2)
legend('topright', c(paste('mesh', mesh_trk, sep=''), 'True'),
       lty=c(rep(1,nmesh), 2, 3), lwd=rep(2, nmesh), col=c(rcols,3,3), bty='n')


#KAPPA
xrange <- range(sapply(lrf, function(r) range(r$marginals.kappa[[1]][,1])))
yrange <- range(sapply(lrf, function(r) range(r$marginals.kappa[[1]][,2])))
plot(lrf[[1]]$marginals.kappa[[1]], type='l',
     xlim=xrange, ylim=yrange, xlab=expression(kappa), ylab='Density')
for (k in 1:nmesh)
  lines(lrf[[k]]$marginals.kappa[[1]], col=rcols[k], lwd=2)
legend('topright', c(paste('mesh', mesh_trk, sep=''), 'True'),
       lty=c(rep(1,nmesh), 2, 3), lwd=rep(2, nmesh), col=c(rcols,3,3), bty='n')


#NOMINAL RANGE
xrange <- range(sapply(lrf, function(r) range(r$marginals.range.nominal[[1]][,1])))
yrange <- range(sapply(lrf, function(r) range(r$marginals.range.nominal[[1]][,2])))
plot(lrf[[1]]$marginals.range.nominal[[1]], type='l',
     xlim=xrange, ylim=yrange, xlab='nominal range', ylab='Density')
for (k in 1:nmesh)
  lines(lrf[[k]]$marginals.range.nominal[[1]], col=rcols[k], lwd=2)
legend('topright', c(paste('mesh', mesh_trk, sep=''), 'True'),
       lty=c(rep(1,nmesh), 2, 3), lwd=rep(2, nmesh), col=c(rcols,3,3), bty='n')

#TAU
xrange <- range(sapply(lrf, function(r) range(r$marginals.tau[[1]][,1])))
yrange <- range(sapply(lrf, function(r) range(r$marginals.tau[[1]][,2])))
plot(lrf[[1]]$marginals.tau[[1]], type='l',
     xlim=xrange, ylim=yrange, xlab=expression(tau), ylab='Density')
for (k in 1:nmesh)
  lines(lrf[[k]]$marginals.tau[[1]], col=rcols[k], lwd=2)
legend('topright', c(paste('mesh', mesh_trk, sep=''), 'True'),
       lty=c(rep(1,nmesh), 2, 3), lwd=rep(2, nmesh), col=c(rcols,3,3), bty='n')

#COMPUTATION TIME AND NUMBER OF NODES
mesh_trk=c("A", "B","C","D","E","F","G","H")
cpu_plt$msh_trk=mesh_trk
par(mar = c(5, 4, 4, 4) + 0.3)            
plot(x=cpu_plt$NUM_mesh, y=cpu_plt$CPU_time, col = "red", xaxt="n",
     xlab=c("Mesh"), ylab="CPU Time in Seconds", main="Graph of CPU time vs number of Vertices")
axis(side = 1, at = 1:nmesh, labels = c("A", "B","C","D","E","F","G","H"))
lines(cpu_plt$CPU_time, col="red", lwd=3)
par(new = TRUE)                             
plot(x=cpu_plt$NUM_mesh, y=cpu_plt$nvert, col = "blue",              
     axes = FALSE, xlab = "", ylab = "")
axis(side = 1, at = 1:nmesh, labels = c(" ", " "," "," "," "," "," "," "))
lines(cpu_plt$nvert, col="blue", lwd=3)
axis(side = 4, at = pretty(range(cpu_plt$nvert)))      
mtext("Number of Vertices", side = 4, line = 3)
legend("topleft", legend=c("CPU Time", "Number of Vertices"),
       col=c("red", "blue"), lty=c(rep(1,2), 2, 3), lwd=rep(2, 2))


###SELECTION METRICS
DIC_res=round(sapply(lres, function(x) x$dic$dic), 2)
WAIC_res=round(sapply(lres, function(x) x$waic$waic), 2)
logCPO_res=round(sapply(lres, function(x) -mean(log(x$cpo$cpo))), 6)

which.min(DIC_res)
which.min(WAIC_res)
which.min(logCPO_res)

which.max(DIC_res)
which.max(WAIC_res)
which.max(logCPO_res)


###POINT ESTIMATES
##INTERCEPT
res_beta0=round(sapply(lres, function(x) x$summary.fixed$mode[1]), 6)
res_beta0

##SIGMA2e
s2.marg <- lapply(lres, function(m)
  inla.tmarginal(function(x) 1/x, m$marginals.hy[[1]]))
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
res_sigmaE=matrix(nrow = nmesh, ncol = 1, NA)
for (k in 1:nmesh) {
  res_sigmaE[k]=round(getmode(s2.marg[[k]]),4)
}
res_sigmaE

##SIGMA2X
res_sigmaX=matrix(nrow = nmesh, ncol = 1, NA)
for (k in 1:nmesh) {
  res_sigmaX[k]=round(getmode(lrf[[k]]$marginals.variance.nominal[[1]]),4)
}
res_sigmaX

##KAPPA
res_kappa=matrix(nrow = nmesh, ncol = 1, NA)
for (k in 1:nmesh) {
  res_kappa[k]=getmode(abs(lrf[[k]]$marginals.kappa[[1]]))
}
res_kappa

##NOMINAL RANGE
res_nr=matrix(nrow = nmesh, ncol = 1, NA)
for (k in 1:nmesh) {
  res_nr[k]=getmode(abs(lrf[[k]]$marginals.range.nominal[[1]]))
}
res_nr

##TAU
res_tau=matrix(nrow = nmesh, ncol = 1, NA)
for (k in 1:nmesh){
  res_tau[k]=getmode(lrf[[k]]$marginals.tau[[1]])
}
res_tau










######################################################
############################MESH-E AS SELECTED MESH
#####################################################

#MESH_E
mesh5 <- inla.mesh.2d(loc.domain = pl.dom,coords, max.edge = c(0.55, 0.55), offset = c(0.15, 0.15), cutoff = c(0.55))
plot(mesh5)
points(coords, col = "red", pch = 16)

lrf <- inla_res <- l.dat <- l.spde <- l.a <- mesh.index <-dat <- list()
for (k in 5:5) {
  l.a[[k]] <- inla.spde.make.A(get(paste('mesh', k, sep='')), loc=coords)
  l.spde[[k]] <- inla.spde2.matern(get(paste('mesh', k, sep='')), alpha=2)
  mesh.index[[k]] <- inla.spde.make.index(name = "field", n.spde = l.spde[[k]]$n.spde)
  l.dat[[k]] <- inla.stack(
    data = list(resp = y),
    A = list(l.a[[k]], 1,1,1,1,1,1), 
    effects = list(i = 1:l.spde[[k]]$n.spde,
                   Intercept = Intercept,
                   altitude=alt,
                   f_month=f_month,
                   c_month=c_month,
                   cos_d=cos_d,
                   sin_d=sin_d),
    tag = 'est')
  dat[[k]] <- inla.stack.data(l.dat[[k]])
  inla_res[[k]] <- inla(f.s, family = "Weibull", data = dat[[k]], verbose = TRUE, control.inla = list(cmin=0),
                    control.predictor=list(A=inla.stack.A(l.dat[[k]]),compute=TRUE),
                    control.compute = list(dic=TRUE, waic=TRUE,cpo=TRUE),
                    inla.mode = "experimental")
  lrf[[k]] <- inla.spde2.result(inla_res[[k]], 'i', l.spde[[k]], do.transf=TRUE)
}
result1=inla_res[[5]]
summary(result1)
result1_lrf=lrf[[5]]
summary(result1_lrf)

k=5
e_col=rcols[k]

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


### SPATIAL PARMS
#SIGMA2E
s2.marg <- inla.tmarginal(function(x) 1/x, result1$marginals.hy[[1]])
#s2.est=modal(s2.marg[,1])
s2.est=mlv(round(s2.marg[,1],4), method = "mfv")
xrange <- range(s2.marg[,1])
yrange <- range(s2.marg[,2])
plot.default(s2.marg, type='l', xlim=xrange, ylim=yrange,
             xlab=expression(sigma[e]^2), ylab='Density', col=e_col, lwd=3,
             main="SigmaE Estimate")
abline(v=s2.est,col="Red",lwd=2)


#SIGMA2x
#s2x.est = modal(result1_lrf$marginals.variance.nominal[[1]][,1])
s2x.est = mlv(round(result1_lrf$marginals.variance.nominal[[1]][,1],4))
xrange <- range(result1_lrf$marginals.variance.nominal[[1]][,1])
yrange <- range(result1_lrf$marginals.variance.nominal[[1]][,2])
plot(result1_lrf$marginals.variance.nominal[[1]], type='l',
     xlim=xrange, ylim=yrange, xlab=expression(sigma[x]^2), ylab='Density', col=e_col, lwd=3,
     main="SigmaX Estimate")
abline(v=s2x.est,col="Red",lwd=2)

#KAPPA
#kappa.est = modal(result1_lrf$marginals.kappa[[1]][,1])
kappa.est = kappa.est = mlv(round(result1_lrf$marginals.kappa[[1]][,1],4))
xrange <- range(result1_lrf$marginals.kappa[[1]][,1])
yrange <- range(result1_lrf$marginals.kappa[[1]][,2])
plot(result1_lrf$marginals.kappa[[1]], type='l',
     xlim=xrange, ylim=yrange, xlab=expression(kappa), ylab='Density', col=e_col, lwd=3,
     main="Kappa Estimate")
abline(v=kappa.est,col="Red",lwd=2)

#NOMINAL RANGE
nran.est=modal(result1_lrf$marginals.range.nominal[[1]][,1])
xrange <- range(result1_lrf$marginals.range.nominal[[1]][,1])
yrange <- range(result1_lrf$marginals.range.nominal[[1]][,2])
plot(result1_lrf$marginals.range.nominal[[1]], type='l',
     xlim=xrange, ylim=yrange, xlab='nominal range', ylab='Density', col=e_col, lwd=3,
     main="Nominal Range Estimate")
abline(v=nran.est,col="Red",lwd=2)

#TAU
tau.est=modal(result1_lrf$marginals.tau[[1]][,1])
xrange <- range(result1_lrf$marginals.tau[[1]][,1])
yrange <- range(result1_lrf$marginals.tau[[1]][,2])
plot(result1_lrf$marginals.tau[[1]], type='l',
     xlim=xrange, ylim=yrange, xlab=expression(tau), ylab='Density', col=e_col, lwd=3,
     main="Tau Estimate")
abline(v=tau.est,col="Red",lwd=2)

###SPAT RES
res_spat=cbind(s2.est,s2x.est,kappa.est,nran.est,tau.est)



### FIXED PARMS
#BETA0
beta0_est=result1$summary.fixed[1,6]
xrange <- range(result1$marginals.fixed[[1]][,1])
yrange <- range(result1$marginals.fixed[[1]][,2])
plot(result1$marginals.fix[[1]], type='l', xlim=xrange, ylim=yrange,
     xlab=expression(beta[0]), ylab='Density', col="red", lwd=3,
     main="Intercept Estimate")
abline(v=beta0_est,col="Blue",lwd=2)


#COS 
cos_est=result1$summary.fixed[2,6]
xrange <- range(result1$marginals.fixed[[2]][,1])
yrange <- range(result1$marginals.fixed[[2]][,2])
plot(result1$marginals.fix[[2]], type='l', xlim=xrange, ylim=yrange,
     xlab=expression(beta[cos]), ylab='Density', col="red", lwd=3,
     main="Cosine Estimate")
abline(v=cos_est,col="Blue",lwd=2)


#SIN 
sin_est=result1$summary.fixed[3,6]
xrange <- range(result1$marginals.fixed[[3]][,1])
yrange <- range(result1$marginals.fixed[[3]][,2])
plot(result1$marginals.fix[[3]], type='l', xlim=xrange, ylim=yrange,
     xlab=expression(beta[sin]), ylab='Density', col="red", lwd=3,
     main="Sine Estimate")
abline(v=sin_est,col="Blue",lwd=2)

##Fixed Res
res_fix=cbind(beta0_est,cos_est,sin_est)


### RANDOM EFFECTS
# ALTITUDE: RANDOM EFFECT
plot(result1$summary.random$altitude[,1],
     result1$summary.random$altitude[,2], type = "l",
     main= "Summarising the Random Altitude Effect",
     xlab= "Altitude in Meters",
     ylab= "Random Effect",
     lwd=3)
lines(result1$summary.random$altitude[,1],result1$summary.random$altitude[,4], col="Red", lwd=2)
lines(result1$summary.random$altitude[,1],result1$summary.random$altitude[,6], col="Red", lwd=2)
legend("topright", legend=c("0.975 Credible Interval", "Mean","0.025 Credible Interval"),
       col=c("red", "black","red"), lty=1:2, cex=0.8)



# F_MONTH: RANDOM EFFECT
yrange1=min(result1$summary.random$f_month[,4])
yrange2=max(result1$summary.random$f_month[,6])
yrange=range(yrange1,yrange2)

plot(result1$summary.random$f_month[,1],
     result1$summary.random$f_month[,2], type = "l",
     main= "Summarising the Repeating Random Month Effect",
     xlab= "Months in a Single Year",
     ylab= "Random Effect",
     lwd=3,
     ylim=yrange)
lines(result1$summary.random$f_month[,1],result1$summary.random$f_month[,4], 
      col="Red", lwd=2)
lines(result1$summary.random$f_month[,1],result1$summary.random$f_month[,6], 
      col="Red", lwd=2)
legend("topright", legend=c("0.975 Credible Interval", "Mean","0.025 Credible Interval"),
       col=c("red", "black","red"), lty=1:2, cex=0.8)


# C_MONTH: RANDOM EFFECT
yrange1=min(result1$summary.random$c_month[,4])
yrange2=max(result1$summary.random$c_month[,6])
yrange=range(yrange1,yrange2)

plot(result1$summary.random$c_month[,1],
     result1$summary.random$c_month[,2], type = "l",
     main= "Summarising the Cumulative Random Month Effect",
     xlab= "Months across all years",
     ylab= "Random Effect",
     lwd=3,
     ylim=yrange)
lines(result1$summary.random$c_month[,1],result1$summary.random$c_month[,4], 
      col="Red", lwd=2)
lines(result1$summary.random$c_month[,1],result1$summary.random$c_month[,6], 
      col="Red", lwd=2)
legend("topright", legend=c("0.975 Credible Interval", "Mean","0.025 Credible Interval"),
       col=c("red", "black","red"), lty=1:2, cex=0.8)




### HYPER PARAMETERS
#WEIBULL ALPHA
est_alpha=result1$summary.hyperpar$mode[1]
plot(result1$marginals.hyperpar$`alpha parameter for weibull`, type = "l",col="purple",
     main= "Hyperparameter Posterior Density Function: Weibull Shape Parameter",
     lwd=3,
     xlab=expression(alpha),
     ylab="Density")
abline(v=est_alpha,col="Red", lwd=2)

#ALTITUDE PRECISION
est_palt=result1$summary.hyperpar$mode[2]
plot(result1$marginals.hyperpar$`Precision for altitude`, type = "l",col="purple",
     main= "Hyperparameter Posterior Density Function: Precision for Altitude ",
     lwd=3,
     xlab=expression(Sigma),
     ylab="Density")
abline(v=est_palt,col="Red", lwd=2)

#F_MONTH PRECISION
est_pfmonth=result1$summary.hyperpar$mode[3]
plot(result1$marginals.hyperpar$`Precision for f_month`, type = "l",col="purple",
     main= "Hyperparameter Posterior Density Function: Repeating Month Precision",
     lwd=3,
     xlab=expression(Sigma),
     ylab="Density")
abline(v=est_pfmonth,col="Red", lwd=2)

#F_MONTH RHO
est_rhofmonth=result1$summary.hyperpar$mode[4]
plot(result1$marginals.hyperpar$`Rho for f_month`, type = "l",col="purple",
     main= "Hyperparameter Posterior Density Function: Repeating Month Rho",
     lwd=3,
     xlab=expression(rho),
     ylab="Density")
abline(v=est_rhofmonth,col="Red", lwd=2)

#C_MONTH PRECISION
est_pcmonth=result1$summary.hyperpar$mode[5]
plot(result1$marginals.hyperpar$`Precision for c_month`, type = "l",col="purple",
     main= "Hyperparameter Posterior Density Function: Cumulative Month Precision",
     lwd=3,
     xlab=expression(Sigma),
     ylab="Density")
abline(v=est_pcmonth,col="Red", lwd=2)

#C_MONTH RHO
est_rhocmonth=result1$summary.hyperpar$mode[6]
plot(result1$marginals.hyperpar$`Rho for c_month`, type = "l",col="purple",
     main= "Hyperparameter Posterior Density Function: Cumulative Month Rho",
     lwd=3,
     xlab=expression(rho),
     ylab="Density")
abline(v=est_rhocmonth,col="Red", lwd=2)

## Hyperpar Res
res_hyp=cbind(est_alpha,est_palt,est_pfmonth,est_rhofmonth,est_pcmonth,est_rhocmonth)




res_spat
res_fix
res_hyp


######################################
#HEAT MAP
#Project SPDE onto the SA map

### projector to a grid 
r = result1
bbnw <- matrix(c(16.44756, -46.96289, 37.8877, -22.14629),
               byrow = FALSE, nrow = 2)
msh=mesh5 #MESHE=MESH5
bbnw <- bbox(as_Spatial(st_geometry(world)))
bbnw
r0 <- diff(range(bbnw[1, ])) / diff(range(bbnw[2, ]))
r0
prj <- inla.mesh.projector(msh, xlim = bbnw[1, ], 
                           ylim = bbnw[2, ], dims = round(c(r0, 1)*300))

## NA's were not to plot 

spat.m <- inla.mesh.project(prj, r$summary.random$i$mean)
spat.sd <- inla.mesh.project(prj, r$summary.random$i$sd)

spp <- sf::as_Spatial(st_geometry(world))
crs(spp) <- NA

ov <- over(SpatialPoints(prj$lattice$loc), spp)
spat.sd[is.na(ov)] <- NA
spat.m[is.na(ov)] <- NA

###MEAN OF SPATIAL FIELD
par(mfrow = c(1,1), mar = c(0.1,0.1,0.1,0.1))
plot(spp, xlim = c(28,28.5), ylim = c(-35,-22))
title('Mean of Spatial Field',line=-1.5)
image.plot(x = prj$x, y = prj$y, z = spat.m, add = TRUE)
points(x = site_locat[,3], y = site_locat[,2], cex = 2, pch = 18)

###SD OF SPATIAL FIELD
par(mfrow = c(1,1), mar = c(0.1,0.1,0.1,0.1))
plot(spp, xlim = c(28,28.5), ylim = c(-35,-22))
title('Standard Deviation of Spatial Field',line=-1.5)
image.plot(x = prj$x, y = prj$y, z = spat.sd , add = TRUE)
points(x = site_locat[,3], y = site_locat[,2], cex = 2, pch = 18)

############################################################################
######## PROJECTING THE MEAN FOR MESH E#####################################
r = result1
mesh_use=mesh5
bbnw <- matrix(c(16.44756, -46.96289, 37.8877, -22.14629),
               byrow = FALSE, nrow = 2)
r0 <- diff(range(bbnw[1, ])) / diff(range(bbnw[2, ]))

require(raster)
MeshPred=mesh_use
spde.pred <- inla.spde2.matern(mesh = MeshPred,
                               alpha = 2)
s.index.p <- inla.spde.make.index(name = "sp.field.pred",
                                  n.spde = spde.pred$n.spde)

A_pred <- inla.spde.make.A(mesh = MeshPred)
stackEst=l.dat[[k]]



stackPred <- inla.stack(
  data = list(resp = NA),
  A = list(A_pred), 
  effects = list(c(s.index.p, list(Intercept = 1))),
  tag = 'Pred')


StackJoin <- inla.stack(stackEst, stackPred)
index.pred <- inla.stack.index(StackJoin, "Pred")$data

post.mean.pred <- r$summary.linear.predictor[index.pred, "mean"]
post.sd.pred <- r$summary.linear.predictor[index.pred, "sd"]

proj.grid <- inla.mesh.projector(MeshPred, 
                                 xlim = bbnw[1, ], 
                                 ylim = bbnw[2, ], 
                                 dims = round(c(r0, 1)*300))

spat.m <- inla.mesh.project(proj.grid, post.mean.pred)

predmean <- t(spat.m)
predmean2 <- predmean[rev(1:length(predmean[,1])),]
predmean_ras <- raster(predmean2,
                       xmn = range(proj.grid$x)[1], xmx = range(proj.grid$x)[2],
                       ymn = range(proj.grid$y)[1], ymx = range(proj.grid$y)[2])

spp <- sf::as_Spatial(st_geometry(world))
crs(spp) <- NA

ov <- over(SpatialPoints(proj.grid$lattice$loc), spp)
spat.m[is.na(ov)] <- NA

par(mfrow = c(1,1), mar = c(0.1,0.1,0.1,0.1))
plot(spp, xlim = c(28,28.5), ylim = c(-35,-22))
title('Heat Map based on MESH_E',line=-1.5)
image.plot(x = proj.grid$x, y = proj.grid$y, z = spat.m, add = TRUE)
points(x = site_locat[,3], y = site_locat[,2], cex = 1, pch = 8)







#########################################
#MODEL ASSESSMENT CRITERIA
#BUILDING EACH MODEL
#FULL MODEL:
form1=y ~ -1 + Intercept +  f(i, model=l.spde[[k]])
form2=y ~ -1 + Intercept +  f(i, model=l.spde[[k]]) + f(altitude, model = "rw2")
form3=y ~ -1 + Intercept +  f(i, model=l.spde[[k]]) + f(altitude, model = "rw2") + f(f_month, model = "ar1")
form4=y ~ -1 + Intercept +  f(i, model=l.spde[[k]]) + f(altitude, model = "rw2") + f(f_month, model = "ar1")+f(c_month, model = "ar1")
form5=y ~ -1 + Intercept +  f(i, model=l.spde[[k]]) + f(altitude, model = "rw2")+ f(f_month, model = "ar1")+f(c_month, model = "ar1")  + cos_d
form6=y ~ -1 + Intercept +  f(i, model=l.spde[[k]]) + f(altitude, model = "rw2")+ f(f_month, model = "ar1")+f(c_month, model = "ar1")+ cos_d + sin_d

nform=6
#MESH_E
mesh_use <- inla.mesh.2d(loc.domain = pl.dom,coords, max.edge = c(0.55, 0.55), offset = c(0.15, 0.15), cutoff = c(0.55))
plot(mesh_use)
points(coords, col = "red", pch = 16)

lrf <- inla_res <- l.dat <- l.spde <- l.a <- mesh.index <-dat<-form <- list()
for (k in 1:6) {
  l.a[[k]] <- inla.spde.make.A(mesh_use, loc=coords)
  l.spde[[k]] <- inla.spde2.matern(mesh_use, alpha=2)
  mesh.index[[k]] <- inla.spde.make.index(name = "field", n.spde = l.spde[[k]]$n.spde)
  l.dat[[k]] <- inla.stack(
    data = list(resp = y),
    A = list(l.a[[k]], 1,1,1,1,1,1), 
    effects = list(i = 1:l.spde[[k]]$n.spde,
                   Intercept = Intercept,
                   altitude=alt,
                   f_month=f_month,
                   c_month=c_month,
                   cos_d=cos_d,
                   sin_d=sin_d),
    tag = 'est')
  dat[[k]] <- inla.stack.data(l.dat[[k]])
  form[[k]] <- get(paste('form', k, sep=''))
  inla_res[[k]] <- inla(formula = form[[k]], family = "Weibull", data = dat[[k]], verbose = TRUE, control.inla = list(cmin=0),
                        control.predictor=list(A=inla.stack.A(l.dat[[k]]),compute=TRUE),
                        control.compute = list(dic=TRUE, waic=TRUE,cpo=TRUE),
                        inla.mode = "experimental")
  lrf[[k]] <- inla.spde2.result(inla_res[[k]], 'i', l.spde[[k]], do.transf=TRUE)
}

reduced_model_res=inla_res


#WAIC MATRIX
res_WAIC=matrix(nrow = nform, ncol = 1, NA)
for (k in 1:nform) {
  res_WAIC[k]=reduced_model_res[[k]]$waic$waic
}
res_WAIC

#WAIC PLOT
form_trk=c("SM1", "SM2","SM3","SM4","SM5","Spatial-Model1")
NUM_form=c(1:nform)
waic_plt=as.data.frame(cbind(NUM_form,res_WAIC))
yrange <- range(res_WAIC)
plot(waic_plt$V2, type='l', xaxt="n",
     ylim=yrange, xlab="Model", ylab='WAIC', col="orange", lwd=3,
     main="WAIC for Each Model")
axis(side = 1, at = 1:nform, labels = c("SM1", "SM2","SM3","SM4","SM5","Spatial-Model1"))


#DIC MATRIX
res_DIC=matrix(nrow = nform, ncol = 1, NA)
for (k in 1:nform) {
  res_DIC[k]=reduced_model_res[[k]]$dic$dic
}
res_DIC

#DIC PLOT
form_trk=c("SM1", "SM2","SM3","SM4","SM5","Spatial-Model1")
NUM_form=c(1:nform)
dic_plt=as.data.frame(cbind(NUM_form,res_DIC))
yrange <- range(res_DIC)
plot(dic_plt$V2, type='l', xaxt="n",
     ylim=yrange, xlab="Model", ylab='WAIC', col="orange", lwd=3,
     main="DIC for Each Model")
axis(side = 1, at = 1:nform, labels = c("SM1", "SM2","SM3","SM4","SM5","Spatial-Model1"))



###########################################################################
#QUANTILE REGRESSION
#CONSTRUCTING QUANTILE REGRESSION MODELS
quants=c(0.05,0.25,0.5,0.75,0.95)
n_quant=length(quants)
n_quant=as.vector(n_quant)
f.s=y ~ -1 + Intercept +  f(i, model=l.spde[[k]]) + f(altitude, model = "rw2")+ f(f_month, model = "ar1")+f(c_month, model = "ar1")+ cos_d + sin_d



#REMEMBER THIS IS MESH-E
mesh_use <- inla.mesh.2d(loc.domain = pl.dom,coords, max.edge = c(0.55, 0.55), offset = c(0.15, 0.15), cutoff = c(0.55))


lrf <- inla_res <- l.dat <- l.spde <- l.a <- mesh.index <-dat<-form <- list()
for (k in 1:n_quant) {
  l.a[[k]] <- inla.spde.make.A(mesh_use, loc=coords)
  l.spde[[k]] <- inla.spde2.matern(mesh_use, alpha=2)
  mesh.index[[k]] <- inla.spde.make.index(name = "field", n.spde = l.spde[[k]]$n.spde)
  l.dat[[k]] <- inla.stack(
    data = list(resp = y),
    A = list(l.a[[k]], 1,1,1,1,1,1), 
    effects = list(i = 1:l.spde[[k]]$n.spde,
                   Intercept = Intercept,
                   altitude=alt,
                   f_month=f_month,
                   c_month=c_month,
                   cos_d=cos_d,
                   sin_d=sin_d),
    tag = 'est')
  dat[[k]] <- inla.stack.data(l.dat[[k]])
  inla_res[[k]] <- inla(formula = f.s, family = "Weibull", data = dat[[k]], verbose = TRUE, control.inla = list(cmin=0),
                        control.predictor=list(A=inla.stack.A(l.dat[[k]]),compute=TRUE),
                        control.compute = list(dic=TRUE, waic=TRUE,cpo=TRUE),
                        inla.mode = "experimental",
                        control.family = list(control.link=list(model="quantile", 
                                                                quantile=quants[[k]])))
  lrf[[k]] <- inla.spde2.result(inla_res[[k]], 'i', l.spde[[k]], do.transf=TRUE)
}

# setwd("C:/Users/matth/OneDrive/Documents/1 UNI STUFF/Masters Research/Early Univariate Model RStudio/Updated Spatial Code July 2022/QREG MODEL OBJECTS")
# saveRDS(inla_res[[1]], file = "Matthew_INLA_QREG_RESULTS.RDS")
# test =readRDS(Matthew_INLA_QREG_RESULTS.RDS)
# DOESN'T WORK FOR SOME REASON




inla_res_q1=inla_res[[1]]
inla_res_q2=inla_res[[2]]
inla_res_q3=inla_res[[3]]


#QUANTILE REGRESSION FIXED SUMMARY
inla_res_q1$summary.fixed
inla_res_q2$summary.fixed
inla_res_q3$summary.fixed
nam_quants=c("_05","_25","_50","_75","_95")

##########################
#GRAPH OF ESTIMATED POSTERIORS-FIXED EFFECTS AT QUANTILES
##########################
#BETA0
xrange <- range(sapply(inla_res, function(x) range(x$marginals.fix[[1]][,1])))
yrange <- range(sapply(inla_res, function(x) range(x$marginals.fix[[1]][,2])))
plot(inla_res[[1]]$marginals.fix[[1]], type='l', xlim=xrange, ylim=yrange,
     xlab=expression(beta[0]), ylab='Density',
     main="Estimated Quantile Regression Posteriors for Intercept")
for (k in 1:n_quant)
  lines(inla_res[[k]]$marginals.fix[[1]], col=rcols[k], lwd=2)
legend('topright', c(paste('quantile', nam_quants, sep='')),
       lty=c(rep(1,nmesh), 2, 3), lwd=rep(2, nmesh), col=c(rcols,3,3), bty='n')

###POINT ESTIMATES INTERCEPT
qres_beta0=round(sapply(inla_res, function(x) x$summary.fixed$mode[1]), 4)
qres_beta0


#COS
xrange <- range(sapply(inla_res, function(x) range(x$marginals.fix[[2]][,1])))
yrange <- range(sapply(inla_res, function(x) range(x$marginals.fix[[2]][,2])))
plot(inla_res[[1]]$marginals.fix[[1]], type='l', xlim=xrange, ylim=yrange,
     xlab=expression(beta[cos]), ylab='Density',
     main="Estimated Quantile Regression Posteriors for Cos fixed effect")
for (k in 1:n_quant)
  lines(inla_res[[k]]$marginals.fix[[2]], col=rcols[k], lwd=2)
legend('topright', c(paste('quantile', nam_quants, sep='')),
       lty=c(rep(1,nmesh), 2, 3), lwd=rep(2, nmesh), col=c(rcols,3,3), bty='n')
###POINT ESTIMATES INTERCEPT
qres_cos=round(sapply(inla_res, function(x) x$summary.fixed$mode[2]), 4)
qres_cos



#SIN
xrange <- range(sapply(inla_res, function(x) range(x$marginals.fix[[3]][,1])))
yrange <- range(sapply(inla_res, function(x) range(x$marginals.fix[[3]][,2])))
plot(inla_res[[1]]$marginals.fix[[1]], type='l', xlim=xrange, ylim=yrange,
     xlab=expression(beta[sin]), ylab='Density',
     main="Estimated Quantile Regression Posteriors for Sin fixed effect")
for (k in 1:n_quant)
  lines(inla_res[[k]]$marginals.fix[[3]], col=rcols[k], lwd=2)
legend('topright', c(paste('quantile', nam_quants, sep='')),
       lty=c(rep(1,nmesh), 2, 3), lwd=rep(2, nmesh), col=c(rcols,3,3), bty='n')
###POINT ESTIMATES INTERCEPT
qres_sin=round(sapply(inla_res, function(x) x$summary.fixed$mode[3]), 4)
qres_sin

#######################################################
### QUANTILE REGRESSION GRAPHS RANDOM EFFECTS
#######################################################
# ALTITUDE: RANDOM EFFECT
plot(x=inla_res[[1]]$summary.random$altitude[,1],
     y=inla_res[[1]]$summary.random$altitude[,2], type = "l",
     main= "Quantile Regression Results for Altitude Random effect",
     xlab= "Altitude in Meters",
     ylab= "Random Effect",
     lwd=2)
for (k in 1:n_quant) 
  lines(x=inla_res[[1]]$summary.random$altitude[,1],
        y=inla_res[[k]]$summary.random$altitude[,2], 
        col=rcols[k], lwd=2)
legend('bottomright', c(paste('quantile', nam_quants, sep='')),
       lty=c(rep(1,nmesh), 2, 3), lwd=rep(2, nmesh), col=c(rcols,3,3), bty='n')

# FMonth: RANDOM EFFECT
plot(x=inla_res[[1]]$summary.random$f_month[,1],
     y=inla_res[[1]]$summary.random$f_month[,2], type = "l",
     main= "Quantile Regression Results for Repeating Month Random effect",
     xlab= "Months",
     ylab= "Random Effect",
     lwd=2)
for (k in 1:n_quant) 
  lines(x=inla_res[[k]]$summary.random$f_month[,1],
        y=inla_res[[k]]$summary.random$f_month[,2], 
        col=rcols[k], lwd=2)
legend('bottomright', c(paste('quantile', nam_quants, sep='')),
       lty=c(rep(1,nmesh), 2, 3), lwd=rep(2, nmesh), col=c(rcols,3,3), bty='n')

# CMonth: RANDOM EFFECT
plot(x=inla_res[[1]]$summary.random$c_month[,1],
     y=inla_res[[1]]$summary.random$c_month[,2], type = "l",
     main= "Quantile Regression Results for Cumulative Month Random effect",
     xlab= "Months",
     ylab= "Random Effect",
     lwd=2)
for (k in 1:n_quant) 
  lines(x=inla_res[[k]]$summary.random$c_month[,1],
        y=inla_res[[k]]$summary.random$c_month[,2], 
        col=rcols[k], lwd=2)
legend('bottomright', c(paste('quantile', nam_quants, sep='')),
       lty=c(rep(1,nmesh), 2, 3), lwd=rep(2, nmesh), col=c(rcols,3,3), bty='n')



#######################################################
### QUANTILE REGRESSION GRAPHS SPATIAL PARAMETERS
#######################################################


#SIGMA2e

s2.marg <- lapply(inla_res, function(m)
  inla.tmarginal(function(x) 1/x, m$marginals.hy[[1]]))
xrange <- range(sapply(s2.marg, function(x) range(x[,1])))
yrange <- range(sapply(s2.marg, function(x) range(x[,2])))
plot.default(s2.marg[[1]], type='l', xlim=xrange, ylim=yrange,
             xlab=expression(sigma[e]^2), ylab='Density', main= 'Quantile Regression Results for Sigma2e')
for (k in 1:n_quant)
  lines(s2.marg[[k]], col=rcols[k], lwd=2)
legend('topright', c(paste('quantile', nam_quants, sep='')),
       lty=c(rep(1,n_quant), 2, 3), lwd=rep(2, n_quant), col=c(rcols,3,3), bty='n')
s2emarg_mode=matrix(nrow=n_quant,1,1)
for (q in 1:n_quant) {
  s2e_track=as.data.frame(s2.marg[[q]])
  s2e_track=round(s2e_track,digits = 4)
  s2emarg_mode[q,]=getmode(s2e_track[,1])
}
s2emarg_mode

       


#SIGMA2x
xrange <- range(sapply(lrf, function(r) range(r$marginals.variance.nominal[[1]][,1])))
yrange <- range(sapply(lrf, function(r) range(r$marginals.variance.nominal[[1]][,2])))
plot(lrf[[1]]$marginals.variance.nominal[[1]], type='l',
     xlim=xrange, ylim=yrange, xlab=expression(sigma[x]^2), ylab='Density', main= 'Quantile Regression Results for Sigma2x')
for (k in 1:n_quant)
  lines(lrf[[k]]$marginals.variance.nominal[[1]], col=rcols[k], lwd=2)
legend('topright', c(paste('quantile', nam_quants, sep='')),
       lty=c(rep(1,n_quant), 2, 3), lwd=rep(2, n_quant), col=c(rcols,3,3), bty='n')
s2xmarg_mode=matrix(nrow=n_quant,1,1)
for (q in 1:n_quant) {
  s2x_track=as.data.frame(lrf[[q]]$marginals.variance.nominal[[1]])
  s2x_track=round(s2x_track,digits = 4)
  s2xmarg_mode[q,]=getmode(s2x_track[,1])
}
s2xmarg_mode


#KAPPA
xrange <- range(sapply(lrf, function(r) range(r$marginals.kappa[[1]][,1])))
yrange <- range(sapply(lrf, function(r) range(r$marginals.kappa[[1]][,2])))
plot(lrf[[1]]$marginals.kappa[[1]], type='l',
     xlim=xrange, ylim=yrange, xlab=expression(kappa), ylab='Density', main= 'Quantile Regression Results for Kappa')
for (k in 1:n_quant)
  lines(lrf[[k]]$marginals.kappa[[1]], col=rcols[k], lwd=2)
legend('topright', c(paste('quantile', nam_quants, sep='')),
       lty=c(rep(1,n_quant), 2, 3), lwd=rep(2, n_quant), col=c(rcols,3,3), bty='n')
kappa_mode=matrix(nrow=n_quant,1,1)
for (q in 1:n_quant) {
  kappa_track=as.data.frame(lrf[[q]]$marginals.kappa[[1]])
  kappa_track=round(kappa_track,digits = 3)
  kappa_mode[q,]=getmode(kappa_track[,1])
}
kappa_mode

#NOMINAL RANGE
xrange <- range(sapply(lrf, function(r) range(r$marginals.range.nominal[[1]][,1])))
yrange <- range(sapply(lrf, function(r) range(r$marginals.range.nominal[[1]][,2])))
plot(lrf[[1]]$marginals.range.nominal[[1]], type='l',
     xlim=xrange, ylim=yrange, xlab='nominal range', ylab='Density', main= 'Quantile Regression Results for Nominal Range')
for (k in 1:n_quant)
  lines(lrf[[k]]$marginals.range.nominal[[1]], col=rcols[k], lwd=2)
legend('topright', c(paste('quantile', nam_quants, sep='')),
       lty=c(rep(1,n_quant), 2, 3), lwd=rep(2, n_quant), col=c(rcols,3,3), bty='n')
nomrange_mode=matrix(nrow=n_quant,1,1)
for (q in 1:n_quant) {
  nomrange_track=as.data.frame(lrf[[q]]$marginals.range.nominal[[1]])
  nomrange_track=round(nomrange_track,digits = 2)
  nomrange_mode[q,]=getmode(nomrange_track[,1])
}
nomrange_mode

#TAU
xrange <- range(sapply(lrf, function(r) range(r$marginals.tau[[1]][,1])))
yrange <- range(sapply(lrf, function(r) range(r$marginals.tau[[1]][,2])))
plot(lrf[[1]]$marginals.tau[[1]], type='l',
     xlim=xrange, ylim=yrange, xlab=expression(tau), ylab='Density', main= 'Quantile Regression Results for Tau')
for (k in 1:n_quant)
  lines(lrf[[k]]$marginals.tau[[1]], col=rcols[k], lwd=2)
legend('topright', c(paste('quantile', nam_quants, sep='')),
       lty=c(rep(1,n_quant), 2, 3), lwd=rep(2, n_quant), col=c(rcols,3,3), bty='n')
tau_mode=matrix(nrow=n_quant,1,1)
for (q in 1:n_quant) {
  tau_track=as.data.frame(lrf[[q]]$marginals.tau[[1]])
  tau_track=round(tau_track,digits = 2)
  tau_mode[q,]=getmode(tau_track[,1])
}
tau_mode


cn=c("S2E", "S2X", "KAPPA","NOM_RANGE","TAU")
qres_spat_parms=cbind(s2emarg_mode,s2xmarg_mode,kappa_mode,nomrange_mode,tau_mode)
colnames(qres_spat_parms)=cn
qres_spat_parms

############################################################
### QUANTILE REGRESSION GRAPHS HYPERPARAMETERS
############################################################

#ALTITUDE PRECISION
xrange=c(0,50000)
yrange=c(0,0.0001)
plot(inla_res[[3]]$marginals.hyperpar$`Precision for altitude`, type = "l",col=rcols[3],
     main= "Hyperparameter Posterior Density Function: Precision for Altitude ",
     lwd=3,
     xlab=expression(Sigma),
     ylab="Density",
     xlim=xrange,
     ylim=yrange)
for (k in 1:n_quant) 
  lines(inla_res[[k]]$marginals.hyperpar$`Precision for altitude` ,col=rcols[k], lwd=3)
legend('topright', c(paste('quantile', nam_quants, sep='')),
       lty=c(rep(1,nmesh), 2, 3), lwd=rep(2, nmesh), col=c(rcols,3,3), bty='n')
alt_mode=matrix(nrow=n_quant,1,1)
for (q in 1:n_quant) {
  alt_mode[q,]=round(inla_res[[q]]$summary.hyperpar$mode[4], digits = 4)
}
inla_res[[1]]$summary.hyperpar


#F_MONTH PRECISION
xrange=c(0,50000)
yrange=c(0,0.0001)
plot(inla_res[[2]]$marginals.hyperpar$`Precision for f_month`, type = "l",col=rcols[2],
     main= "Hyperparameter Posterior Density Function: Repeating Month Precision",
     lwd=3,
     xlab=expression(Sigma),
     ylab="Density",
     xlim=xrange,
     ylim=yrange)
for (k in 1:n_quant) 
  lines(inla_res[[k]]$marginals.hyperpar$`Precision for f_month` ,col=rcols[k], lwd=3)
legend('topright', c(paste('quantile', nam_quants, sep='')),
       lty=c(rep(1,nmesh), 2, 3), lwd=rep(2, nmesh), col=c(rcols,3,3), bty='n')
fmonth_mode=matrix(nrow=n_quant,1,1)
for (q in 1:n_quant) {
  fmonth_mode[q,]=round(inla_res[[q]]$summary.hyperpar$mode[5], digits = 4)
}

#C_MONTH PRECISION
xrange=c(0,1500)
yrange=c(0,0.008)
plot(inla_res[[2]]$marginals.hyperpar$`Precision for c_month`, type = "l",col=rcols[2],
     main= "Hyperparameter Posterior Density Function: Cumulative Month Precision",
     lwd=3,
     xlab=expression(Sigma),
     ylab="Density",
     xlim=xrange,
     ylim=yrange)
for (k in 1:n_quant) 
  lines(inla_res[[k]]$marginals.hyperpar$`Precision for c_month` ,col=rcols[k], lwd=3)
legend('topright', c(paste('quantile', nam_quants, sep='')),
       lty=c(rep(1,nmesh), 2, 3), lwd=rep(2, nmesh), col=c(rcols,3,3), bty='n')
cmonth_mode=matrix(nrow=n_quant,1,1)
for (q in 1:n_quant) {
  cmonth_mode[q,]=round(inla_res[[q]]$summary.hyperpar$mode[7], digits = 4)
}

#F_Month RHO
xrange=c(-1,1)
yrange=c(0,4)
plot(inla_res[[2]]$marginals.hyperpar$`Rho for f_month`, type = "l",col=rcols[2],
     main= "Hyperparameter Posterior Density Function: Repeating Month Rho",
     lwd=3,
     xlab=expression(rho),
     ylab="Density",
     xlim=xrange,
     ylim=yrange)
for (k in 1:n_quant) 
  lines(inla_res[[k]]$marginals.hyperpar$`Rho for f_month` ,col=rcols[k], lwd=3)
legend('topright', c(paste('quantile', nam_quants, sep='')),
       lty=c(rep(1,nmesh), 2, 3), lwd=rep(2, nmesh), col=c(rcols,3,3), bty='n')
rho_fmonth_mode=matrix(nrow=n_quant,1,1)
for (q in 1:n_quant) {
  rho_fmonth_mode[q,]=round(inla_res[[q]]$summary.hyperpar$mode[6], digits = 4)
}

#C_Month RHO
xrange=c(0.1,1)
yrange=c(0,10)
plot(inla_res[[3]]$marginals.hyperpar$`Rho for c_month`, type = "l",col=rcols[3],
     main= "Hyperparameter Posterior Density Function: Cumulative Month Rho",
     lwd=3,
     xlab=expression(rho),
     ylab="Density",
     xlim=xrange,
     ylim=yrange)
for (k in 1:n_quant) 
  lines(inla_res[[k]]$marginals.hyperpar$`Rho for c_month` ,col=rcols[k], lwd=3)
legend('topright', c(paste('quantile', nam_quants, sep='')),
       lty=c(rep(1,nmesh), 2, 3), lwd=rep(2, nmesh), col=c(rcols,3,3), bty='n')
rho_cmonth_mode=matrix(nrow=n_quant,1,1)
for (q in 1:n_quant) {
  rho_cmonth_mode[q,]=round(inla_res[[q]]$summary.hyperpar$mode[8], digits = 4)
}


#ALPHA
xrange=c(1.95,2.20)
yrange=c(0,22)
plot(inla_res[[1]]$marginals.hyperpar$`alpha parameter for weibull`, type = "l",
     col=rcols[1],
     main= "Hyperparameter Posterior Density Function: Weibull Shape Parameter",
     lwd=3,
     xlab=expression(alpha),
     ylab="Density",
     xlim=xrange, ylim=yrange)
for (k in 1:n_quant) 
  lines(inla_res[[k]]$marginals.hyperpar$`alpha parameter for weibull`, col=rcols[k], lwd=3)
legend('topright', c(paste('quantile', nam_quants, sep='')),
       lty=c(rep(1,nmesh), 2, 3), lwd=rep(2, nmesh), col=c(rcols,3,3), bty='n')
alpha_mode=matrix(nrow=n_quant,1,1)
for (q in 1:n_quant) {
  alpha_mode[q,]=round(inla_res[[q]]$summary.hyperpar$mode[1], digits = 4)
}

qres_hyp_parms=cbind(alt_mode,fmonth_mode,cmonth_mode,rho_fmonth_mode,
                     rho_cmonth_mode,alpha_mode)
cn=c("Alt_Prec", "fmonth_Prec", "cmonth_prec", "rho_fmonth","rho_cmonth","alpha_mode")
colnames(qres_hyp_parms)=cn
qres_hyp_parms

#######################################################
#QUANTILE REGRESSION HEAT MAPS ORIGINAL ATTEMPT
########################################################
for (k in 1:n_quant) {
  r = inla_res[[k]]
  bbnw <- matrix(c(16.44756, -46.96289, 37.8877, -22.14629),
                 byrow = FALSE, nrow = 2)
  msh=mesh5 #MESHE=MESH5
  bbnw <- bbox(as_Spatial(st_geometry(world)))
  bbnw
  r0 <- diff(range(bbnw[1, ])) / diff(range(bbnw[2, ]))
  r0
  prj <- inla.mesh.projector(msh, xlim = bbnw[1, ], 
                             ylim = bbnw[2, ], dims = round(c(r0, 1)*300))
  
  spat.m <- inla.mesh.project(prj, r$summary.random$i$mean)
  spat.sd <- inla.mesh.project(prj, r$summary.random$i$sd)
  
  spp <- sf::as_Spatial(st_geometry(world))
  crs(spp) <- NA
  
  ov <- over(SpatialPoints(prj$lattice$loc), spp)
  spat.sd[is.na(ov)] <- NA
  spat.m[is.na(ov)] <- NA
  
  par(mfrow = c(1,1), mar = c(0.1,0.1,0.1,0.1))
  plot(spp, xlim = c(28,28.5), ylim = c(-35,-22))
  title(c(paste('Heat Map based on Quantile', nam_quants[[k]], sep='')),line=-1.5)
  image.plot(x = prj$x, y = prj$y, z = spat.m, add = TRUE)
  points(x = site_locat[,3], y = site_locat[,2], cex = 1, pch = 8)
}

k=1
r = inla_res[[k]]
bbnw <- matrix(c(16.44756, -46.96289, 37.8877, -22.14629),
               byrow = FALSE, nrow = 2)

############################################################################
###################### NEW MAPPING WORK#####################################
require(raster)
MeshPred=mesh_use
spde.pred <- inla.spde2.matern(mesh = MeshPred,
                               alpha = 2)


A_pred <- inla.spde.make.A(mesh = MeshPred)
stackEst=l.dat[[k]]
stackPred <- inla.stack(
  data = list(resp = NA),
  A = list(l.a[[k]], 1,1,1,1,1,1), 
  effects = list(i = 1:l.spde[[k]]$n.spde,
                 Intercept = Intercept,
                 altitude=alt,
                 f_month=f_month,
                 c_month=c_month,
                 cos_d=cos_d,
                 sin_d=sin_d),
  tag = 'Pred')


StackJoin <- inla.stack(stackEst, stackPred)
index.pred <- inla.stack.index(StackJoin, "Pred")$data

Mod_Pred=inla(formula = f.s, family = "Weibull", data = inla.stack.data(StackJoin, spde = l.spde[[k]]), verbose = TRUE, control.inla = list(cmin=0),
              control.predictor=list(A=inla.stack.A(StackJoin),compute=TRUE),
              control.compute = list(dic=TRUE, waic=TRUE,cpo=TRUE),
              inla.mode = "experimental",
              control.family = list(control.link=list(model="quantile", 
                                                      quantile=quants[[k]])))

###############################
#Cholmod error 'out of memory' at file//occurs here
post.mean.pred <- r$summary.linear.predictor[index.pred, "mean"]
###############################
post.sd.pred <- r$summary.linear.predictor[index.pred, "sd"]
dimen= round(c(r0, 1)*300)
proj.grid <- inla.mesh.projector(MeshPred, 
                                 xlim = bbnw[1, ], 
                                 ylim = bbnw[2, ], 
                                 dims = round(c(r0, 1)*300))

prj <- inla.mesh.projector(msh, xlim = bbnw[1, ], 
                           ylim = bbnw[2, ], dims = round(c(r0, 1)*300))

spat.m <- inla.mesh.project(prj, r$summary.random$i$mean)

spat.m <- inla.mesh.project(prj, post.mean.pred)

spat.sd <- inla.mesh.project(prj, r$summary.random$i$sd)

spp <- sf::as_Spatial(st_geometry(world))
crs(spp) <- NA

ov <- over(SpatialPoints(prj$lattice$loc), spp)
spat.sd[is.na(ov)] <- NA
spat.m[is.na(ov)] <- NA

par(mfrow = c(1,1), mar = c(0.1,0.1,0.1,0.1))
plot(spp, xlim = c(28,28.5), ylim = c(-35,-22))
title(c(paste('Heat Map based on Quantile', nam_quants[[k]], sep='')),line=-1.5)
image.plot(x = prj$x, y = prj$y, z = spat.m, add = TRUE)
points(x = site_locat[,3], y = site_locat[,2], cex = 1, pch = 8)









post.mean.pred.grid <- inla.mesh.project(proj.grid, post.mean.pred)
post.sd.pred.grid <- inla.mesh.project(proj.grid, post.sd.pred)

predmean <- t(post.mean.pred.grid)
predmean2 <- predmean[rev(1:length(predmean[,1])),]
predmean_ras <- raster(predmean2,
                       xmn = range(projgrid$x)[1], xmx = range(projgrid$x)[2],
                       ymn = range(projgrid$y)[1], ymx = range(projgrid$y)[2])

par(mfrow = c(1,1), mar = c(0.1,0.1,0.1,0.1))
plot(predmean_ras, xlim = c(28,28.5), ylim = c(-35,-22))
title(c(paste('Heat Map based on Quantile', nam_quants[[k]], sep='')),line=-1.5)
image.plot(x = prj$x, y = prj$y, z = spat.m, add = TRUE)
points(x = site_locat[,3], y = site_locat[,2], cex = 1, pch = 8)

##############################################################################
########### THIS SHOULD PRODUCE A HEAT MAP
##############################################################################



























#This Spatial Attempt might work
str(sample_Q)
kk=ncol(sample_Q)

sample_Q[, (kk-1):kk] <- sapply(sample_Q[, (kk-1):kk], as.numeric)
sample_Q$lat_jit=jitter(sample_Q$latitude, factor = 1, amount = 0.65)
sample_Q$lon_jit=jitter(sample_Q$longitude, factor = 1, amount = 0.65)
# show map with Latitude 200 as center
maps::map( xlim = c(10, 40), ylim = c(-40,-20), fill = FALSE)
points(sample_Q$lon_jit, sample_Q$lat_jit, 
       col = "darkblue", cex = 2, pch = 20)
# add axes
maps::map.axes()
cc=ncol(sample_Q)

coords <- cbind( as.matrix(sample_Q$lon_jit), as.matrix(sample_Q$lat_jit) )
pl.dom <- cbind(c(15, 35), c(-35,-20))
msh <- inla.mesh.2d(loc.domain = pl.dom,coords, max.edge = c(0.5,0.5 ), offset = c(0.15, 0.15), 
                    cutoff = c(0.5)) 
plot(msh)
# points(sample_Q$lon_jit, sample_Q$lat_jit, 
#        col = "darkblue", cex = 2, pch = 20)

A1 <- inla.spde.make.A(msh, loc = coords)
proj_nc=ncol(A1)
proj_nr=nrow(A1)
elem_a1=proj_nc*proj_nr
str(A1)



#Setting the SPDE
spde <- inla.spde2.pcmatern(
  # Mesh and smoothness parameter
  mesh = msh, alpha = 2,
  prior.range = c(0.3, 0.5),
  prior.sigma = c(10, 0.01)) 



#Creating a STACK
str(sample_Q)

alt=as.matrix(sample_Q$Altitude)
f_month=as.numeric(as.matrix(sample_Q$f_month))
c_month=as.numeric(as.matrix(sample_Q$c_month))
cos_d=as.matrix(sample_Q$cos_direct)
sin_d=as.matrix(sample_Q$sin_direct)

mesh.index <- inla.spde.make.index(name = "field", n.spde = spde$n.spde)

stk.dat <- inla.stack(data = list(y = sample_Q$Wind_Speed),
                      A = list(A1, 1), 
                      effects = list(c(mesh.index,list(Intercept=1)),list(long=inla.group(coords[,1]), lat=inla.group(coords[,2]),
                                                                          altitude=inla.group(alt), f_month=inla.group(f_month), c_month=inla.group(c_month),
                                                                          cos_d=inla.group(cos_d), sin_d=inla.group(sin_d))),
                      tag = 'est')


f.s <- y ~ -1 + Intercept + f(altitude, model = "rw2")+ f(f_month, model = "ar1")+f(c_month, model = "ar1") + f(field, model = spde) + cos_d + sin_d

result1 <- inla(f.s, family = "Weibull", data = inla.stack.data(stk.dat), verbose = TRUE, control.inla = list(cmin=0),
                control.predictor = list(A = inla.stack.A(stk.dat), compute = TRUE),
                control.compute = list(dic=TRUE, waic=TRUE),
                inla.mode = "experimental")

r.f <- inla.spde2.result(result1, "field", spde, do.transf = TRUE)


r_line=result1$summary.hyperpar$mode[7]
std_line=result1$summary.hyperpar$mode[8]**2

r_line
std_line

plot.default(r.f$marginals.variance.nominal[[1]], type = "l", xlab = expression(sigma[x]^2), 
             ylab = "Density", main="Model2 Standard Deviation of Spatial Field")
abline(v=std_line,col="Green")

plot.default(r.f$marginals.range.nominal[[1]], type = "l", xlab = "Practical range", 
             ylab = "Density", main="Model2 Range of Spatial Field")
abline(v=r_line,col="Green")
#####

#Creating Alternate Mesh Values for plotting purposes
mesh_A <- inla.mesh.2d(loc.domain = pl.dom,coords, max.edge = c(1, 1), offset = c(0.3, 0.3), cutoff = c(0.95))
plot(mesh_A)
mesh_B <- inla.mesh.2d(loc.domain = pl.dom,coords, max.edge = c(0.9, 0.9), offset = c(0.2, 0.2), cutoff = c(0.90))
plot(mesh_B)
mesh_C <- inla.mesh.2d(loc.domain = pl.dom,coords, max.edge = c(0.75, 0.75), offset = c(0.15, 0.15), cutoff = c(0.75))
plot(mesh_C)
mesh_D <- inla.mesh.2d(loc.domain = pl.dom,coords, max.edge = c(0.725, 0.725), offset = c(0.15, 0.15), cutoff = c(0.725))
plot(mesh_D)
mesh_E <- inla.mesh.2d(loc.domain = pl.dom,coords, max.edge = c(0.55, 0.55), offset = c(0.15, 0.15), cutoff = c(0.55))
plot(mesh_E)
mesh_F <- inla.mesh.2d(loc.domain = pl.dom,coords, max.edge = c(0.50, 0.50), offset = c(0.15, 0.15), cutoff = c(0.50))
plot(mesh_F)
mesh_G <- inla.mesh.2d(loc.domain = pl.dom,coords, max.edge = c(0.45, 0.45), offset = c(0.15, 0.15), cutoff = c(0.45))
plot(mesh_G)
mesh_H <- inla.mesh.2d(loc.domain = pl.dom,coords, max.edge = c(0.40, 0.40), offset = c(0.15, 0.15), cutoff = c(0.40))
plot(mesh_H)