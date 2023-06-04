#######################################################
#RESULTS OBTAINED USING 1 MILLION OBSERVATIONS
#######################################################

rm(list = ls(all.names = TRUE)) 
options(max.print = .Machine$integer.max)
gc() 

library(INLA)
library(leaflet)
library(ggplot2)
library(sf)
library("rnaturalearth")
library("rnaturalearthdata")
library(fields)
library(readr)
library(raster)
library(modeest)

Latitude = c(-28.583331, -31.47069 , 	-31.639122, -32.8999964 ,-34.4666648 ,-32.374331836 ,-33.2166658 ,-34.0027,-31.18736 ,-32.33083 )
Longitude = c(16.4833314,19.77601,18.5285, 17.9833294,  19.8999964, 20.806413441, 22.0333332, 24.7440,24.94991, 28.14981)

Site=c("WM01","WM02","WM03","WM04","WM05","WM06","WM07","WM08","WM09","WM10")
site_locat <- data.frame(Site, Latitude, Longitude)

wind_res1=load("C:/Users/matth/OneDrive/Documents/1 UNI STUFF/Masters Research/CODE AND DATA FOR FULL RUN/Wind1.RData")

result1=inla_res
summary(result1)
result1_lrf=lrf
summary(result1_lrf)

k=5
nmesh=8
rcols <- rainbow(nmesh)
e_col=rcols[k]

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


### SPATIAL PARMS
#SIGMA2E
s2.marg <- inla.tmarginal(function(x) 1/x, result1$marginals.hy[[1]])
s2.est=mlv(round(s2.marg[,1],4))
xrange <- range(s2.marg[,1])
yrange <- range(s2.marg[,2])
plot.default(s2.marg, type='l', xlim=xrange, ylim=yrange,
             xlab=expression(sigma[e]^2), ylab='Density', col=e_col, lwd=3,
             main="SigmaE Estimate")
abline(v=s2.est,col="Red",lwd=2)


#SIGMA2x
s2x.est = mlv(round(result1_lrf$marginals.variance.nominal[[1]][,1],4))
xrange <- range(result1_lrf$marginals.variance.nominal[[1]][,1])
yrange <- range(result1_lrf$marginals.variance.nominal[[1]][,2])
plot(result1_lrf$marginals.variance.nominal[[1]], type='l',
     xlim=xrange, ylim=yrange, xlab=expression(sigma[x]^2), ylab='Density', col=e_col, lwd=3,
     main="SigmaX Estimate")
abline(v=s2x.est,col="Red",lwd=2)

#KAPPA
kappa.est = mlv(round(result1_lrf$marginals.kappa[[1]][,1],4))
xrange <- range(result1_lrf$marginals.kappa[[1]][,1])
yrange <- range(result1_lrf$marginals.kappa[[1]][,2])
plot(result1_lrf$marginals.kappa[[1]], type='l',
     xlim=xrange, ylim=yrange, xlab=expression(kappa), ylab='Density', col=e_col, lwd=3,
     main="Kappa Estimate")
abline(v=kappa.est,col="Red",lwd=2)

#NOMINAL RANGE
nran.est=mlv(round(result1_lrf$marginals.range.nominal[[1]][,1],4))
xrange <- range(result1_lrf$marginals.range.nominal[[1]][,1])
yrange <- range(result1_lrf$marginals.range.nominal[[1]][,2])
plot(result1_lrf$marginals.range.nominal[[1]], type='l',
     xlim=xrange, ylim=yrange, xlab='nominal range', ylab='Density', col=e_col, lwd=3,
     main="Nominal Range Estimate")
abline(v=nran.est,col="Red",lwd=2)

#TAU
tau.est=mlv(round(result1_lrf$marginals.tau[[1]][,1],4))
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
beta0_est=result1$summary.fixed[1,1]
xrange <- range(result1$marginals.fixed[[1]][,1])
yrange <- range(result1$marginals.fixed[[1]][,2])
plot(result1$marginals.fix[[1]], type='l', xlim=xrange, ylim=yrange,
     xlab=expression(beta[0]), ylab='Density', col="red", lwd=3,
     main="Intercept Estimate")
abline(v=beta0_est,col="Blue",lwd=2)


#COS 
cos_est=result1$summary.fixed[2,1]
xrange <- range(result1$marginals.fixed[[2]][,1])
yrange <- range(result1$marginals.fixed[[2]][,2])
plot(result1$marginals.fix[[2]], type='l', xlim=xrange, ylim=yrange,
     xlab=expression(beta[cos]), ylab='Density', col="red", lwd=3,
     main="Cosine Estimate")
abline(v=cos_est,col="Blue",lwd=2)


#SIN 
sin_est=result1$summary.fixed[3,1]
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
     xlab= "Months across all Years",
     ylab= "Random Effect",
     lwd=3,
     ylim=yrange)
# lines(result1$summary.random$c_month[,1],result1$summary.random$c_month[,4], 
#       col="Red", lwd=0.5)
# lines(result1$summary.random$c_month[,1],result1$summary.random$c_month[,6], 
#       col="Red", lwd=0.5)
# legend("topright", legend=c("0.975 Credible Interval", "Mean","0.025 Credible Interval"),
#        col=c("red", "black","red"), lty=1:2, cex=0.8)
legend("topright", legend=c("Mean"),
       col=c("black"), lty=1:2, cex=0.8)




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
est_palt=result1$summary.hyperpar$`0.5quant`[2]
plot(result1$marginals.hyperpar$`Precision for altitude`, type = "l",col="purple",
     main= "Hyperparameter Posterior Density Function: Precision for Altitude ",
     lwd=3,
     xlab=expression(Sigma),
     ylab="Density")
abline(v=est_palt,col="Red", lwd=2)

#F_MONTH PRECISION
est_pfmonth=result1$summary.hyperpar$`0.5quant`[3]
plot(result1$marginals.hyperpar$`Precision for f_month`, type = "l",col="purple",
     main= "Hyperparameter Posterior Density Function: Repeating Month Precision",
     lwd=3,
     xlab=expression(Sigma),
     ylab="Density")
abline(v=est_pfmonth,col="Red", lwd=2)

#F_MONTH RHO
est_rhofmonth=result1$summary.hyperpar$`0.5quant`[4]
plot(result1$marginals.hyperpar$`Rho for f_month`, type = "l",col="purple",
     main= "Hyperparameter Posterior Density Function: Repeating Month Rho",
     lwd=3,
     xlab=expression(rho),
     ylab="Density")
abline(v=est_rhofmonth,col="Red", lwd=2)

#C_MONTH PRECISION
est_pcmonth=result1$summary.hyperpar$`0.5quant`[5]
plot(result1$marginals.hyperpar$`Precision for c_month`, type = "l",col="purple",
     main= "Hyperparameter Posterior Density Function: Cumulative Month Precision",
     lwd=3,
     xlab=expression(Sigma),
     ylab="Density")
abline(v=est_pcmonth,col="Red", lwd=2)

#C_MONTH RHO
est_rhocmonth=result1$summary.hyperpar$`0.5quant`[6]
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
set.seed(123)
world <- ne_countries(scale = "medium", returnclass = "sf" )
world <- world[world$name == "South Africa",]

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