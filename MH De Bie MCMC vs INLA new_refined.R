rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc()


#update.packages()
#install.packages("cli")
#devtools::install_github("rpruim/CalvinBayes")

#install.packages("rjags")
library(rjags)
library(lme4)
library(coda)
library(runjags)
library(jagsUI)
library(glmmTMB)
library(sn)
library(car)
library(fastDummies)
library(plotrix)
library(aod)
library(CalvinBayes)
library(INLA)

set.seed(123)


###############################################################################
#JAGS AND INLA: MULTIPLE SAMPLE SIZES
##############################################################################
n_per_clust=c(25,50,75,100,125)
n_total=n_per_clust*5
nam_sim=as.character(n_total)
k_sim=length(n_per_clust)
rcols <- rainbow(k_sim)

n_model=4



jags_m1res <- jags_m2res <- inla_m1res <- inla_m2res <-list()
jags_m1time <- jags_m2time <- inla_m1time <- inla_m2time <-list()

jags_m1b0<- jags_m1b1 <- jags_m1b2 <- jags_m1alpha <-list()
inla_m1b0<- inla_m1b1 <- inla_m1b2 <- inla_m1alpha <-list()
jags_m2b0<- jags_m2b1 <- jags_m2b2 <- jags_m2alpha <- jags_m2noise <-list()
inla_m2b0<- inla_m2b1 <- inla_m2b2 <- inla_m2alpha <- inla_m2noise <-list()
jags_m2r1b0_1<-jags_m2r1b0_2<-jags_m2r1b0_3<-jags_m2r1b0_4<-jags_m2r1b0_5<-list()
jags_m1psrf<-jags_m2psrf<-list()
n_simtrk<-list()
for (k in 1:k_sim) {
r1.cluster.n <- 5
n_per_cluster1 <- n_per_clust[k]


r1.b0.noise <- 0.25
r1.b0.mu <- 0


b0 <- 0.95
b1 <- 0.5
b2 <- 0.75

alpha = 1.95

true_parms=cbind(b0,b1,b2,alpha)

#################################################################
# JAGS MODEL1
#################################################################
n <- r1.cluster.n * n_per_cluster1 #n_sim effectively
n_simtrk[[k]]=n
r1.cluster.i <- gl(n=r1.cluster.n, k=n_per_cluster1)

x1 <- rnorm(n, 0, 1)
x2 <- rnorm(n, 0, 1)

d <- data.frame(r1.cluster.i = r1.cluster.i, x0 = 1, x1 = x1, 
                x2 = x2, y = NA, y_init=NA, alpha=NA)

for(i in 1:n) {
  
  fixed1 <- b0 + b1 * x1[i] + b2 * x2[i]
  d$y_init[i] <- fixed1 
  #BASIC SIMULATION WE ONLY HAVE A FIXED EFFECT WITH TWO COVARIATES
}

d$y= rweibull(n, scale = 1/(exp(d$y_init)), shape = alpha)
sim_data1=d
X <- model.matrix(~ x1 + x2, data=d)
y <- d$y

d_jags <- list (
  n = dim(d)[1], 
  y = y,
  X = X,
  P = dim(X)[2]
)

inits1 <- list(
  b = rnorm(dim(X)[2], 0, 10),
  shape = 1.75,
  .RNG.name="base::Super-Duper"
)
inits2 <- list(
  b = rnorm(dim(X)[2], 0, 10),
  shape = 1.75,
  .RNG.name="base::Wichmann-Hill"
)

iterations <- 2500 * 2
jags <- run.jags('C:/Users/matth/OneDrive/Documents/1 UNI STUFF/Masters Research/Early Univariate Model RStudio/Updated Spatial Code July 2022/Linear_multilevel_models/JAGS/weibull 1 - fixed effects only.R',
                 data = d_jags,
                 monitor=c('b', 'shape'),
                 n.chains=2, method='parallel', inits=list(inits1,inits2),     
                 sample = iterations,
                 burnin = round(iterations/4),
                 adapt = round(iterations/10)
)
jags_m1res[[k]]=jags
jags_m1time[[k]]=jags$timetaken
jags_m1b0[[k]]=jags$summaries[1,6]
jags_m1b1[[k]]=jags$summaries[2,6]
jags_m1b2[[k]]=jags$summaries[3,6]
jags_m1alpha[[k]]=jags$summaries[4,6]
jags_m1psrf[[k]]=jags$summaries[,11]
#################################################################
# JAGS MODEL 2 
#################################################################
X <- model.matrix(~ x1 + x2, data=d)
r1.cluster.i <- as.numeric(as.factor(d$r1.cluster.i))
y <- d$y

d_jags <- list (
  n = dim(d)[1], 
  r1.cluster.n = length(unique(r1.cluster.i)), 
  r1.cluster.i = r1.cluster.i,
  y = y,
  X = X,
  P = dim(X)[2]
)

inits1 <- list(
  r1.b0.noise = runif(1, 0, 10),
  b = rnorm(dim(X)[2], 0, 10),
  shape = 1.55,
  .RNG.name="base::Super-Duper"
)
inits2 <- list(
  r1.b0.noise = runif(1, 0, 10),
  #y.noise = runif(1, 0, 100),
  b = rnorm(dim(X)[2], 0, 10),
  shape = 1.55,
  .RNG.name="base::Wichmann-Hill"
)
sim_data2=d



iterations <- 2500 * 2
jags <- run.jags('C:/Users/matth/OneDrive/Documents/1 UNI STUFF/Masters Research/Early Univariate Model RStudio/Updated Spatial Code July 2022/Linear_multilevel_models/JAGS/weibull 2 - random intercept attempt.R',
                 data = d_jags,
                 monitor=c('b', 'r1.b0.mu', 'r1.b0.noise', 'shape', 
                           'r1.b0[1]', 'r1.b0[2]','r1.b0[3]','r1.b0[4]','r1.b0[5]'),
                 n.chains=2, method='parallel', inits=list(inits1,inits2),     
                 sample = iterations,
                 burnin = round(iterations/4),
                 adapt = round(iterations/10)
)
jags_m2res[[k]]=jags
jags_m2time[[k]]=jags$timetaken
jags_m2b0[[k]]=jags$summaries[1,6]
jags_m2b1[[k]]=jags$summaries[2,6]
jags_m2b2[[k]]=jags$summaries[3,6]
jags_m2noise[[k]]=jags$summaries[5,6]
jags_m2alpha[[k]]=jags$summaries[6,6]

jags_m2r1b0_1[[k]]=jags$summaries[7,6]
jags_m2r1b0_2[[k]]=jags$summaries[8,6]
jags_m2r1b0_3[[k]]=jags$summaries[9,6]
jags_m2r1b0_4[[k]]=jags$summaries[10,6]
jags_m2r1b0_5[[k]]=jags$summaries[11,6]

jags_m2psrf[[k]]=jags$summaries[,11]
#################################################################
# INLA MODEL1
#################################################################
f1=y ~ 1 + x1 + x2
inla_res <- inla(f1, family = "Weibull", data = sim_data1,
                   control.predictor = list(hyper="Exponential", compute = TRUE),
                   control.compute = list(dic=TRUE, waic=TRUE),
                   control.family = list(list(variant = 1)))
inla_m1res[[k]]=inla_res

inla_m1time[[k]]=inla_res$cpu.used[4]
inla_m1b0[[k]]=inla_res$summary.fixed$mode[1]
inla_m1b1[[k]]=inla_res$summary.fixed$mode[2]
inla_m1b2[[k]]=inla_res$summary.fixed$mode[3]
inla_m1alpha[[k]]=inla_res$summary.hyperpar$mode

#################################################################
# INLA MODEL2
#################################################################
f2=y ~ 1 + x1 + x2 + f(r1.cluster.i,model="iid")
inla_res <- inla(f2, family = "Weibull", data = sim_data2,
                   control.predictor = list(hyper="Exponential", compute = TRUE),
                   control.compute = list(dic=TRUE, waic=TRUE),
                   control.family = list(list(variant = 1)))
inla_m2res[[k]]=inla_res

inla_m2time[[k]]=inla_res$cpu.used[4]
inla_m2b0[[k]]=inla_res$summary.fixed$mode[1]
inla_m2b1[[k]]=inla_res$summary.fixed$mode[2]
inla_m2b2[[k]]=inla_res$summary.fixed$mode[3]

inla_m2alpha[[k]]=inla_res$summary.hyperpar$mode[1]
}



###########################################################
################START OF PLOTS
##########################################################

jags_summarym1 <- print(jags_m1res[[1]])
rownames(jags_summarym1)[1:dim(X)[2]] <- colnames(X)
print(jags_summarym1)


plot(jags_m1res[[1]], vars = "b[1]", layout = c(1,1))
plot(jags_m1res[[1]], vars = "b[2]", layout = c(1,1))
plot(jags_m1res[[1]], vars = "b[3]", layout = c(1,1))
plot(jags_m1res[[1]], vars = "shape", layout = c(1,1))

jags_summarym2 <- print(jags_m2res[[1]])
rownames(jags_summarym2)[1:dim(X)[2]] <- colnames(X)
print(jags_summarym2)


plot(jags_m2res[[1]], vars = "b[1]", layout = c(1,1))
plot(jags_m2res[[1]], vars = "b[2]", layout = c(1,1))
plot(jags_m2res[[1]], vars = "b[3]", layout = c(1,1))
plot(jags_m2res[[1]], vars = "shape", layout = c(1,1))

#################################################################
# INLA MODEL1
#################################################################
inla_summarym1 <- summary(inla_m1res[[1]])
print(inla_summarym1)

#BETA0
xrange <- range(sapply(inla_m1res, function(x) range(x$marginals.fix[[1]][,1])))
yrange <- range(sapply(inla_m1res, function(x) range(x$marginals.fix[[1]][,2])))
plot(inla_m1res[[1]]$marginals.fix[[1]], type='l', xlim=xrange, ylim=yrange,
     xlab=expression(beta[0]), ylab='Density',
     main="Estimated Posteriors for Intercept Model1")
for (k in 1:k_sim)
  lines(inla_m1res[[k]]$marginals.fix[[1]], col=rcols[k], lwd=2)
legend('topright', c(paste('simulation_', nam_sim, sep='')),
       lty=c(rep(1,k_sim), 2, 3), lwd=rep(2, k_sim), col=c(rcols,3,3), bty='n')

#BETA1
xrange <- range(sapply(inla_m1res, function(x) range(x$marginals.fix[[2]][,1])))
yrange <- range(sapply(inla_m1res, function(x) range(x$marginals.fix[[2]][,2])))
plot(inla_m1res[[1]]$marginals.fix[[2]], type='l', xlim=xrange, ylim=yrange,
     xlab=expression(beta[1]), ylab='Density',
     main="Estimated Posteriors for B1 Model1")
for (k in 1:k_sim)
  lines(inla_m1res[[k]]$marginals.fix[[2]], col=rcols[k], lwd=2)
legend('topright', c(paste('simulation_', nam_sim, sep='')),
       lty=c(rep(1,k_sim), 2, 3), lwd=rep(2, k_sim), col=c(rcols,3,3), bty='n')

#BETA2
xrange <- range(sapply(inla_m1res, function(x) range(x$marginals.fix[[3]][,1])))
yrange <- range(sapply(inla_m1res, function(x) range(x$marginals.fix[[3]][,2])))
plot(inla_m1res[[1]]$marginals.fix[[3]], type='l', xlim=xrange, ylim=yrange,
     xlab=expression(beta[2]), ylab='Density',
     main="Estimated Posteriors for B1 Model1")
for (k in 1:k_sim)
  lines(inla_m1res[[k]]$marginals.fix[[3]], col=rcols[k], lwd=2)
legend('topright', c(paste('simulation_', nam_sim, sep='')),
       lty=c(rep(1,k_sim), 2, 3), lwd=rep(2, k_sim), col=c(rcols,3,3), bty='n')


#ALPHA
xrange=c(0.50,3.00)
yrange=c(0,6)
plot(inla_m1res[[1]]$marginals.hyperpar$`alpha parameter for weibull`, type = "l",
     col=rcols[1],
     main= "Hyperparameter Posterior Density Function: Weibull Shape Parameter Model1",
     lwd=3,
     xlab=expression(alpha),
     ylab="Density",
     xlim=xrange, ylim=yrange)
for (k in 1:k_sim) 
  lines(inla_m1res[[k]]$marginals.hyperpar$`alpha parameter for weibull`, col=rcols[k], lwd=3)
legend('topright', c(paste('simulation_', nam_sim, sep='')),
       lty=c(rep(1,k_sim), 2, 3), lwd=rep(2, k_sim), col=c(rcols,3,3), bty='n')
alpha_mode=matrix(nrow=k_sim,1,1)


#################################################################
# INLA MODEL2
#################################################################
inla_summarym2 <- summary(inla_m2res[[1]])
print(inla_summarym2)


#BETA0
xrange <- range(sapply(inla_m2res, function(x) range(x$marginals.fix[[1]][,1])))
yrange <- range(sapply(inla_m2res, function(x) range(x$marginals.fix[[1]][,2])))
plot(inla_m2res[[1]]$marginals.fix[[1]], type='l', xlim=xrange, ylim=yrange,
     xlab=expression(beta[0]), ylab='Density',
     main="Estimated Posteriors for Intercept Model2")
for (k in 1:k_sim)
  lines(inla_m2res[[k]]$marginals.fix[[1]], col=rcols[k], lwd=2)
legend('topright', c(paste('simulation_', nam_sim, sep='')),
       lty=c(rep(1,k_sim), 2, 3), lwd=rep(2, k_sim), col=c(rcols,3,3), bty='n')

#BETA1
xrange=c(0.00,1.00)
yrange=c(0,15)
plot(inla_m2res[[1]]$marginals.fix[[2]], type='l', xlim=xrange, ylim=yrange,
     xlab=expression(beta[1]), ylab='Density',
     main="Estimated Posteriors for B1 Model2")
for (k in 1:k_sim)
  lines(inla_m2res[[k]]$marginals.fix[[2]], col=rcols[k], lwd=2)
legend('topright', c(paste('simulation_', nam_sim, sep='')),
       lty=c(rep(1,k_sim), 2, 3), lwd=rep(2, k_sim), col=c(rcols,3,3), bty='n')

#BETA2
xrange=c(0.20,1.00)
yrange=c(0,15)
plot(inla_m2res[[1]]$marginals.fix[[3]], type='l', xlim=xrange, ylim=yrange,
     xlab=expression(beta[2]), ylab='Density',
     main="Estimated Posteriors for B2 Model2")
for (k in 1:k_sim)
  lines(inla_m2res[[k]]$marginals.fix[[3]], col=rcols[k], lwd=2)
legend('topright', c(paste('simulation_', nam_sim, sep='')),
       lty=c(rep(1,k_sim), 2, 3), lwd=rep(2, k_sim), col=c(rcols,3,3), bty='n')


#ALPHA
xrange=c(0.50,3.00)
yrange=c(0,6)
plot(inla_m2res[[1]]$marginals.hyperpar$`alpha parameter for weibull`, type = "l",
     col=rcols[1],
     main= "Hyperparameter Posterior Density Function: Weibull Shape Parameter Model2",
     lwd=3,
     xlab=expression(alpha),
     ylab="Density",
     xlim=xrange, ylim=yrange)
for (k in 1:k_sim) 
  lines(inla_m2res[[k]]$marginals.hyperpar$`alpha parameter for weibull`, col=rcols[k], lwd=3)
legend('topright', c(paste('simulation_', nam_sim, sep='')),
       lty=c(rep(1,k_sim), 2, 3), lwd=rep(2, k_sim), col=c(rcols,3,3), bty='n')
alpha_mode=matrix(nrow=k_sim,1,1)


#ALL TIME
inla_m1time=round(sapply(inla_m1res, function(x) x$cpu[2]), 10)
inla_m2time=round(sapply(inla_m2res, function(x) x$cpu[2]), 10)

all_time=cbind(n_simtrk,jags_m1time,jags_m2time,inla_m1time,inla_m2time)
cn=c("N_sim","JAGS_M1 Time", "JAGS_M2 Time", "INLA_M1 Time", "INLA_M2 Time")
colnames(all_time)=cn
all_time=as.data.frame(all_time)


yrange_max=max(range(all_time$`JAGS_M2 Time`))
yrange_min=min(range(all_time$`JAGS_M1 Time`))
yrange_vec=rbind(yrange_min,yrange_max)
yrange=range(yrange_vec)
plot(x=all_time$N_sim, y=all_time$`JAGS_M1 Time`, type='l', 
     xlab="Size of Simulation", 
     ylab="Computational Time",
     main="JAGS Model Computation Time in Seconds",
     col=rcols[1],
     lwd=3,
     ylim=yrange)
lines(x=all_time$N_sim,y=all_time$`JAGS_M2 Time`, col=rcols[2], lwd=3)
legend('topleft', legend=c('JAGS M1', 'JAGS M2'),
       col=c(rcols[1], rcols[2]), lty=c(rep(1,2), 2, 3), lwd=rep(2, 2))

yrange_max=max(range(all_time$`INLA_M2 Time`))
yrange_min=min(range(all_time$`INLA_M1 Time`))
yrange_vec=rbind(yrange_min,yrange_max)
yrange=range(yrange_vec)
plot(x=all_time$N_sim, y=all_time$`INLA_M1 Time`, type='l', 
     xlab="Size of Simulation", 
     ylab="Computational Time",
     main="INLA Model Computation Time in Seconds",
     col=rcols[3],
     lwd=3,
     ylim=yrange)
lines(x=all_time$N_sim,y=all_time$`INLA_M2 Time`, col=rcols[4], lwd=3)
legend('topleft', legend=c('INLA M1', 'INLA M2'),
       col=c(rcols[3], rcols[4]), lty=c(rep(1,2), 2, 3), lwd=rep(2, 2))

####TRACKING DEVIATIONS
#TRK b0

jags_m1b0=as.numeric(jags_m1b0)
res_b0_jagsm1=matrix(nrow = k_sim, ncol = 1, NA)

jags_m2b0=as.numeric(jags_m2b0)
res_b0_jagsm2=matrix(nrow = k_sim, ncol = 1, NA)

inla_m1b0=as.numeric(inla_m1b0)
res_b0_inlam1=matrix(nrow = k_sim, ncol = 1, NA)

inla_m1b0=as.numeric(inla_m1b0)
res_b0_inlam1=matrix(nrow = k_sim, ncol = 1, NA)

inla_m2b0=as.numeric(inla_m2b0)
res_b0_inlam2=matrix(nrow = k_sim, ncol = 1, NA)

for (k in 1:k_sim) {
  res_b0_jagsm1[k]=as.numeric(abs(b0-jags_m1b0[k]))
  res_b0_jagsm2[k]=as.numeric(abs(b0-jags_m2b0[k]))
  res_b0_inlam1[k]=as.numeric(abs(b0-inla_m1b0[k]))
  res_b0_inlam2[k]=as.numeric(abs(b0-inla_m2b0[k]))
}
res_b0=cbind(n_simtrk,res_b0_jagsm1,res_b0_jagsm2,res_b0_inlam1,res_b0_inlam2)
res_b0=as.data.frame(res_b0)
cn=c("N_sim","JAGS_M1_Dev", "JAGS_M2_Dev", "INLA_M1_Dev", "INLA_M2_Dev")
colnames(res_b0)=cn

yrange=c(0,0.15)
plot(x=res_b0$N_sim, y=res_b0$JAGS_M1_Dev, type='l', 
     xlab= "Size of Simulation", 
     ylab="Deviation between estimate and actual",
     main= expression(beta[0]),
  col=rcols[1],
  lwd=3,
  ylim=yrange)
lines(x=res_b0$N_sim, y=res_b0$JAGS_M2_Dev, col=rcols[2], lwd=3)
lines(x=res_b0$N_sim, y=res_b0$INLA_M1_Dev, col=rcols[3], lwd=3)
lines(x=res_b0$N_sim, y=res_b0$INLA_M2_Dev, col=rcols[4], lwd=3)
legend('topright', legend=c('JAGS M1', 'JAGS M2','INLA M1', 'INLA M2'),
       col=c(rcols[1], rcols[2],rcols[3], rcols[4]), lty=c(rep(1,4), 2, 3), lwd=rep(2, 2))



plot(x=res_b0$N_sim, y=res_b0$JAGS_M2_Dev, type='l', 
     xlab= "Size of Simulation", 
     ylab="Deviation between estimate and actual JAGS M2",
     main= expression(beta[0]),
     col=rcols[2],
     lwd=3)



#TRK b1

jags_m1b1=as.numeric(jags_m1b1)
res_b1_jagsm1=matrix(nrow = k_sim, ncol = 1, NA)

jags_m2b1=as.numeric(jags_m2b1)
res_b1_jagsm2=matrix(nrow = k_sim, ncol = 1, NA)

inla_m1b1=as.numeric(inla_m1b1)
res_b1_inlam1=matrix(nrow = k_sim, ncol = 1, NA)

inla_m1b1=as.numeric(inla_m1b1)
res_b1_inlam1=matrix(nrow = k_sim, ncol = 1, NA)

inla_m2b1=as.numeric(inla_m2b1)
res_b1_inlam2=matrix(nrow = k_sim, ncol = 1, NA)

for (k in 1:k_sim) {
  res_b1_jagsm1[k]=as.numeric(abs(b1-jags_m1b1[k]))
  res_b1_jagsm2[k]=as.numeric(abs(b1-jags_m2b1[k]))
  res_b1_inlam1[k]=as.numeric(abs(b1-inla_m1b1[k]))
  res_b1_inlam2[k]=as.numeric(abs(b1-inla_m2b1[k]))
}
res_b1=cbind(n_simtrk,res_b1_jagsm1,res_b1_jagsm2,res_b1_inlam1,res_b1_inlam2)
res_b1=as.data.frame(res_b1)
cn=c("N_sim","JAGS_M1_Dev", "JAGS_M2_Dev", "INLA_M1_Dev", "INLA_M2_Dev")
colnames(res_b1)=cn

yrange=c(0,0.08)
plot(x=res_b1$N_sim, y=res_b1$JAGS_M1_Dev, type='l', 
     xlab= "Size of Simulation", 
     ylab="Deviation between estimate and actual",
     main= expression(beta[1]),
     col=rcols[1],
     lwd=3,
     ylim=yrange)
lines(x=res_b1$N_sim, y=res_b1$JAGS_M2_Dev, col=rcols[2], lwd=3)
lines(x=res_b1$N_sim, y=res_b1$INLA_M1_Dev, col=rcols[3], lwd=3)
lines(x=res_b1$N_sim, y=res_b1$INLA_M2_Dev, col=rcols[4], lwd=3)
legend('topright', legend=c('JAGS M1', 'JAGS M2','INLA M1', 'INLA M2'),
       col=c(rcols[1], rcols[2],rcols[3], rcols[4]), lty=c(rep(1,4), 2, 3), lwd=rep(2, 2))



#TRK b2

jags_m1b2=as.numeric(jags_m1b2)
res_b2_jagsm1=matrix(nrow = k_sim, ncol = 1, NA)

jags_m2b2=as.numeric(jags_m2b2)
res_b2_jagsm2=matrix(nrow = k_sim, ncol = 1, NA)

inla_m1b2=as.numeric(inla_m1b2)
res_b2_inlam1=matrix(nrow = k_sim, ncol = 1, NA)

inla_m1b2=as.numeric(inla_m1b2)
res_b2_inlam1=matrix(nrow = k_sim, ncol = 1, NA)

inla_m2b2=as.numeric(inla_m2b2)
res_b2_inlam2=matrix(nrow = k_sim, ncol = 1, NA)

for (k in 1:k_sim) {
  res_b2_jagsm1[k]=as.numeric(abs(b2-jags_m1b2[k]))
  res_b2_jagsm2[k]=as.numeric(abs(b2-jags_m2b2[k]))
  res_b2_inlam1[k]=as.numeric(abs(b2-inla_m1b2[k]))
  res_b2_inlam2[k]=as.numeric(abs(b2-inla_m2b2[k]))
}
res_b2=cbind(n_simtrk,res_b2_jagsm1,res_b2_jagsm2,res_b2_inlam1,res_b2_inlam2)
res_b2=as.data.frame(res_b2)
cn=c("N_sim","JAGS_M1_Dev", "JAGS_M2_Dev", "INLA_M1_Dev", "INLA_M2_Dev")
colnames(res_b2)=cn

yrange=c(0,0.12)
plot(x=res_b2$N_sim, y=res_b2$JAGS_M1_Dev, type='l', 
     xlab= "Size of Simulation", 
     ylab="Deviation between estimate and actual",
     main= expression(beta[2]),
     col=rcols[1],
     lwd=3,
     ylim=yrange)
lines(x=res_b2$N_sim, y=res_b2$JAGS_M2_Dev, col=rcols[2], lwd=3)
lines(x=res_b2$N_sim, y=res_b2$INLA_M1_Dev, col=rcols[3], lwd=3)
lines(x=res_b2$N_sim, y=res_b2$INLA_M2_Dev, col=rcols[4], lwd=3)
legend('topright', legend=c('JAGS M1', 'JAGS M2','INLA M1', 'INLA M2'),
       col=c(rcols[1], rcols[2],rcols[3], rcols[4]), lty=c(rep(1,4), 2, 3), lwd=rep(2, 2))



#TRK alpha

jags_m1alpha=as.numeric(jags_m1alpha)
res_alpha_jagsm1=matrix(nrow = k_sim, ncol = 1, NA)

jags_m2alpha=as.numeric(jags_m2alpha)
res_alpha_jagsm2=matrix(nrow = k_sim, ncol = 1, NA)

inla_m1alpha=as.numeric(inla_m1alpha)
res_alpha_inlam1=matrix(nrow = k_sim, ncol = 1, NA)

inla_m1alpha=as.numeric(inla_m1alpha)
res_alpha_inlam1=matrix(nrow = k_sim, ncol = 1, NA)

inla_m2alpha=as.numeric(inla_m2alpha)
res_alpha_inlam2=matrix(nrow = k_sim, ncol = 1, NA)

for (k in 1:k_sim) {
  res_alpha_jagsm1[k]=as.numeric(abs(alpha-jags_m1alpha[k]))
  res_alpha_jagsm2[k]=as.numeric(abs(alpha-jags_m2alpha[k]))
  res_alpha_inlam1[k]=as.numeric(abs(alpha-inla_m1alpha[k]))
  res_alpha_inlam2[k]=as.numeric(abs(alpha-inla_m2alpha[k]))
}
res_alpha=cbind(n_simtrk,res_alpha_jagsm1,res_alpha_jagsm2,res_alpha_inlam1,res_alpha_inlam2)
res_alpha=as.data.frame(res_alpha)
cn=c("N_sim","JAGS_M1_Dev", "JAGS_M2_Dev", "INLA_M1_Dev", "INLA_M2_Dev")
colnames(res_alpha)=cn

yrange=c(0,0.40)
plot(x=res_alpha$N_sim, y=res_alpha$JAGS_M1_Dev, type='l', 
     xlab= "Size of Simulation", 
     ylab="Deviation between estimate and actual",
     main= expression(alpha),
     col=rcols[1],
     lwd=3,
     ylim=yrange)
lines(x=res_alpha$N_sim, y=res_alpha$JAGS_M2_Dev, col=rcols[2], lwd=3)
lines(x=res_alpha$N_sim, y=res_alpha$INLA_M1_Dev, col=rcols[3], lwd=3)
lines(x=res_alpha$N_sim, y=res_alpha$INLA_M2_Dev, col=rcols[4], lwd=3)
legend('topright', legend=c('JAGS M1', 'JAGS M2','INLA M1', 'INLA M2'),
       col=c(rcols[1], rcols[2],rcols[3], rcols[4]), lty=c(rep(1,4), 2, 3), lwd=rep(2, 2))



##################################
#WORKING WITH RANDOM EFFECT
#################################




##########JAGS M2
# IID RANDOM EFFECT


r1_b0_n1=rbind(jags_m2r1b0_1[1],jags_m2r1b0_2[1],jags_m2r1b0_3[1],
               jags_m2r1b0_4[1],jags_m2r1b0_5[1])
r1_b0_n2=rbind(jags_m2r1b0_1[2],jags_m2r1b0_2[2],jags_m2r1b0_3[2],
               jags_m2r1b0_4[2],jags_m2r1b0_5[2])
r1_b0_n3=rbind(jags_m2r1b0_1[3],jags_m2r1b0_2[3],jags_m2r1b0_3[3],
               jags_m2r1b0_4[3],jags_m2r1b0_5[3])
r1_b0_n4=rbind(jags_m2r1b0_1[4],jags_m2r1b0_2[4],jags_m2r1b0_3[4],
               jags_m2r1b0_4[4],jags_m2r1b0_5[4])
r1_b0_n5=rbind(jags_m2r1b0_1[5],jags_m2r1b0_2[5],jags_m2r1b0_3[5],
               jags_m2r1b0_4[5],jags_m2r1b0_5[5])

n_clust=1:r1.cluster.n
res_rand=cbind(n_clust,r1_b0_n1,r1_b0_n2,r1_b0_n3,r1_b0_n4,r1_b0_n5)
cn=c("n_clust","r1_b0_n1", "r1_b0_n2", "r1_b0_n3", "r1_b0_n4","r1_b0_n5")
colnames(res_rand)=cn
res_rand=as.data.frame(res_rand)

yrange=c(-0.05,0.05)
plot(x=res_rand$n_clust, y=res_rand$r1_b0_n1, type='l', 
     xlab= "Clusters", 
     ylab="Random Effect",
     main="JAGS M2 Results for Random Intercept",
     col=rcols[1],
     lwd=3,
     ylim=yrange)
lines(x=res_rand$n_clust, y=res_rand$r1_b0_n2, col=rcols[2], lwd=3)
lines(x=res_rand$n_clust, y=res_rand$r1_b0_n3, col=rcols[3], lwd=3)
lines(x=res_rand$n_clust, y=res_rand$r1_b0_n4, col=rcols[4], lwd=3)
lines(x=res_rand$n_clust, y=res_rand$r1_b0_n5, col=rcols[5], lwd=3)
legend('topright', c(paste('simulation_', nam_sim, sep='')),
       lty=c(rep(1,k_sim), 2, 3), lwd=rep(2, k_sim), col=c(rcols,3,3), bty='n')

plot(x=res_rand$n_clust, y=res_rand$r1_b0_n3, type='l', 
     xlab= "Clusters", 
     ylab="Random Effect",
     main="JAGS M2 Results for Random Intercept simulation_375",
     col=rcols[3],
     lwd=3)

plot(x=res_rand$n_clust, y=res_rand$r1_b0_n4, type='l', 
     xlab= "Clusters", 
     ylab="Random Effect",
     main="JAGS M2 Results for Random Intercept simulation_500",
     col=rcols[4],
     lwd=3)



##########JAGS TAU
jags_tau=matrix(nrow = k_sim, ncol = 1,0)

for (k in 1:k_sim) {
  jags_int=jags_m2res[[k]]
  jags_tau[k]=round(jags_int$summaries[5,6]**-2,4)
}

jags_tau

###########INLA M2
# IID RANDOM EFFECT
yrange=c(-0.005,0.005)
plot(x=inla_m2res[[1]]$summary.random$r1.cluster.i[,1],
     y=inla_m2res[[1]]$summary.random$r1.cluster.i[,2], type = "l",
     main= "INLA M2 Results for Random Intercept",
     xlab= "Clusters",
     ylab= "Random Effect",
     lwd=3,
     ylim=yrange) 
for (k in 1:k_sim) 
  lines(x=inla_m2res[[k]]$summary.random$r1.cluster.i[,1],
        y=inla_m2res[[k]]$summary.random$r1.cluster.i[,2], 
        col=rcols[k], lwd=3)
legend('topright', c(paste('simulation_', nam_sim, sep='')),
       lty=c(rep(1,k_sim), 2, 3), lwd=rep(2, k_sim), col=c(rcols,3,3), bty='n')

###########INLA TAU
inla_tau=matrix(nrow = k_sim, ncol = 1,0)
k=1

for (k in 1:k_sim) {
  inla_int=inla_m2res[[k]]
  inla_tau[k]=round(inla_int$summary.hyperpar[2,6],4)
}
inla_tau
########## JAGS PSRF M1
n_parm=length(jags_m1psrf[[1]])
psrf_res_m1=matrix(ncol=n_parm,nrow=k_sim,0)

for (k in 1:k_sim) {
  psrf_res_m1[k,]=jags_m1psrf[[k]]
}
parm_names=row.names(jags_m1res[[1]]$summaries)
psrf_res_m1=as.data.frame(psrf_res_m1)
names(psrf_res_m1)=parm_names

########## JAGS PSRF M2
n_parm=length(jags_m2psrf[[1]])
psrf_res_m2=matrix(ncol=n_parm,nrow=k_sim,0)

for (k in 1:k_sim) {
  psrf_res_m2[k,]=jags_m2psrf[[k]]
}
parm_names=row.names(jags_m2res[[1]]$summaries)
psrf_res_m2=as.data.frame(psrf_res_m2)
names(psrf_res_m2)=parm_names
