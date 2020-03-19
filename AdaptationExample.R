require(adaptMCMC)
require(coda)
set.seed(255)
## --------------example adapted from manual of adaptMCMC--------
NumberOfIterations<-5000

## Banana shaped distribution
## log-pdf to sample from
p.log <- function(x) {
  B <- 0.03 # controls 'bananacity'
  -x[1]^2/200 - 1/2*(x[2]+B*x[1]^2-100*B)^2
}
## ----------------------
## generate samples
## 1) non-adaptive sampling
# Note that the function MCMC is part of the adaptive MCMC package, and is designed to do adaptive MCMC
samp.1a <- MCMC(p.log, n=NumberOfIterations, init=c(0, 1), scale=c(1, 0.1),
               adapt=FALSE)
samp.1b <- MCMC(p.log, n=NumberOfIterations, init=c(0, 1), scale=c(1, 0.1),
               adapt=FALSE)
## 2) adaptive sampling
samp.2a <- MCMC(p.log, n=NumberOfIterations, init=c(0, 1), scale=c(1, 0.1),
               adapt=TRUE, acc.rate=0.234)
samp.2b <- MCMC(p.log, n=NumberOfIterations, init=c(0, 1), scale=c(1, 0.1),
               adapt=TRUE, acc.rate=0.234)
## ----------------------
## ----------------------
## plot density and samples
x1 <- seq(-15, 15, length=80)
x2 <- seq(-15, 15, length=80)
d.banana <- matrix(apply(expand.grid(x1, x2), 1, p.log), nrow=80)
par(mfrow=c(1,2))
image(x1, x2, exp(d.banana), col=cm.colors(60), asp=1, main="no adaption")
contour(x1, x2, exp(d.banana), add=TRUE, col=gray(0.6))
lines(samp.1a$samples[1:200,], type='b', pch=3)
image(x1, x2, exp(d.banana), col=cm.colors(60), asp=1, main="with adaption")
contour(x1, x2, exp(d.banana), add=TRUE, col=gray(0.6))
lines(samp.2a$samples[1:200,], type='b', pch=3)
## ----------------------


## Coda diagnostics
## convert in object of class 'mcmc'
samp1a.coda <- convert.to.coda(samp.1a)
samp1b.coda <- convert.to.coda(samp.1b)
bothsamps1<-mcmc.list(samp1a.coda,samp1b.coda)
samp2a.coda <- convert.to.coda(samp.2a)
samp2b.coda <- convert.to.coda(samp.2b)
bothsamps2<-mcmc.list(samp2a.coda,samp2b.coda)

## ----------------------
## use functions of package 'coda'
gelman.plot(bothsamps1,main="No adaptation")
gelman.plot(bothsamps2,main="Adaptation")
print(gelman.diag(bothsamps1))
print(gelman.diag(bothsamps2))
