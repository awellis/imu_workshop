
N <- 1000 # number of observations

x <- seq(from = -2, to = 2, length.out = N) # predictor variable

a <- -1 # intercept
b <- 0.8 # slope

sigma <- 1.2 #sampling noise

y_hat <- a + b*x # linear predictor

y <- rnorm(N, y_hat, sigma)  # response variable

library(ggplot2)
qplot(x, y, geom = "line")

modelstr <- "
model {
    for (i in 1:N){
        y[i] ~ dnorm(y_hat[i], tau)
        y_hat[i] <- a + b * x[i]
    }
    
    # priors on the coefficients
    a ~ dnorm(0, 1e-03)
    b ~ dnorm(0, 1e-03)

    # prior residual standard deviation
    tau <- pow(sigma, -2)
    sigma ~ dunif(0, 100)
}"

library(R2jags)

jagsdata <- list(y = y,
                 x = x,
                 N = length(y))

inits <- function() {
    list(a = rnorm(1, 0, 10),
         b = rnorm(1, 0, 10),
         sigma = runif(1, 0, 100))
    }

parameters = c("a", "b", "sigma")

fit <- jags(data = jagsdata, model.file = textConnection(modelstr), 
             inits = inits, n.chains = 2,
             n.iter = 2000, n.thin = 2,n.burnin = 100,
             parameters.to.save = parameters)

print(fit)

library(mcmcplots)
caterplot(fit, parms = c("a", "b", "sigma"))

set.seed(3425)

N <- 1000 # number of observations

x <- seq(from = -2, to = 2, length.out = N) # predictor variable

a <- -1 # intercept
b <- 0.8 # slope

z_hat <- plogis(a + b*x) # linear predictor on the logit scale

z <- rbinom(N, prob = z_hat, size = 1)  # response variable

library(ggplot2)
qplot(x, z, geom = "point")

modelstr <- "
model {
  for (i in 1:N){
    z[i] ~ dbern(z_hat[i])
    logit(z_hat[i]) <- a + b * x[i]
}

  # priors on the coefficients
  a ~ dnorm(0, 1e-03)
  b ~ dnorm(0, 1e-03)
}"

library(R2jags)

jagsdata <- list(z = z,
                 x = x,
                 N = length(z))

inits <- function() {
    list(a = rnorm(1, 0, 10),
         b = rnorm(1, 0, 10))
}

parameters = c("a", "b")

fit <- jags(data = jagsdata, model.file = textConnection(modelstr),
            inits = inits, n.chains = 2,
            n.iter = 2000, n.thin = 2,n.burnin = 100,
            parameters.to.save = parameters)

print(fit)

library(mcmcplots)
caterplot(fit, parms = c("a", "b", "sigma"))
