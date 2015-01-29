
library(R2jags)

modelstr <- "
model{
    # likelihood
    k ~ dbinom(theta, n)
    
    # prior
    theta ~ dbeta(1, 1)
}"

jagsdata <- list(k = 9, n = 10)

inits <- function() {
    list(theta = rbeta(1, 1, 1))
}

parameters = "theta"

fit <- jags(data = jagsdata, model.file = textConnection(modelstr), 
             inits = inits, n.chains = 2,
             n.iter = 2000, n.thin = 2,n.burnin = 100,
             parameters.to.save = parameters)

print(fit)

plot(fit)

traceplot(fit, varname = "theta")

library(mcmcplots)

denplot(fit, parms = "theta")

samples <- fit$BUGSoutput$sims.list
theta <- samples$theta

dim(theta)

mean(theta)

sd(theta)

quantile(theta, c(0.025, 0.975))

hist(theta)


