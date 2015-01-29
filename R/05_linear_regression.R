
N <- 1000 # number of observations

x <- 1:N # predictor variable

a <- 5 # intercept
b <- 1.5 # slope

sigma <- 5 #sampling noise

y_hat <- a + b*x # linear predictor

y <- rnorm(N, y_hat, sigma)  # response variable

plot(rnorm(100,y_hat))

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
