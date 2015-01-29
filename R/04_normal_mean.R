
# this makes sure we get the same random numbers every time
set.seed(34522)

n <- 20
y <- round(rnorm(n = n, mean = 120, sd = 15))
head(y)

library(ggplot2)
qplot(y, geom = "histogram", binwidth = 10)

library(dplyr)
library(tidyr)
library(R2jags)
library(ggmcmc)

modelstr <- "
model{
  for (i in 1:n_obs) { 
    y[i] ~ dnorm(mu, tau)
    }

    mu ~ dnorm(0, 1e-03)
    sigma <- 15
    tau <- pow(sigma, -2) # 1/(sigma*sigma)
}"

jagsdata <- list(y = y,
                n_obs = length(y))

inits <- function() {
    list(mu = rnorm(1, 1, 1))
    }

parameters = "mu"

fit <- jags(data = jagsdata, model.file = textConnection(modelstr), 
             inits = inits, n.chains = 2,
             n.iter = 2000, n.thin = 2,n.burnin = 100,
             parameters.to.save = parameters)

print(fit)

modelstr <- "
model{
  for (i in 1:n_obs) { 
    y[i] ~ dnorm(mu, tau)
    }

    mu ~ dnorm(0, 1e-03)
    sigma ~ dunif(0, 100) # sigma must be > 0
    tau <- pow(sigma, -2) # 1/(sigma*sigma)
}"

jagsdata <- list(y = y,
                n_obs = length(y))

inits <- function() {
    list(mu = rnorm(1, 1, 1),
         sigma = runif(1, 0, 100))
    }

parameters = c("mu", "sigma")

fit <- jags(data = jagsdata, model.file = textConnection(modelstr), 
             inits = inits, n.chains = 2,
             n.iter = 2000, n.thin = 2,n.burnin = 100,
             parameters.to.save = parameters)

print(fit)

true_iq <- 110
sd <- 5
n_subjects <- 10
person_iq <- round(rnorm(n_subjects, mean = true_iq, sd = sd))
person_iq

measure_iq <- function(x, sd = 2, n_rep = 3) {
    round(rnorm(n_rep, x, sd))
}

df <- data.frame(sapply(X = person_iq, FUN = measure_iq , n_rep = 3))
colnames(df) <- c(paste("subj", 1:n_subjects, sep = ""))
df <- df %>% gather(subject, iq)
df$subj_idx <- match(df$subject, unique(df$subject))
df %>% head

modelstr <- "
model{
    for (i in 1:n_obs) { 
      y[i] ~ dnorm(mu[subject[i]], tau)
    }
    
    for (k in 1:n_subjects) { 
      mu[k] ~ dnorm(100, 1e-02)
    }
    
    sigma ~ dunif(0, 10) # sigma must be > 0
    tau <- pow(sigma, -2) # 1/(sigma*sigma)
}"

jagsdata <- list(y = df$iq,
                 subject = df$subj_idx,
                 n_subjects = length(unique(df$subj_idx)),
                 n_obs = length(df$iq))

inits <- function() {
    list(mu = rnorm(jagsdata$n_subjects, 1, 1),
         sigma = runif(1, 0, 10))
    }

parameters = c("mu", "sigma")

fit <- jags(data = jagsdata, model.file = textConnection(modelstr), 
             inits = inits, n.chains = 2,
             n.iter = 2000, n.thin = 2,n.burnin = 100,
             parameters.to.save = parameters)

print(fit)

modelstr <- "
model{
    for (i in 1:n_obs) { 
      y[i] ~ dnorm(mu[subject[i]], tau)
    }
    
    for (k in 1:n_subjects) { 
      mu[k] ~ dnorm(group_mu, group_tau)
    }
    
    group_mu ~ dnorm(100, 1e-02)
    group_sigma ~ dunif(0, 10)
    group_tau <- pow(group_sigma, -2)

    sigma ~ dunif(0, 100) # sigma must be > 0
    tau <- pow(sigma, -2) # 1/(sigma*sigma)
}"

jagsdata <- list(y = df$iq,
                 subject = df$subj_idx,
                 n_subjects = length(unique(df$subj_idx)),
                 n_obs = length(df$iq))

inits <- function() {
    list(group_mu = rnorm(1, 100, 15),
         group_sigma = runif(1, 0, 10),
         sigma = runif(1, 0, 100))
}

parameters = c("group_mu", "group_sigma", "mu", "sigma")

fit <- jags(data = jagsdata, model.file = textConnection(modelstr), 
             inits = inits, n.chains = 2,
             n.iter = 2000, n.thin = 2,n.burnin = 100,
             parameters.to.save = parameters)

print(fit)

ggs_caterpillar(ggs(as.mcmc(fit)), family = "mu")
