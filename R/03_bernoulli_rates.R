
library(R2jags)
library(ggplot2)

true_theta <- 0.6

coin_flips <- rbinom(n = 100, size = 1, prob = true_theta)

head(y, 20)

modelstr <- "
model{
  for (i in 1:length(y)) { 
    y[i] ~ dbern(theta)
    }
    theta ~ dbeta(1, 1)
}"

jagsdata <- list(y = coin_flips)

inits <- function() {
    list(theta = rbeta(1, 1, 1))
    }

parameters <- "theta"

fit <- jags(data = jagsdata, model.file = textConnection(modelstr), 
             inits = inits, n.chains = 2,
             n.iter = 2000, n.thin = 2,n.burnin = 100,
             parameters.to.save = parameters)

fit

traceplot(fit, varname = "theta")

samples <- fit$BUGSoutput$sims.list
theta <- samples$theta
head(theta)

hist(theta)
abline(v = true_theta, col = "red", lty = 2, lwd = 4)

true_ability <- 0.75

n_questions <- 10

answers <- rbinom(n = 20, size = n_questions, prob = true_ability)

modelstr <- "
model{
  for (i in 1:n_obs) { 
    y[i] ~ dbinom(theta, n)
    }
    theta ~ dbeta(1, 1)
}"

jagsdata <- list(y = answers, n = n_questions, n_obs = length(answers))

inits <- function() {
    list(theta = rbeta(1, 1, 1))
    }

parameters <- "theta"

fit <- jags(data = jagsdata, model.file = textConnection(modelstr), 
             inits = inits, n.chains = 2,
             n.iter = 2000, n.thin = 2,n.burnin = 100,
             parameters.to.save = parameters)

print(fit)

plot(fit)

traceplot(fit, varname = "theta")

library(mcmcplots)
denplot(fit, parms = "theta")

group1_ability <- 0.8
group2_ability <- 0.5

n_questions <- 10

group1_answers <- rbinom(n = 20, size = n_questions, prob = group1_ability)
group2_answers <- rbinom(n = 20, size = n_questions, prob = group2_ability)

library(ggplot2)
library(tidyr)
library(dplyr)

df <- data.frame(group1 = group1_answers, group2 = group2_answers)
df <- df %>% gather(group, answers, one_of(c("group1", "group2")))
df %>% head(10)

p <- ggplot(df, aes(x = group, y = answers, fill = group)) + theme_bw()
p + geom_boxplot()

modelstr <- "
model{
  for (i in 1:n_obs) { 

    # nested indexing
    y[i] ~ dbinom(theta[group[i]], n)
    }
  for (j in 1:n_groups) {
    theta[j] ~ dbeta(1, 1)
    }
    
  delta <- theta[1] - theta[2]
}"

df$group_idx <- match(df$group, unique(df$group))
df %>% head()
df %>% tail()

jagsdata <- list(y = df$answers, 
                 n = n_questions, 
                 n_obs = length(df$answers),
                 group = df$group_idx,
                 n_groups = length(unique(df$group)))

inits <- function() {
    list(theta = rbeta(jagsdata$n_groups, 1, 1))
    }

parameters <- c("theta", "delta")

fit <- jags(data = jagsdata, model.file = textConnection(modelstr), 
             inits = inits, n.chains = 2,
             n.iter = 2000, n.thin = 2,n.burnin = 100,
             parameters.to.save = parameters)

print(fit)

denplot(fit, parms = c("theta", "delta"))

samples <- fit$BUGSoutput$sims.list
theta <- as.data.frame(samples$theta)
colnames(theta) <- c("group1", "group2")
head(theta)

theta <- theta %>% gather(group, theta, one_of(c("group1", "group2")))

theta_post <- ggplot(theta, aes(x = theta,
                                fill = group)) + theme_bw()
theta_post + geom_density() +
             geom_vline(xintercept = group1_ability, size = 2) +
             geom_vline(xintercept = group2_ability)

delta <- as.data.frame(samples$delta)
colnames(delta) <- "delta"

q <- quantile(delta$delta, c(0.05, 0.975))
delta_mean <- colMeans(delta)
delta_mean

delta_post <- ggplot(delta, aes(x = delta)) + theme_bw()
delta_post + geom_density() +
             geom_vline(xintercept = q[1], color = "grey75") +
             geom_vline(xintercept = q[2], color = "grey75") + 
             geom_vline(xintercept = delta_mean, color = "firebrick")
