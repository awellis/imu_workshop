
library(dplyr)
library(R2jags)

data(chickwts)

## Restrict to two groups
chickwts <- chickwts %>% filter(feed %in% c("horsebean", "linseed"))

## Drop unused factor levels
chickwts$feed = factor(chickwts$feed)


chickwts %>% group_by(feed) %>%
    summarise(mean = mean(weight),
              sd = sd(weight))

## Plot data
plot(weight ~ feed, data = chickwts, main = "Chick weights")

## traditional t test
t.test(weight ~ feed, data = chickwts, var.eq=TRUE)

## create index variable for Jags
chickwts$feed_ix <- match(chickwts$feed, unique(chickwts$feed))
head(chickwts)

modelstr <- "
model {
 for (i in 1:n_obs) {
    y[i] ~ dnorm(mu[i], tau)
    mu[i] <- beta[feed[i]]
  }
  for (j in 1:2) {
  beta[j] ~ dnorm(0, 1e-03)
  }

  tau <- pow(sigma, -2)
  sigma ~ dunif(0, 60)

  # contrast
  difference <- beta[1] - beta[2]
}"

jagsdata <- list(y = chickwts$weight,
                 feed = chickwts$feed_ix,
                 n_obs = length(chickwts$weight))

inits <- function() {
    list(beta = rnorm(2, 0, 100))
}

parameters = c("beta", "difference", "sigma")

fit <- jags(data = jagsdata, model.file = textConnection(modelstr),
            inits = inits, n.chains = 2,
            n.iter = 2000, n.thin = 2,n.burnin = 100,
            parameters.to.save = parameters)
print(fit)
