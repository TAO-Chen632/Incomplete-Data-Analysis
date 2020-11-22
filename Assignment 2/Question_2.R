library(maxLik)
load("dataex2.Rdata")
sigma = 1.5
# The log-likelihood function of the observed data
log_likelihood = function(mu, data){
  x = data[[1]]
  r = data[[2]]
  non_censored = r * log(dnorm(x, mean = mu, sd = sigma))
  left_censored = (1 - r) * log(pnorm(x, mean = mu, sd = sigma))
  sum(non_censored + left_censored)
}
# Utilize the `maxLik()` function to maximize the log-likelihood
maxLik(logLik = log_likelihood, data = dataex2, start = 5)
mle = maxLik(logLik = log_likelihood, data = dataex2, start = 5)
mle$estimate
