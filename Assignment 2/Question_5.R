load("dataex5.Rdata")
# The function for implementing the EM algorithm
em.mixture = function(data, theta0, eps = 10^(-6)){
  # Initialize some variables
  Y = data
  n = length(Y)
  theta = theta0
  p = theta[1]
  mu = theta[2]
  sigma = theta[3]
  lambda = theta[4]
  diff = 1
  while(diff >= eps){
    theta_old = theta
    # The E-step
    ptilde1 = p * dlnorm(Y, mean = mu, sd = sigma)
    ptilde2 = (1 - p) * dexp(Y, rate = lambda)
    ptilde = ptilde1 / (ptilde1 + ptilde2)
    # The M-step
    p = mean(ptilde)
    mu = sum(ptilde * log(Y)) / sum(ptilde)
    sigma = sqrt(sum(ptilde * (log(Y) - mu)^2) / sum(ptilde))
    lambda = sum(1 - ptilde) / sum((1 - ptilde) * Y)
    # Update the parameters
    theta = c(p, mu, sigma, lambda)
    diff = sum(abs(theta - theta_old))
  }
  return(theta)
}

# The final result
theta = em.mixture(data = dataex5, theta0 = c(0.1, 1, 0.5, 2))
p = theta[1]; mu = theta[2]; sigma = theta[3]; lambda = theta[4]
p; mu; sigma; lambda

# The histogram of the data with the estimated density superimposed
pdf("Question_5.pdf")
hist(dataex5, main = "The distribution of the random sample Y",
     xlab = "Y", ylab = "Density", xlim = c(0, 100), ylim = c(0, 0.1),
     freq = FALSE, col = "deepskyblue2")
curve(p * dlnorm(y, mean = mu, sd = sigma) + (1 - p) * dexp(y, rate = lambda),
      add = TRUE, xname = "y", lwd = 2, col = "red")
dev.off()
