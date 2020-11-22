load("dataex4.Rdata")
# The function for implementing the EM algorithm
em_algorithm = function(data, beta0, eps = 10^(-6)){
  # Initialize some variables
  X = data$X
  X1 = data$X[!is.na(data$Y)]
  X2 = data$X[is.na(data$Y)]
  Y_obs = data$Y[!is.na(data$Y)]
  beta = beta0
  diff = 1
  # The parameter p_i(beta) of the Bernoulli distribution for Y_i
  p_beta = function(x, beta){
    exp(beta[1] + x * beta[2])/(1 + exp(beta[1] + x * beta[2]))
  }
  # The E-step
  Q_beta = function(beta, beta_t){
    sum(Y_obs * (beta[1] + X1 * beta[2])) +
    sum((beta[1] + X2 * beta[2]) * p_beta(X2, beta_t)) -
    sum(log(1 + exp(beta[1] + X * beta[2])))
  }
  # The gradient of the function Q(beta | beta_t)
  gradient = function(beta, beta_t){
    partial1 = sum(Y_obs) + sum(p_beta(X2, beta_t)) - sum(p_beta(X, beta))
    partial2 = sum(X1 * Y_obs) + sum(X2 * p_beta(X2, beta_t)) - sum(X * p_beta(X, beta))
    c(partial1, partial2)
  }
  while (diff >= eps){
    beta_old = beta
    # The M-step
    # The gradient of Q(beta | beta_t) is further supplied to improve accuracy
    optimum = optim(par = beta, fn = Q_beta, gr = gradient, beta_t = beta,
                    control = list(fnscale = -1), hessian = FALSE)
    beta = optimum$par
    diff = sum(abs(beta - beta_old))
  }
  return(beta)
}

# The final result
beta = em_algorithm(data = dataex4, beta0 = c(1, -1))
beta0 = beta[1]; beta1 = beta[2]
beta0; beta1
