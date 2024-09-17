# Logistic regression functions

# Likelihood function L
L <- function(theta, y, X) {
  p <- 1 / (1 + exp(-X %*% theta))  # Compute the probability p
  likelihood <- prod(dbinom(y, size = 1, prob = p))  # Product of each Bernoulli trial
  return(likelihood)
}

# Log likelihood function l
l <- function(theta, y, X) {
  likelihood <- L(theta, y, X)  # Compute the likelihood
  log_likelihood <- log(likelihood)  #Log-likelihood: l(θ) = log(L(θ))
  return(log_likelihood)
}

# Score function S
S <- function(theta, y, X) {
  p <- 1 / (1 + exp(-X %*% theta))  # Compute the probability p
  score <- t(X) %*% (y - p)  # Score function: S(θ) = X^T (y - p)
  return(score)
}

# Fisher Information matrix I
I <- function(theta, y, X) {
  p <- 1 / (1 + exp(-X %*% theta))  # Compute the probability p
  v <- as.vector(p * (1 - p))  # Variance v_i = p_i (1 - p_i)
  D <- diag(v)  # Diagonal matrix D with v on the diagonal
  fisher_info <- t(X) %*% D %*% X  # Fisher Information: I(θ) = X^T D X
  return(fisher_info)
}
