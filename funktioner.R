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

NR <- function(theta0, niter, y, X) {
  i <- 1  # Initialize iteration counter
  while (i <= niter) {
    score <- S(theta0, y, X)  # Compute the score function
    fisher_info <- I(theta0, y, X)  # Compute the Fisher Information matrix
    theta_new <- theta0 + solve(fisher_info) %*% score  # Newton-Raphson update
    theta0 <- theta_new  # Update theta
    std <- sqrt(diag(solve(fisher_info)))  # Standard errors
    i <- i + 1  # Update iteration counter
  }
  return(list(theta_new,std))
}


parametric_bootstrap <- function(model, data, num_iterations = 1000) {
  fitted_probabilities <- fitted(modell)
  bootstrap_coefficients <- matrix(NA, nrow = num_iterations, ncol = length(coef(modell)))
  for (i in 1:num_iterations) {
      new_resultat <- rbinom(length(fitted_probabilities), size = 1, prob = fitted_probabilities)

      bootstrap_model <- glm(new_resultat ~ Alder + Kon + Utbildare, data = data,family = "binomial")
      bootstrap_coefficients[i, ] <- coef(bootstrap_model)
      }
  bootstrap_se <- apply(bootstrap_coefficients, 2, sd)
  return(bootstrap_se)
}

prob_ci <- function(model, data, age, num_iterations=1000){
  fitted_probabilities <- fitted(model)
  bootstrap_probabilities <- matrix(NA, nrow = num_iterations, ncol = length(age))
  for (i in 1:num_iterations) {
    new_resultat <- rbinom(length(fitted_probabilities), size = 1, prob = fitted_probabilities)
    bootstrap_model <- glm(new_resultat ~ Alder + Kon + Utbildare, data = data, family = "binomial")
    bootstrap_probabilities[i, ] <- predict(bootstrap_model, newdata = data.frame(Alder = age, Kon = "Man", Utbildare = "Privatist"), type = "response")
  }
  lower_ci <- apply(bootstrap_probabilities, 2, quantile, probs = 0.025)
  upper_ci <- apply(bootstrap_probabilities, 2, quantile, probs = 0.975)
  return(list(lower_ci, upper_ci))
}
                

