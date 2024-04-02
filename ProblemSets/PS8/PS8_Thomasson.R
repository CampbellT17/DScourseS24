# PS8

library(nloptr)
library(modelsummary)

# Set seed
set.seed(100)

# Parameters
N <- 100000
K <- 10
sigma <- 0.5
eps <- rnorm(N, mean = 0, sd = sigma)
beta <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)

# Generate X matrix
X <- matrix(1, N, K)
X[, -1] <- matrix(rnorm(N * (K - 1)), N, K - 1)

# Generate Y
Y <- X %*% beta + eps

# Compute OLS estimate
beta_hat_OLS <- solve(crossprod(X)) %*% crossprod(X,Y)

# Display OLS estimate
print("OLS Estimate:")
print(beta_hat_OLS)

# Compute OLS estimate using gradient descent
alpha <- 0.0000003
gradient <- function(beta, Y, X) {
  grad <- numeric(length(beta))
  grad <- -t(X) %*% (Y - X %*% beta)
  return(grad)
}

beta_hat_OLS_gradient <- rep(0, K)
alpha <- 0.0000003
for (i in 1:100) {
  beta_hat_OLS_gradient <- beta_hat_OLS_gradient - alpha * gradient(beta_hat_OLS_gradient, Y, X)
}

# Display OLS estimate using gradient descent
print("OLS Estimate using Gradient Descent:")
print(beta_hat_OLS_gradient)

# Compute OLS estimate using nloptr's L-BFGS algorithm and Nelder-Mead algorithm

# L-BFGS

## Our objective function
objfun <- function(beta, Y, X) {
  return (sum((Y - X %*% beta)^2))
}

## Gradient of our objective function
gradient <- function(beta, Y, X) {
  return (as.vector(-2 * t(X) %*% (Y - X %*% beta)))
}

## Initial values
beta0 <- runif(K)

## Algorithm parameters
options_OLS_lbfgs <- list("algorithm" = "NLOPT_LD_LBFGS", "xtol_rel" = 1.0e-6, "maxeval" = 100)

## Optimize using L-BFGS
beta_hat_OLS_lbfgs <- nloptr(x0 = beta0, eval_f = objfun, eval_grad_f = gradient, opts = options_OLS_lbfgs, Y = Y, X = X)

## Display L-BFGS result
print("OLS Estimate using L-BFGS algorithm:")
print(beta_hat_OLS_lbfgs)

# Nelder-Mead

## Our objective function
objfun <- function(beta, Y, X) {
  return (sum((Y - X %*% beta)^2))
}

## Initial values
beta0 <- runif(K)

## Algorithm parameters
options_nm <- list("algorithm" = "NLOPT_LN_NELDERMEAD", "xtol_rel" = 1.0e-6, "maxeval" = 100)

## Optimize using Nelder-Mead
beta_hat_OLS_nm <- nloptr(x0 = beta0, eval_f = objfun, opts = options_nm, Y = Y, X = X)

## Display Nelder-Mead result
print("OLS Estimate using Nelder-Mead algorithm:")
print(beta_hat_OLS_nm)

# Compute MLE estimate using nloptr's L-BFGS algorithm

## Our objective function
objfun  <- function(theta,Y,X) {
  # need to slice our parameter vector into beta and sigma components
  beta    <- theta[1:(length(theta)-1)]
  sig     <- theta[length(theta)]
  # write objective function as *negative* log likelihood (since NLOPT minimizes)
  loglike <- -sum( -.5*(log(2*pi*(sig^2)) + ((Y-X%*%beta)/sig)^2) ) 
  return (loglike)
}

## Gradient of the objective function
gradient <- function(theta,Y,X) {
  grad <- as.vector(rep (0,length(theta)))
  beta <- theta[1:(length(theta)-1)]
  sig <- theta[length(theta)]
  grad[1:(length(theta)-1)] <- -t(X)%*%(Y - X%*%beta)/(sig^2)
  grad[length(theta)] <- dim(X)[1] /sig - crossprod(Y-X%*%beta)/(sig^3)
  return (grad)
}

## initial values
theta0 <- append(runif(ncol(X)), runif(1))

## Algorithm parameters
options_MLE_lbfgs <- list("algorithm" = "NLOPT_LD_LBFGS", "xtol_rel" = 1.0e-6, "maxeval" = 100)

## Optimize!
beta_hat_MLE_lbfgs <- nloptr(x0=theta0,eval_f=objfun,eval_grad_f=gradient,opts=options_MLE_lbfgs,Y=Y,X=X)

## Display L-BFGS result
print("MLE Estimate using L-BFGS algorithm:")
print(beta_hat_MLE_lbfgs)

# Compute OLS estimate using lm() without intercept
beta_hat_OLS_Linear <- lm(Y ~ X -1)

# Export regression output to a .tex file using modelsummary
modelsummary(beta_hat_OLS_Linear, output = "latex", file = "ols_regression_output.tex")