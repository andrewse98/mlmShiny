#' Simulate Multilevel Data
#' @description A function to generate random intercept data.
#' @export
simulate_mlm_data <- function(
  N = 30,
  C = 50,
  g00 = 2,
  g01 = 5,
  tau2_u0 = input$tau2_00,
  tau2_u1 = input$tau2_11,
  cov_uij.u0j = input$tau2_01,
  sigma2 = input$sigma2
) {
  # Fixed
  g00 <- g00
  g01 <- g01

  # Random Variance-Covariance Matrix
  var_mat <- matrix(c(tau2_u0, cov_uij.u0j, cov_uij.u0j, tau2_u1), 2, 2)
  set.seed(170845)
  u <- MASS::mvrnorm(n = C, mu = c(0, 0), Sigma = var_mat)

  # Random Intercept (level-2)
  u0j <- u[, 1]
  u0j <- rep(x = u0j, each = N)
  b0j <- g00 + u0j

  # Random Slope (level-2)
  u1j <- u[, 2]
  u1j <- rep(x = u1j, each = N)
  b1j <- g01 + u1j

  # Random (level-1)
  set.seed(281028)
  eij <- rnorm(n = N * C, mean = 0, sd = sqrt(sigma2))

  # Predictor (level-1)
  set.seed(2104)
  X1ij <- as.vector(replicate(n = C, expr = rnorm(N, 0, 1)))

  # Outcome
  yij <- b0j + b1j * X1ij + eij

  # Generate dataframe
  ID <- 1:(N * C)
  Cluster <- factor(rep(x = seq(1, C), each = N))

  df <- data.frame(
    y = yij,
    ID = ID,
    Cluster = Cluster,
    eij = eij,
    u0j = u0j,
    g00 = rep(x = g00, times = N * C),
    u1j = u1j,
    g01 = rep(x = g01, times = N * C),
    b0j = b0j,
    b1j = b1j,
    X = X1ij
  )

  return(df)
}


# Function to simulate only random intercept
simulate_mlm_random_intercept <- function(...) {
  df <- simulate_mlm_data(tau2_u1 = 0, cov_uij.u0j = 0, ...)
  return(df)
}
