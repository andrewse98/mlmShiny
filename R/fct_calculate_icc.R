#' Calculate Theoretical ICC
#' @description Computes ICC based on input standard deviations
#' @export
calculate_theoretical_icc <- function(sd_u, sd_e) {
  var_u <- sd_u^2
  var_e <- sd_e^2
  icc <- var_u / (var_u + var_e)
  return(round(icc, 3))
}
