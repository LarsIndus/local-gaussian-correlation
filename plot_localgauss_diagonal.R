#' Create a line plot of local Gaussian correlation estimates along the diagonal
#'
#' @description
#' This function estimates local Gaussian correlation along the main diagonal
#' of a two-dimensional plane and plots the estimated values in a line plot.
#' Default values are appropriate for standard normal marginal distributions.
#'
#' @param dat Bivariate input data, either a matrix or data frame.
#' @param diag_low Numeric, lower bound for points to be estimated/plotted.
#' @param diag_high Numeric, upper bound for points to be estimated/plotted, i.e.,
#'   plots are created from (diag_low, diag_low) to (diag_high, diag_high).
#' @param step_size Numeric, distance between points for which the local Gaussian
#'   correlation is estimated and plotted.
#' @param b1 Numeric, first bandwidth parameter used by the function localgauss.
#' @param b2 Numeric, second bandwidth parameter used by the function localgauss.
#' @return A ggplot object showing the local Gaussian correlation estimates along the diagonal.
#' 
#' @examples
#' \dontrun{
#' library(localgauss)
#' library(magrittr)
#' library(ggplot2)
#' library(mvtnorm)
#'
#' # Sample from a bivariate t distribution:
#'  
#' n <- 2000
#' df <- 5
#' rho <- 0
#' sigma <- matrix(c(1, rho, rho, 1), nrow = 2)
#' b <- 0.7
#' 
#' x <- rmvt(n, sigma = sigma, df = df)
#' 
#' plot_localgauss_diagonal(x, diag_low = -3, diag_high = 3)
#' }

plot_localgauss_diagonal <- function(dat, diag_low = -4, diag_high = 4, step_size = 0.1,
                                     b1 = 1, b2 = 1) {
    
  # convert to matrix if necessary
  if(!is.matrix(dat)) {
    dat <- as.matrix(dat)
  }
  
  # values for which to calculate LGC
  diag_matrix <- matrix(c(seq(diag_low, diag_high, step_size),
                          seq(diag_low, diag_high, step_size)),
                        ncol = 2)
  
  # estimate the LGC
  lg.out <- localgauss::localgauss(x = dat[, 1], y = dat[, 2], xy.mat = diag_matrix,
                                   b1 = b1, b2 = b2)
  
  # plotting
  data.frame(diag = seq(diag_low, diag_high, step_size), rho = lg.out$par.est[, "rho"]) %>%
    ggplot(aes(x = diag, y = rho)) +
      geom_line() +
      labs(title = "", x = "Diagonal x1 = x2", y = "Local Gaussian Correlation")
  
}