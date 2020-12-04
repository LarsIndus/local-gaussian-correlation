#' Create a line plot of local Gaussian correlation estimates along the diagonal
#'
#' @description
#' This function estimates local Gaussian correlation along the main diagonal
#' of a two-dimensional plane and plots the estimated values in a line plot.
#' Default values are appropriate for standard normal marginal distributions.
#'
#' @param dat Bivariate input data, either a matrix, data frame or data table.
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
#' set.seed(42)
#'
#' Sample from a bivariate normal distribution:
#'
#' n <- 1000
#' mean_x <- 0
#' mean_y <- 0
#' var_x <- 1
#' var_y <- 1
#' rho <- -0.7
#' 
#' x <- MASS::mvrnorm(n, mu = c(mean_x, mean_y),
#'                    Sigma = matrix(c(var_x, rho, rho, var_y), ncol = 2))
#'                    
#' plot_localgauss_diagonal(x)
#' }

plot_localgauss_diagonal <- function(dat, diag_low = -4, diag_high = 4, step_size = 0.1,
                                     b1 = 1, b2 = 1) {
    
  # function logic based on data.frame
  if(is.data.table(dat)) {
    dat <- data.frame(dat)
  }
  
  # values for which to calculate LGC
  diag_matrix <- matrix(c(seq(diag_low, diag_high, step_size),
                          seq(diag_low, diag_high, step_size)),
                        ncol = 2)
  
  lg.out <- localgauss(x = dat[, 1], y = dat[, 2], xy.mat = diag_matrix, b1 = b1, b2 = b2)
  
  data.frame(diag = seq(diag_low, diag_high, step_size), rho = lg.out$par.est[, "rho"]) %>%
    ggplot(aes(x = diag, y = rho)) +
      geom_line() +
      labs(title = "", x = "Diagonal x1 = x2", y = "Local Gaussian Correlation")
  
}