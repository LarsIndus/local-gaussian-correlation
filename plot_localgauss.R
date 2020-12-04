#' Create a heat map of local Gaussian correlation estimates
#'
#' @description
#' This function takes an object of type localgauss (created with the local gauss package),
#' i.e., estimates for the local Gaussian correlation, and visualizes them as a heat map.
#'
#' @param dat localgauss object created by the package localgauss.
#' @param plot_points Logical, if TRUE, original data points are shown on the heat map
#'   (note that they are stored in the localgauss object).
#' @param plot_text Logical, if TRUE, the values will be printed onto the heat map.
#' @param text_size Numeric, text size of the values that can be printed.
#' @param points_size Numeric, size of the original points that can be shown.
#' @param points_color Color of the original points that can be shown.
#' @param low_color Color for the lower spectrum of the local Gaussian correlation.
#' @param mid_color Color for the middle spectrum of the local Gaussian correlation,
#'   i.e., around 0.
#' @param high_color Color for the upper spectrum of the local Gaussian correlation.
#' @return A ggplot object showing the local Gaussian correlation estimates as a heat map.
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
#' b <- 1
#'
#' x <- MASS::mvrnorm(n, mu = c(mean_x, mean_y),
#'                    Sigma = matrix(c(var_x, rho, rho, var_y), ncol = 2))
#'                    
#' lg_normal <- localgauss(x = x[, 1], y = x[, 2], gsize = 15, b1 = b, b2 = b, hthresh = 0.01)
#' 
#' plot_localgauss(lg_normal, plot_text = TRUE, plot_points = FALSE)
#' }

plot_localgauss <- function(dat, plot_points = FALSE, plot_text = FALSE,
                            text_size = 3, points_size = 1, points_color = "black",
                            low_color = "#12b5ac", mid_color = "#ffffff",
                            high_color = "#d82051") {

  # Check arguments:
  assert_class(dat, classes = "localgauss")
  assert_logical(c(plot_points, plot_text))
  
  original_points <- data.table(x = dat$x, y = dat$y)
  
  # Create the plot:
  p <- data.table(x = dat$xy.mat[, 1], y = dat$xy.mat[, 2], rho = dat$par.est[, "rho"]) %>%
    ggplot(aes(x = x, y = y)) +
    geom_tile(aes(fill = rho)) +
    scale_fill_gradient2(
      low = low_color, mid = mid_color, high = high_color,
      midpoint = 0, limit = c(-1, 1), breaks = seq(-1, 1, 0.2),
      space = "Lab",
      name = "LGC",
      guide = guide_colorbar(barheight = 12, raster = FALSE, nbin = 11, ticks = FALSE,
                             frame.colour = "black", frame.linewidth = 1)
    ) +
    labs(title = "", x = "", y = "")
  
  # Add points and/or text:
  if(plot_points) {
    p <- p + geom_point(data = original_points, size = points_size, color = points_color)
  }

  if(plot_text) {
    p <- p + geom_text(aes(label = round(rho, 2)), size = text_size)
  }

  return(p)

}