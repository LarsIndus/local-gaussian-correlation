# _____________________________________________________________________________________________________
#
# This function creates local Gaussian correlation heat maps for an object of type localgauss.
# Objects of these class are created using the package localgauss.
# If 'plot_points' is set to true, the original observations (stored in the localgauss object)
# will be lain over.
# If 'plot_text' is set to true, the local Gaussian correlation values will be
# printed onto the heat map.
#
# _____________________________________________________________________________________________________


if (!require(checkmate)) {
  install.packages("checkmate")
  library(checkmate)
}

if (!require(magrittr)) {
  install.packages("magrittr")
  library(magrittr)
}

if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(checkmate)
}

if (!require(data.table)) {
  install.packages("data.table")
  library(data.table)
}


plot_localgauss <- function(dat, plot_points = FALSE, plot_text = FALSE,
                            text_size = 3, points_size = 1, points_color = "black",
                            low_color = "#12b5ac", mid_color = "#ffffff", high_color = "#d82051") {

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