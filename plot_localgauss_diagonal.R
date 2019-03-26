# _____________________________________________________________________________________________________
#
# This function plots local Gaussian correlation plots along the diagonal.
# 'dat' must be bivariate data in the form of a data.frame or data.table.
# The function calls localgauss to estimate the local Gaussian correlation along the diagonal
# from 'diag_low' to 'diag_high', where the distance between points is set by 'step_size'.
# Default values are appropriate for standard normal marginals.
# b1 and b2 are the bandwidths used by the function localgauss.
#
# _____________________________________________________________________________________________________


if (!require(magrittr)) {
  install.packages("magrittr")
  library(magrittr)
}

if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}

if (!require(localgauss)) {
  install.packages("localgauss")
  library(localgauss)
}


plot_localgauss_diagonal <- function(dat, diag_low = -4, diag_high = 4, step_size = 0.1,
                                     b1 = 1, b2 = 1) {
    
  # function logic based on data.frame
  if(is.data.table(dat)) {
    dat <- data.frame(dat) 
  }
  
  # values for which to calculate LGC
  diag_matrix <- matrix(c(seq(diag_low, diag_high, step_size), seq(diag_low, diag_high, step_size)),
                        ncol = 2)
  
  lg.out <- localgauss(x = dat[, 1], y = dat[, 2], xy.mat = diag_matrix, b1 = b1, b2 = b2)
  
  data.frame(diag = seq(diag_low, diag_high, step_size), rho = lg.out$par.est[, "rho"]) %>%
    ggplot(aes(x = diag, y = rho)) +
      geom_line() +
      labs(title = "", x = "Diagonal x1 = x2", y = "Local Gaussian Correlation")
  
}