# Setup ----------------------------------------------------------------

library(MASS)
library(localgauss)
library(checkmate)
library(magrittr)
library(ggplot2)
library(data.table)

source("plot_localgauss.R")

my_seed <- 42


# LGC for Normal Distribution (Figure 3) --------------------------------------------------------

set.seed(my_seed)

n <- 1000
mean_x <- 0
mean_y <- 0
var_x <- 1
var_y <- 1
rho <- -0.7
b <- 1

x <- mvrnorm(n, mu = c(mean_x, mean_y), Sigma = matrix(c(var_x, rho, rho, var_y), ncol = 2))
lg_normal <- localgauss(x = x[, 1], y = x[, 2], gsize = 15, b1 = b, b2 = b, hthresh = 0.01)

plot_localgauss(lg_normal, plot_text = TRUE, plot_points = FALSE)


# LGC for Parabola -------------------------------------------------------------

set.seed(my_seed)

n <- 1000
mean_x <- 0
sd_x <- sqrt(1)
mean_epsilon <- 0
sd_epsilon <- sqrt(0.5)
b <- 2

x <- rnorm(n, mean = mean_x, sd = sd_x)
epsilon <- rnorm(n, mean = mean_epsilon, sd = sd_epsilon)
y <- x^2 + epsilon

# Pearson correlation close to zero:
cor(x, y)

lg_parabola <- localgauss(x = x, y = y, b1 = b, b2 = b, gsize = 25, hthresh = 0.005)

plot_localgauss(lg_parabola, plot_text = TRUE, plot_points = FALSE)