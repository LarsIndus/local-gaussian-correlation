# Setup ----------------------------------------------------------------

library(localgauss)
library(magrittr)
library(ggplot2)
library(mvtnorm)
library(TSA)
library(copula)

source("plot_localgauss.R")

SEED <- 42


# LGC for Double Parabola (Figure 5) ----------------------------------------

set.seed(SEED)

n <- 500
x <- runif(n, -3, 3)
noise <- rnorm(n, sd = sqrt(1))
u <- sample(c(-1, 1), n, replace = TRUE)
y <- u * (x^2 + noise)
b <- 2

# Pearson correlation is close to 0:
cor(x, y, method = "pearson")

lg_double <- localgauss(x = x, y = y, b1 = b, b2 = b, gsize = 15)

plot_localgauss(lg_double, plot_points = TRUE, points_size = 0.5)


# LGC for Bivariate t Distribution (Figure 6) -------------------------------------------------------

set.seed(SEED)

n <- 2000
df <- 5
rho <- 0
sigma <- matrix(c(1, rho, rho, 1), nrow = 2)
b <- 0.7

x <- rmvt(n, sigma = sigma, df = df)
lg_t <- localgauss(x = x[, 1], y = x[, 2], gsize = 60, b1 = b, b2 = b, hthresh = 0.008)

plot_localgauss(lg_t, plot_text = TRUE, plot_points = FALSE)


# LGC for GARCH(1,1) Model (Figure 7) ----------------------------------------------------------

set.seed(SEED)

n <- 2000
alpha <- c(0.2, 0.6)
beta <- 0.2
garch <- garch.sim(alpha = alpha, beta = beta, n = n)
b <- 0.7

y_lag1 <- garch[-1]
x_lag1 <- garch[1 : (n - 1)]
lg_GARCH <- localgauss(x = x_lag1, y = y_lag1, b1 = b, b2 = b, gsize = 50, hthresh = 0.015)

plot_localgauss(lg_GARCH, plot_text = TRUE, plot_points = FALSE)

 
# LGC for Clayton Copula (Figure 8) --------------------------------------------------------

set.seed(SEED)

n <- 500
b <- 1

# Clayton copula with standard normal marginals and parameter theta = 2
my_mvd <- mvdc(copula = archmCopula(family = "Clayton", dim = 2, param = 2),
               margins = c("norm", "norm"),
               paramMargins = list(list(mean = 0, sd = 1), list(mean = 0, sd = 1)))

x <- rMvdc(mvdc = my_mvd, n)

lg_clayton <- localgauss(x = x[, 1], y = x[, 2], b1 = b, b2 = b, gsize = 15)

plot_localgauss(lg_clayton, plot_points = TRUE, plot_text = FALSE,points_size = 0.5)