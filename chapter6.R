# Setup ----------------------------------------------------------------

library(localgauss)
library(magrittr)
library(ggplot2)
library(copula)
library(VineCopula)

source("plot_localgauss.R")
source("plot_localgauss_diagonal.R")

SEED <- 42


# Copula Scatterplots and LGC heat maps (Figures 9 and 10-15) -----------------------------

n <- 500   # sample size for scatter plots
m <- 1000  # sample size for heat maps

# 1. Gaussian Copula (Figures 9 and 10)
set.seed(SEED)

rho <- 0.7
b <- 1

# Gaussian copula with uniform marginals and correlation 0.7
gauss_mvd_uniform_marginals <- mvdc(
  copula = ellipCopula(family = "normal", dim = 2, dispstr = "ex", param = rho),
  margins = c("unif", "unif"),
  paramMargins = list(list(min = 0, max = 1), list(min = 0, max = 1))
)

y <- rMvdc(mvdc = gauss_mvd_uniform_marginals, n)

# scatter plot  
data.frame(x = y[, 1], y = y[, 2]) %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(size = 0.5) +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5))

# Gaussian copula with Gaussian marginals and correlation 0.7
gauss_mvd_gaussian_marginals <- mvdc(
  copula = ellipCopula(family = "normal", dim = 2, dispstr = "ex", param = rho),
  margins = c("norm", "norm"),
  paramMargins = list(list(mean = 0, sd = 1), list(mean = 0, sd = 1))
)

set.seed(SEED)
x <- rMvdc(mvdc = gauss_mvd_gaussian_marginals, m)

lg_gauss_gaussian_marginals <- localgauss(x = x[, 1], y = x[, 2], b1 = b, b2 = b, gsize = 15)

# LGC plot:
plot_localgauss(lg_gauss_gaussian_marginals, plot_text = TRUE)


# 2. t Copula (Figures 9 and 11)
set.seed(SEED)

rho <- 0.7
df <- 2
b <- 1

# t copula with uniform marginals, correlation 0.7 and degrees of freedom 2
t_mvd_uniform_marginals <- mvdc(
  copula = ellipCopula(family = "t", dim = 2, dispstr = "ex", param = rho, df = df),
  margins = c("unif", "unif"),
  paramMargins = list(list(min = 0, max = 1), list(min = 0, max = 1))
)

y <- rMvdc(mvdc = t_mvd_uniform_marginals, n)

# scatter plot
data.frame(x = y[, 1], y = y[, 2]) %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(size = 0.5) +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5))

# t copula with Gaussian marginals, correlation 0.7 and degrees of freedom 2
t_mvd_gaussian_marginals <- mvdc(
  copula = ellipCopula(family = "t", dim = 2, dispstr = "ex", param = rho, df = df),
  margins = c("norm", "norm"),
  paramMargins = list(list(mean = 0, sd = 1), list(mean = 0, sd = 1))
)

set.seed(SEED)
x <- rMvdc(mvdc = t_mvd_gaussian_marginals, m)

lg_t_gaussian_marginals <- localgauss(x = x[, 1], y = x[, 2], b1 = b, b2 = b,
                                      gsize = 15, hthresh = 0.005)

# LGC plot:
plot_localgauss(lg_t_gaussian_marginals, plot_text = TRUE)


# 3. Clayton Copula (Figures 9 and 12)
set.seed(SEED)

theta <- 3
b <- 1

# corresponding value of Kendall's tau:
BiCopPar2Tau(3, par = theta)

# Clayton copula with uniform marginals and parameter theta = 3
clayton_mvd_uniform_marginals <- mvdc(
  copula = archmCopula(family = "Clayton", dim = 2, param = theta),
  margins = c("unif", "unif"),
  paramMargins = list(list(min = 0, max = 1), list(min = 0, max = 1))
)

y <- rMvdc(mvdc = clayton_mvd_uniform_marginals, n)

data.frame(x = y[, 1], y = y[, 2]) %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(size = 0.5) +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5))

# Clayton copula with Gaussian marginals and parameter theta = 3
clayton_mvd_gaussian_marginals <- mvdc(
  copula = archmCopula(family = "Clayton", dim = 2, param = theta),
  margins = c("norm", "norm"),
  paramMargins = list(list(mean = 0, sd = 1), list(mean = 0, sd = 1))
)

set.seed(SEED)
x <- rMvdc(mvdc = clayton_mvd_gaussian_marginals, m)

lg_clayton_gaussian_marginals <- localgauss(x = x[, 1], y = x[, 2], b1 = b, b2 = b,
                                            gsize = 15, hthresh = 0.001)

# LGC plot:
plot_localgauss(lg_clayton_gaussian_marginals, plot_text = TRUE)
  

# 4. Gumbel Copula (Figures 9 and 13)
set.seed(SEED)

theta <- 2.5
b <- 1

# corresponding value of Kendall's tau:
BiCopPar2Tau(4, par = theta)

# Gumbel copula with uniform marginals and parameter theta = 2.5
gumbel_mvd_uniform_marginals <- mvdc(
  copula = archmCopula(family = "Gumbel", dim = 2, param = theta),
  margins = c("unif", "unif"),
  paramMargins = list(list(min = 0, max = 1), list(min = 0, max = 1))
)

y <- rMvdc(mvdc = gumbel_mvd_uniform_marginals, n)

# scatter plot
data.frame(x = y[, 1], y = y[, 2]) %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(size = 0.5) +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5))
  
# Gumbel copula with Gaussian marginals and parameter theta = 2.5
gumbel_mvd_gaussian_marginals <- mvdc(
  copula = archmCopula(family = "Gumbel", dim = 2, param = theta),
  margins = c("norm", "norm"),
  paramMargins = list(list(mean = 0, sd = 1), list(mean = 0, sd = 1))
)

set.seed(SEED)  
x <- rMvdc(mvdc = gumbel_mvd_gaussian_marginals, m)

lg_gumbel_gaussian_marginals <- localgauss(x = x[, 1], y = x[, 2], b1 = b, b2 = b,
                                           gsize = 15, hthresh = 0.005)

# LGC plot:
plot_localgauss(lg_gumbel_gaussian_marginals, plot_text = TRUE)


# 5. Frank Copula (Figures 9 and 14)
set.seed(SEED)

theta <- 8
b <- 1

# corresponding value of Kendall's tau:
BiCopPar2Tau(5, par = theta)

# Frank copula with uniform marginals and parameter theta = 8
frank_mvd_uniform_marginals <- mvdc(
  copula = archmCopula(family = "Frank", dim = 2, param = theta),
  margins = c("unif", "unif"),
  paramMargins = list(list(min = 0, max = 1), list(min = 0, max = 1))
)

y <- rMvdc(mvdc = frank_mvd_uniform_marginals, n)

# scatter plot
data.frame(x = y[, 1], y = y[, 2]) %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(size = 0.5) +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5))

# Frank copula with Gaussian marginals and parameter theta = 8
frank_mvd_gaussian_marginals <- mvdc(
  copula = archmCopula(family = "Frank", dim = 2, param = theta),
  margins = c("norm", "norm"),
  paramMargins = list(list(mean = 0, sd = 1), list(mean = 0, sd = 1))
)

set.seed(SEED)
x <- rMvdc(mvdc = frank_mvd_gaussian_marginals, m)

lg_frank_gaussian_marginals <- localgauss(x = x[, 1], y = x[, 2], b1 = b, b2 = b,
                                          gsize = 15, hthresh = 0.0005)

# LGC plot:
plot_localgauss(lg_frank_gaussian_marginals, plot_text = TRUE)


# 6. Joe Copula (Figures 9 and 15)
set.seed(SEED)

theta <- 4
b <- 1

# corresponding value of Kendall's tau:
BiCopPar2Tau(6, par = theta)

# Joe copula with uniform marginals and parameter theta = 4
joe_mvd_uniform_marginals <- mvdc(
  copula = archmCopula(family = "Joe", dim = 2, param = theta),
  margins = c("unif", "unif"),
  paramMargins = list(list(min = 0, max = 1), list(min = 0, max = 1))
)

y <- rMvdc(mvdc = joe_mvd_uniform_marginals, n)

# scatter plot
data.frame(x = y[, 1], y = y[, 2]) %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(size = 0.5) +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5))

# Joe copula with Gaussian marginals and parameter theta = 4
joe_mvd_gaussian_marginals <- mvdc(
  copula = archmCopula(family = "Joe", dim = 2, param = theta),
  margins = c("norm", "norm"),
  paramMargins = list(list(mean = 0, sd = 1), list(mean = 0, sd = 1))
)

set.seed(SEED)
x <- rMvdc(mvdc = joe_mvd_gaussian_marginals, m)

lg_joe_gaussian_marginals <- localgauss(x = x[, 1], y = x[, 2], b1 = b, b2 = b,
                                        gsize = 15, hthresh = 0.005)

# LGC plot:
plot_localgauss(lg_joe_gaussian_marginals, plot_text = TRUE)


# LGC along diagonal for copulas (Figure 16) ---------------------------------------

n <- 10000 # large sample size for smooth curves - long runtime!
b <- 1


# 1. Gaussian copula
set.seed(SEED)

rho <- 0.7

# Gaussian copula with Gaussian marginals and correlation 0.7
gauss_mvd <- mvdc(
  copula = ellipCopula(family = "normal", dim = 2, dispstr = "ex", param = rho),
  margins = c("norm", "norm"),
  paramMargins = list(list(mean = 0, sd = 1), list(mean = 0, sd = 1))
)

x <- rMvdc(mvdc = gauss_mvd, n)

# LGC along diagonal:
plot_localgauss_diagonal(x, b1 = b, b2 = b) +
  ylim(0, 1) +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5))


# 2. t Copula
set.seed(SEED)

rho <- 0.7
nu <- 2

# t copula with Gaussian marginals, correlation 0.7 and degrees of freedom 2
t_mvd <- mvdc(
  copula = ellipCopula(family = "t", dim = 2, dispstr = "ex", param = rho, df = nu),
  margins = c("norm", "norm"),
  paramMargins = list(list(mean = 0, sd = 1), list(mean = 0, sd = 1))
)

x <- rMvdc(mvdc = t_mvd, n)

# LGC along diagonal:
plot_localgauss_diagonal(x, b1 = b, b2 = b) +
  ylim(0, 1) +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5))


# 3. Clayton copula
set.seed(SEED)

theta <- 3

# Clayton copula with Gaussian marginals and parameter theta = 3
clayton_mvd <- mvdc(
  copula = archmCopula(family = "Clayton", dim = 2, param = theta),
  margins = c("norm", "norm"),
  paramMargins = list(list(mean = 0, sd = 1), list(mean = 0, sd = 1))
)

x <- rMvdc(mvdc = clayton_mvd, n)

# LGC along diagonal:
plot_localgauss_diagonal(x, b1 = b, b2 = b) +
  ylim(0, 1) +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5))


# 4 Gumbel Copula
set.seed(SEED)

theta <- 2.5

# Gumbel copula with Gaussian marginals and parameter theta = 2.5
gumbel_mvd <- mvdc(
  copula = archmCopula(family = "Gumbel", dim = 2, param = theta),
  margins = c("norm", "norm"),
  paramMargins = list(list(mean = 0, sd = 1), list(mean = 0, sd = 1))
)

x <- rMvdc(mvdc = gumbel_mvd, n)

# LGC along diagonal:
plot_localgauss_diagonal(x, b1 = b, b2 = b) +
  ylim(0, 1) +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5))


# 5. Frank Copula
set.seed(SEED)

theta <- 8

# Frank copula with Gaussian marginals and parameter theta = 8
frank_mvd <- mvdc(
  copula = archmCopula(family = "Frank", dim = 2, param = theta),
  margins = c("norm", "norm"),
  paramMargins = list(list(mean = 0, sd = 1), list(mean = 0, sd = 1))
)

x <- rMvdc(mvdc = frank_mvd, n)

# LGC along diagonal:
plot_localgauss_diagonal(x, b1 = b, b2 = b) +
  ylim(0, 1) +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5))


# 6. Joe Copula
set.seed(SEED)

theta <- 4

# Joe copula with Gaussian marginals and parameter theta = 4
joe_mvd <- mvdc(
  copula = archmCopula(family = "Joe", dim = 2, param = theta),
  margins = c("norm", "norm"),
  paramMargins = list(list(mean = 0, sd = 1), list(mean = 0, sd = 1))
)

x <- rMvdc(mvdc = joe_mvd, n)

# LGC along diagonal:
plot_localgauss_diagonal(x, b1 = b, b2 = b) +
  ylim(0, 1) +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5))