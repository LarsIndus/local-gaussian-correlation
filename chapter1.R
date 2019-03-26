# Setup ----------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(data.table)
library(MASS)

my_seed <- 42


# Outlier Behavior (Figure 1) --------------------------------------------------------

set.seed(my_seed)

n <- 100
z <- mvrnorm(n, mu = c(0, 0), Sigma = matrix(c(1, 0, 0, 1), ncol = 2))
x <- z[, 1]
y <- z[, 2]

# All correlation measures near zero, as sample is drawn from a bivariate normal with rho = 0:
cor(x, y, method = "pearson")
cor(x, y, method = "spearman")
cor(x, y, method = "kendall")

# Large value added (outlier):
x <- append(x, 25)
y <- append(y, 25)

# Pearson detects a linear relationship now; others not that much affected:
cor(x, y, method = "pearson")
cor(x, y, method = "spearman")
cor(x, y, method = "kendall")

data.frame(x = x, y = y) %>% ggplot(aes(x = x, y = y)) +
  geom_point(size = 0.5) +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5)) +
  labs(title = "", x = "", y = "")


# Correlation Coefficients for Nonmonotonic Relationship (Figure 2) ---------------------------------

set.seed(my_seed)

n <- 200
mean_x <- 0
sd_x <- sqrt(1)
mean_epsilon <- 0
sd_epsilon <- sqrt(0.2)

x <- runif(n, -2, 2)
epsilon <- rnorm(n, mean = mean_epsilon, sd = sd_epsilon)
y <- x^2 + epsilon

# Correlation coefficients close to 0 due to symmetry (nonmonotonic relationship):
cor(x, y, method = "pearson")
cor(x, y, method = "spearman")
cor(x, y, method = "kendall")

# Check correlation on positive/negative x-axis. Positive on the positive axis and vice versa:
df_positive <- data.frame(x = x, y = y) %>% dplyr::filter(x > 0)
df_negative <- data.frame(x = x, y = y) %>% dplyr::filter(x < 0)

cor(df_positive[, 1], df_positive[, 2], method = "pearson")
cor(df_positive[, 1], df_positive[, 2], method = "spearman")
cor(df_positive[, 1], df_positive[, 2], method = "kendall")

cor(df_negative[, 1], df_negative[, 2], method = "pearson")
cor(df_negative[, 1], df_negative[, 2], method = "spearman")
cor(df_negative[, 1], df_negative[, 2], method = "kendall")

data.frame(x = x, y = y) %>% ggplot(aes(x = x, y = y)) +
  geom_point(size = 0.5) +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5)) +
  labs(title = "", x = "", y = "")