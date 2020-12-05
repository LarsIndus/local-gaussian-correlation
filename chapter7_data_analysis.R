# Setup --------------------------------------------------------------------

library(localgauss)
library(magrittr)
library(ggplot2)
library(BatchGetSymbols)
library(data.table)
library(fGarch)
library(copula)
library(VineCopula)

source("plot_localgauss.R")
source("plot_localgauss_diagonal.R")

SEED <- 42


# Fetch data and preprocess ------------------------------------------------

# Get daily log returns for Apple and Google from 01/01/2005 to 12/31/2018:
tickers <- c("AAPL", "GOOG")
first.date <- "2005-01-01"
last.date <- "2018-12-31"

l.out <- BatchGetSymbols(tickers = tickers,
                         first.date = first.date,
                         last.date = last.date,
                         type.return = "log",
                         freq.data = "daily",
                         bench.ticker = "^GSPC",
                         do.cache = FALSE)

raw_data <- data.table(l.out$df.tickers)

# Transform into wide format and extract log returns; omit NAs:
mydata <- reshape.wide(raw_data)
mydata <- mydata$ret.adjusted.prices
mydata <- na.omit(mydata, cols = tickers)

# remove zero return rows:
mydata <- mydata[AAPL != 0 & GOOG != 0]

# calculate 100 x log returns:
mydata[, apple_100 := 100 * AAPL]
mydata[, google_100 := 100 * GOOG]

# Perform a GARCH(1,1) filtering with t-distributed innovations:
apple_garch <- garchFit(~ garch(1, 1), data = mydata$AAPL, cond.dist = "std", trace = FALSE)
google_garch <- garchFit(~ garch(1, 1), data = mydata$GOOG, cond.dist = "std", trace = FALSE)

mydata[, apple_garch := residuals(apple_garch, standardize = TRUE)]
mydata[, google_garch := residuals(google_garch, standardize = TRUE)]

# Summaries of the GARCH fitting. Note that all coefficients are significant:
summary(apple_garch)
summary(google_garch)


# Exploratory Data Analysis ----------------------------------------------------

# Prices (Figure 17):
ggplot(data = raw_data, aes(x = ref.date, y = price.close)) +
  geom_line() +
  facet_grid(ticker ~ ., scales = "free_y",
             labeller = labeller(ticker = c(AAPL = "Apple", GOOG = "Google"))) +
  labs(x = "Date", y = "Price [USD]") +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5))

# Log returns (Figure 17):
ggplot(data = na.omit(raw_data), aes(x = ref.date, y = 100 * ret.adjusted.prices)) +
  geom_line() +
  facet_grid(ticker ~ .,
             labeller = labeller(ticker = c(AAPL = "Apple", GOOG = "Google"))) +
  labs(x = "Date", y = "100 x Log Returns") +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5))

# Histograms (Figure 18):
ggplot(data = na.omit(raw_data), aes(x = 100 * ret.adjusted.prices)) +
  geom_histogram(binwidth = 0.5, color = "black", fill = "grey") +
  facet_grid(ticker ~ ., scales = "free_y",
             labeller = labeller(ticker = c(AAPL = "Apple", GOOG = "Google"))) +
  labs(x = "100 x Log Returns", y = "Frequency") +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5))


# QQ Plots:

ggplot(raw_data, aes(sample = 100 * ret.adjusted.prices)) +
  stat_qq(distribution = qnorm) +
  stat_qq_line(distribution = qnorm) +
  facet_grid(ticker ~ .,
             labeller = labeller(ticker = c(AAPL = "Apple", GOOG = "Google"))) +
  labs(x = "", y = "") +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5))

ggplot(raw_data, aes(sample = 100 * ret.adjusted.prices)) +
  stat_qq(distribution = qstd) +
  stat_qq_line(distribution = qstd) +
  facet_grid(ticker ~ .,
             labeller = labeller(ticker = c(AAPL = "Apple", GOOG = "Google"))) +
  labs(x = "", y = "") +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5))

# A t-distribution seems to be much more appropriate.


# Analysis for Nonfiltered Data -------------------------------------------------------

# bandwidth used for local likelihood:
b <- 1.5

# Scatter plot (Figure 19):
ggplot(data = mydata, aes(x = apple_100, y = google_100)) +
  geom_point(size = 0.5) +
  labs(x = "Apple", y = "Google") +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5))

# Correlation is about 0.5:
cor(mydata$apple_100, mydata$google_100, method = "pearson")

lg <- localgauss(x = mydata$apple_100, y = mydata$google_100,
                 b1 = b, b2 = b, gsize = 50, hthresh = 0.0025)

# LGC heat map (Figure 20):
plot_localgauss(lg, plot_text = TRUE) +
  labs(x = "Apple", y = "Google")

# LGC along diagonal (Figure 21):
plot_localgauss_diagonal(mydata[, c("apple_100", "google_100")], b1 = b, b2 = b) +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5))


# Copula Fitting by Maximum Likelihood:
pseudo_observations <- pobs(as.matrix(mydata[, c("apple_100", "google_100")]))

copula <- BiCopSelect(pseudo_observations[, 1], pseudo_observations[, 2],
                      familyset = NA, selectioncrit = "AIC")

copula

# Best fit: t copula with rho = 0.51, df = 3.26
rho <- copula$par
df <- copula$par2

rho
df

# Convert into Gaussian pseudo-observations:
apple_gaussian_pseudo <- qnorm(pseudo_observations[, 1])
google_gaussian_pseudo <- qnorm(pseudo_observations[, 2])

# Compare to simulated t copula along the diagonal:
set.seed(SEED)
n <- 10000 # large sample size - long runtime!
diag_low <- -4
diag_high <- 4
step_size <- 0.1

# t copula with fitted parameters:
t_mvd <- mvdc(
  copula = ellipCopula(family = "t", dim = 2, dispstr = "ex", param = rho, df = df),
  margins = c("norm", "norm"),
  paramMargins = list(list(mean = 0, sd = 1), list(mean = 0, sd = 1))
)

copula_sim <- rMvdc(mvdc = t_mvd, n)

diag_matrix <- matrix(c(seq(diag_low, diag_high, step_size), seq(diag_low, diag_high, step_size)),
                      ncol = 2)

# LGC for Gaussian pseudo-observations:
lg_diag_pseudo <- localgauss(x = apple_gaussian_pseudo, y = google_gaussian_pseudo,
                             xy.mat = diag_matrix, b1 = b, b2 = b)

# LGC for simulated t copula:
lg_diag_t_copula <- localgauss(x = copula_sim[, 1], y = copula_sim[, 2],
                               xy.mat = diag_matrix, b1 = b, b2 = b)

plot_df <- data.frame(diag = seq(diag_low, diag_high, step_size),
                      lgc_pseudo = lg_diag_pseudo$par.est[, "rho"],
                      lgc_t_copula = lg_diag_t_copula$par.est[, "rho"])

# LGC comparison along the diagonal between real data and simulated copula (Figure 22):
plot_df %>% melt(id.vars = "diag", measure.vars = c("lgc_pseudo", "lgc_t_copula")) %>%
  ggplot(aes(x = diag, y = value, linetype = variable)) +
  geom_line(aes(linetype = variable)) +
  scale_linetype_manual(values = c("solid", "dashed"),
                        labels = c("data", "t copula"), name = "") +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    legend.position = c(0.125, 0.16)
  ) +
  labs(x = "Diagonal", y = "LGC")


# Analysis for Filtered Data -------------------------------------------------------

lg_filtered <- localgauss(x = mydata$apple_garch, y = mydata$google_garch,
                          b1 = b, b2 = b, gsize = 40, hthresh = 0.004)

# LGC heat map (Figure 23):
plot_localgauss(lg_filtered, plot_text = TRUE) +
  labs(x = "Apple (filtered)", y = "Google (filtered)")

# LGC along diagonal (Figure 24):
plot_localgauss_diagonal(mydata[, c("apple_garch", "google_garch")], b1 = b, b2 = b) +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5))


# Copula Fitting by Maximum Likelihood:
pseudo_observations_filtered <- pobs(as.matrix(mydata[, c("apple_garch", "google_garch")]))

copula_filtered <- BiCopSelect(pseudo_observations_filtered[, 1], pseudo_observations_filtered[, 2],
                               familyset = NA, selectioncrit = "AIC")

copula_filtered

# Best fit: t copula with rho = 0.5, df = 5.25
rho_filtered <- copula_filtered$par
df_filtered <- copula_filtered$par2

rho_filtered
df_filtered

# Maximum Likelihood still results in the t copula, but we see from the LGC plots
# that it does not provide an adequate fit.