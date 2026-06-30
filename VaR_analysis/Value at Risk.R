if(!require(moments)) install.packages("moments")
if(!require(readxl)) install.packages("readxl")
if(!require(MASS)) install.packages("MASS")
if(!require(tseries)) install.packages("tseries")
library(moments)
library(dplyr)

setwd("/Users/sboerema/Desktop/Algemeen/Fintech")
stocks = read_excel("VaR_Dataset.xlsx")
attach(stocks)
View(stocks)

stocks <- Quantatitive_methods_HW3
stocks$Date = as.Date(stocks$Date, format = "%m/%d/%Y")
stocks= stocks[order(stocks$Date),]
View(stocks)

stocks <- stocks %>%
  mutate(across(-Date, ~ as.numeric(gsub("[^0-9\\.]", "", .x))))
View(stocks)

#Excersise 1 
# Compute the mean and the variance of the daily log returns
log_returns_ISP <- diff(log(stocks$ISP))
log_returns_BBVA <- diff(log(stocks$BBVA))
log_returns_CRDI <- diff(log(stocks$CRDI))
log_returns_BNP <- diff(log(stocks$BNP))
log_returns_DBK <- diff(log(stocks$DBK))

returns <- data.frame(
  ISP = log_returns_ISP,
  BBVA = log_returns_BBVA,
  CRDI = log_returns_CRDI,
  BNP = log_returns_BNP,
  DBK = log_returns_DBK
  )

View(returns)

mean_returns <- sapply(returns, mean, na.rm=TRUE)
print(mean_returns)

variance_returns <- sapply(returns, var, na.rm=TRUE)
print(variance_returns)

# sample variance using EWMA
ewma_var_safe <- function(r, lambda = 0.95) {
  
  r <- r[!is.na(r)]  # remove NA returns
  
  ewma_var <- numeric(length(r))
  ewma_var[1] <- var(r)   # initial variance
  
  for (t in 2:length(r)) {
    ewma_var[t] <- lambda * ewma_var[t-1] + (1 - lambda) * r[t-1]^2
  }
  
  return(ewma_var)
}
ewma_variances <- sapply(returns, ewma_var_safe)
final_ewma_variance <- apply(ewma_variances, 2, tail, 1)
final_ewma_variance

# Combine results into a data frame
variance_table <- data.frame(
  Asset = names(variance_returns),
  Sample_Variance = variance_returns,
  EWMA_Variance = final_ewma_variance
)

print(variance_table)

#Compute the VaR at 0.01 of each asset at one day and ten days under the assumption of Normality and i.i.d. returns
# Confidence level
alpha <- 0.01
z_alpha <- qnorm(alpha)  # 1% quantile of standard normal (~ -2.326)

# Daily standard deviation
sd_daily <- sqrt(variance_returns)

# Daily VaR
VaR_1d <- -(mean_returns) + abs(z_alpha) * sd_daily

# 10-day VaR (assuming i.i.d. returns)
VaR_10d <- VaR_1d * sqrt(10)

# Combine results into a data frame
VaR_df <- data.frame(
  Asset = names(mean_returns),
  VaR_1d = VaR_1d,
  VaR_10d = VaR_10d
)

print(VaR_df)

# Compute the one day VaR at 0.01 of the portfolio (1euro) made up of 50% of Unicredit and 50% of Intesa Sanpaolo
# Portfolio weights
w <- c(0.5, 0.5)
names(w) <- c("CRDI", "ISP")

# Extract means and standard deviations for the two assets
mu_portfolio <- sum(w * mean_returns[c("CRDI", "ISP")])

# Covariance matrix of the two assets
cov_matrix <- cov(returns[, c("CRDI", "ISP")])

# Portfolio variance
sigma2_portfolio <- t(w) %*% cov_matrix %*% w
sigma_portfolio <- sqrt(sigma2_portfolio)

# 1-day VaR at 1%
alpha <- 0.01
z_alpha <- qnorm(alpha)
VaR_1d_portfolio <- -mu_portfolio + abs(z_alpha) * sigma_portfolio

VaR_1d_portfolio

#VaR at 0.01 of the portfolio made up of 20% of Unicredit, 20% BBVA bank, 20% BNP Paribas, 20% of Intesa Sanpaolo, 20% Deutsche Bank.
# Portfolio weights (20% each)
w2 <- rep(0.2, 5)
names(w2) <- c("CRDI", "BBVA", "BNP", "ISP", "DBK")

# Portfolio mean
mu_portfolio2 <- sum(w2 * mean_returns[names(w2)])

# Covariance matrix of the 5 assets
cov_matrix2 <- cov(returns[, names(w2)])

# Portfolio variance
sigma2_portfolio2 <- t(w2) %*% cov_matrix2 %*% w2
sigma_portfolio2 <- sqrt(sigma2_portfolio2)

# 1-day VaR at 1%
alpha <- 0.01
z_alpha <- qnorm(alpha)
VaR_1d_portfolio2 <- -mu_portfolio2 + abs(z_alpha) * sigma_portfolio2

VaR_1d_portfolio2

#Estimate VaR at 0.01 and 0.05 through historical simulation 
# simple simulation
alphas <- c(0.01, 0.05)

simple_VaR <- sapply(returns, function(r) {
  sapply(alphas, function(a) {
    -quantile(r, probs = a, na.rm = TRUE)   # minus sign → loss as positive number
  })
})

# make it nice
simple_VaR <- t(simple_VaR)
colnames(simple_VaR) <- c("VaR_1pct", "VaR_5pct")
simple_VaR

# Weighted simulation
weighted_var <- function(r, alphas = c(0.01, 0.05), lambda = 0.95) {
  r <- r[!is.na(r)]
  T <- length(r)
  
  # exponential weights (oldest has smallest weight)
  exponents <- (T-1):0
  w_raw <- (1 - lambda) * lambda^exponents
  w <- w_raw / sum(w_raw)
  
  # sort returns and align weights
  ord <- order(r)
  r_sorted <- r[ord]
  w_sorted <- w[ord]
  cw <- cumsum(w_sorted)
  
  # weighted quantiles
  sapply(alphas, function(a) {
    idx <- which(cw >= a)[1]
    -r_sorted[idx]      # VaR as positive loss
  })
}

weighted_VaR <- t(sapply(returns, weighted_var))
colnames(weighted_VaR) <- c("VaR_1pct_weighted", "VaR_5pct_weighted")
weighted_VaR

#Column with both simple and weighted VaR
VaR_table <- data.frame(
  Asset = rownames(simple_VaR),
  Simple_VaR_1pct   = simple_VaR[, "VaR_1pct"],
  Simple_VaR_5pct   = simple_VaR[, "VaR_5pct"],
  Weighted_VaR_1pct = weighted_VaR[, "VaR_1pct_weighted"],
  Weighted_VaR_5pct = weighted_VaR[, "VaR_5pct_weighted"]
)

VaR_table






