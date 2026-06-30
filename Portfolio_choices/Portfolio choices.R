install.packages("MASS")
install.packages("moments")
install.packages("readxl")
install.packages("zoo")

# We turn the date column from character to date in the data set
str(df)
df = DATASET_STOCK_PRICES_
df$Date <- as.Date(df$Date, format = "%m/%d/%Y")
# if x is your xts object
df <- df[order(df$Date), ]  # this sorts it from oldest to newest
# We downlaod the xts library in order to attach the date to the data
library(xts)
# We define the TS for every stock
price_xts_AAPL <- xts(df$AAPL, order.by = df$Date)
price_xts_AMZN <- xts(df$AMZN, order.by = df$Date)
price_xts_META <- xts(df$META, order.by = df$Date)
price_xts_GM <- xts(df$GM, order.by = df$Date)
price_xts_WMT <- xts(df$WMT, order.by = df$Date)
price_xts_JPM <- xts(df$JPM, order.by = df$Date)
# We conduct some descriptive statistics for APPLE to make sure that the method work
class(price_xts_AAPL)
print(price_xts_AAPL)
# We revert to the logarithm for easiness of use
# For APPLE
log(price_xts_AAPL)
L_price_xts_AAPL = log(price_xts_AAPL)
print(L_price_xts_AAPL)
plot(L_price_xts_AAPL)
dl_price_xts_AAPL = diff(L_price_xts_AAPL)
plot(dl_price_xts_AAPL)
# AMAZON
log(price_xts_AMZN)
L_price_xts_AMZN = log(price_xts_AMZN)
print(L_price_xts_AMZN)
plot(L_price_xts_AMZN)
dl_price_xts_AMZN = diff(L_price_xts_AMZN)
plot(dl_price_xts_AMZN)
# META
log(price_xts_META)
L_price_xts_META = log(price_xts_META)
print(L_price_xts_META)
plot(L_price_xts_META)
dl_price_xts_META = diff(L_price_xts_META)
plot(dl_price_xts_META)
# General Motors
log(price_xts_GM)
L_price_xts_GM = log(price_xts_GM)
print(L_price_xts_GM)
plot(L_price_xts_GM)
dl_price_xts_GM = diff(L_price_xts_GM)
plot(dl_price_xts_GM)
# WALMART
log(price_xts_WMT)
L_price_xts_WMT = log(price_xts_WMT)
print(L_price_xts_WMT)
plot(L_price_xts_WMT)
dl_price_xts_WMT = diff(L_price_xts_WMT)
plot(dl_price_xts_WMT)
# JPMorgan
log(price_xts_JPM)
L_price_xts_JPM = log(price_xts_JPM)
print(L_price_xts_JPM)
plot(L_price_xts_JPM)
dl_price_xts_JPM = diff(L_price_xts_JPM)
plot(dl_price_xts_JPM)
# we conduct summary statistics, because of the use of xts the first value in p - (p-1) reverts as NA, we adjust for this 
# AAPL
dl_price_xts_AAPL <- na.omit(dl_price_xts_AAPL)
MEAN_dl_AAPL = mean(dl_price_xts_AAPL)
print (MEAN_dl_AAPL)
VAR_dl_AAPL = var(dl_price_xts_AAPL)
print(VAR_dl_AAPL)
SD_dl_AAPL = sqrt(var(dl_price_xts_AAPL))
# AMZN 
dl_price_xts_AMZN <- na.omit(dl_price_xts_AMZN)
MEAN_dl_AMZN = mean(dl_price_xts_AMZN)
VAR_dl_AMZN = var(dl_price_xts_AMZN)
SD_dl_AMZN = sqrt(var(dl_price_xts_AMZN))
# META
dl_price_xts_META <- na.omit(dl_price_xts_META)
MEAN_dl_META = mean(dl_price_xts_META)
VAR_dl_META = var(dl_price_xts_META)
SD_dl_META = sqrt(var(dl_price_xts_META))
# GM
dl_price_xts_GM <- na.omit(dl_price_xts_GM)
MEAN_dl_GM = mean(dl_price_xts_GM)
VAR_dl_GM = var(dl_price_xts_GM)
SD_dl_GM = sqrt(var(dl_price_xts_GM))
# WMT 
dl_price_xts_WMT <- na.omit(dl_price_xts_WMT)
MEAN_dl_WMT = mean(dl_price_xts_WMT)
VAR_dl_WMT = var(dl_price_xts_WMT)
SD_dl_WMT = sqrt(var(dl_price_xts_WMT))
# JPM 
dl_price_xts_JPM <- na.omit(dl_price_xts_JPM)
MEAN_dl_JPM= mean(dl_price_xts_JPM)
VAR_dl_JPM = var(dl_price_xts_JPM)
SD_dl_JPM = sqrt(var(dl_price_xts_JPM))
# We evaluate the normality of the stocks
shapiro.test(as.numeric(dl_price_xts_AAPL))
shapiro.test(as.numeric(dl_price_xts_AMZN))
shapiro.test(as.numeric(dl_price_xts_META))
shapiro.test(as.numeric(dl_price_xts_GM))
shapiro.test(as.numeric(dl_price_xts_WMT))
shapiro.test(as.numeric(dl_price_xts_JPM))

hist(dl_price_xts_AAPL, breaks = 50, main = "Histogram of AAPL Log Returns", xlab = "Log Returns AAPL")
qqnorm(dl_price_xts_AAPL); qqline(dl_price_xts_AAPL, col = "red")
hist(dl_price_xts_AMZN, breaks = 50, main = "Histogram of AMZN Log Returns", xlab = "Log Returns AMZN")
qqnorm(dl_price_xts_AMZN); qqline(dl_price_xts_AMZN, col = "red")
hist(dl_price_xts_META, breaks = 50, main = "Histogram of META Log Returns", xlab = "Log Returns META")
qqnorm(dl_price_xts_META); qqline(dl_price_xts_META, col = "red")
hist(dl_price_xts_GM, breaks = 50, main = "Histogram of GM Log Returns", xlab = "Log Returns GM")
qqnorm(dl_price_xts_GM); qqline(dl_price_xts_GM, col = "red")
hist(dl_price_xts_WMT, breaks = 50, main = "Histogram of WMT Log Returns", xlab = "Log Returns WMT")
qqnorm(dl_price_xts_WMT); qqline(dl_price_xts_WMT, col = "red")
hist(dl_price_xts_JPM, breaks = 50, main = "Histogram of JPM Log Returns", xlab = "Log Returns JPM")
qqnorm(dl_price_xts_JPM); qqline(dl_price_xts_JPM, col = "red")

# Evalutate the assets under the MEAN - VARIANCE criterion

assets <- data.frame(
  asset = c("AAPL", "AMZN", "WMT", "GM", "META", "JPM"),
  mean  = c(MEAN_dl_AAPL, MEAN_dl_AMZN, MEAN_dl_META, MEAN_dl_GM, MEAN_dl_WMT, MEAN_dl_JPM),
  var   = c(VAR_dl_AAPL, VAR_dl_AMZN, VAR_dl_META, VAR_dl_GM, VAR_dl_WMT, VAR_dl_JPM)
)

assets

assets_sorted <- assets[order(-assets$mean, assets$var), ]
assets_sorted

# TO IMPROVE 

library(ggplot2)

ggplot(assets, aes(x = var, y = mean, label = asset)) +
  geom_point(color = "steelblue", size = 3) +
  geom_text(vjust = -1) +
  labs(title = "Mean–Variance Plot of the 6 Assets",
       x = "Variance (Risk)",
       y = "Mean Return") +
  theme_minimal()

# We now insert the UTILITY function 𝑢(𝑥) = 𝐸[𝑥̇] − α/2 σ^2(𝑥)

a1 = 0.05
assets$exp_utility_0.05 <- assets$mean - (a1/2) * assets$var
a2 = 0.5
assets$exp_utility_0.5 <- assets$mean - (a2/2) * assets$var
print(assets)

Utility_sorted = assets [order(-assets$exp_utility_0.05, assets$exp_utility_0.5),]
Utility_sorted

# exercise 2 
COV_AAPLAMZN = cov(dl_price_xts_AAPL, dl_price_xts_AMZN)
COV_AAPLMETA = cov(dl_price_xts_AAPL, dl_price_xts_META)
COV_METAAMZN = cov(dl_price_xts_META, dl_price_xts_AMZN)
COV_GMWMT = cov(dl_price_xts_GM, dl_price_xts_WMT)
COV_GMJPM = cov(dl_price_xts_GM, dl_price_xts_JPM)
COV_WMTJPM = cov(dl_price_xts_WMT, dl_price_xts_JPM)

# We create a covariance Matrix 
Covariance_Matrix1 = matrix(
c(
  VAR_dl_AAPL, COV_AAPLAMZN, COV_AAPLMETA,
  COV_AAPLAMZN, VAR_dl_AMZN, COV_METAAMZN,
  COV_AAPLMETA, COV_METAAMZN, VAR_dl_META
),
nrow = 3,
ncol = 3,
byrow = TRUE
)

# We find the inverse Matrix
Covariance_Matrix1_INVERT = solve (Covariance_Matrix1)
print(Covariance_Matrix_INVERT)
ones = rep(1, nrow(Covariance_Matrix1))
C1 = as.numeric(t(ones) %*% Covariance_Matrix1_INVERT %*% ones)
print(C1)
W_MVP1 = (Covariance_Matrix1_INVERT %*% ones) / C1
print(W_MVP1)
# We repeat for the second portfolio 

Covariance_Matrix2 = matrix(
  c(
    VAR_dl_GM, COV_GMWMT, COV_GMJPM,
    COV_GMWMT, VAR_dl_WMT, COV_WMTJPM,
    COV_GMJPM, COV_WMTJPM, VAR_dl_JPM
  ),
  nrow = 3,
  ncol = 3,
  byrow = TRUE
)

Covariance_Matrix2_INVERT = solve (Covariance_Matrix2)
print(Covariance_Matrix2_INVERT)
C2 = as.numeric(t(ones) %*% Covariance_Matrix2_INVERT %*% ones)
print(C2)
W_MVP2 = (Covariance_Matrix2_INVERT %*% ones) / C2
print(W_MVP2)
# Expected return and variance of portfolios based on MEAN VARIANCE 
μ1 = c(MEAN_dl_AAPL, MEAN_dl_AMZN, MEAN_dl_META)
μ2 = c(MEAN_dl_GM, MEAN_dl_WMT, MEAN_dl_JPM)
expected_portfolio_return1 = as.numeric(t(W_MVP1) %*% μ1)
expected_portfolio_return2 = as.numeric(t(W_MVP2) %*% μ2)
portfolio_variance1 <- as.numeric(t(W_MVP1) %*% Covariance_Matrix1 %*% W_MVP1)
portfolio_variance2 <- as.numeric(t(W_MVP2) %*% Covariance_Matrix2 %*% W_MVP2)

Portfolio_Returns = data.frame(
  Expected_Return = c(expected_portfolio_return1, expected_portfolio_return2),
  Variance = c(portfolio_variance1, portfolio_variance2),
  Portfolio = c("MVP1", "MVP2")
  )

print(Portfolio_Returns)

# Plot to visualise the MVPs 

ggplot(Portfolio_Returns, aes(x = Variance, y = Expected_Return, label = Portfolio)) +
  geom_point(color = "steelblue", size = 3) +
  geom_text(vjust = -1) +
  labs(title = "Mean–Variance Plot for MVPs",
       x = "Variance (Risk)",
       y = "Expected Return") +
  theme_minimal()
# We now compute the whole portfolio including all the assets 
R_all <- na.omit(merge(
  dl_price_xts_AAPL,
  dl_price_xts_AMZN,
  dl_price_xts_META,
  dl_price_xts_GM,
  dl_price_xts_WMT,
  dl_price_xts_JPM
))
colnames(R_all) <- c("AAPL","AMZN","META","GM","WMT","JPM")
print(R_all)
SIGMA = cov(R_all)
SIGMA
dim(SIGMA)
Invert_SIGMA = solve(SIGMA)
ones_all = rep(1, nrow(SIGMA))
Invert_SIGMA
C3 = as.numeric(t(ones_all) %*% Invert_SIGMA %*% ones_all) 
C3
W_MVPALL = as.vector(Invert_SIGMA %*% ones_all / C3)
μALL = as.numeric(colMeans(R_all))# vector of mean returns per asset
EXP_RETURN_PER_STOCK = W_MVPALL*μALL
EXPECTED_RETURN_ALL = sum(EXP_RETURN_PER_STOCK)
EXPECTED_RETURN_ALL
# We reconvert into a matrix for the computation of the variance - Correlation included 
W_MVPALL_MATRIX = Invert_SIGMA %*% ones_all / C3
W_MVPALL_MATRIX
VARIANCE_ALL = as.numeric(t(W_MVPALL_MATRIX) %*% SIGMA %*% (W_MVPALL_MATRIX))
VARIANCE_ALL
# We compute the Variance without correlation - "risk" only
SIGMA_COR_zero = matrix(
  c(
    VAR_dl_AAPL, 0, 0, 0, 0, 0,
    0, VAR_dl_AMZN, 0, 0, 0, 0,  
    0, 0, VAR_dl_META, 0, 0, 0,
    0, 0, 0, VAR_dl_GM, 0, 0,
    0, 0, 0, 0, VAR_dl_WMT, 0,
    0, 0, 0, 0, 0, VAR_dl_JPM
  ),
  nrow = 6,
  ncol = 6,
  byrow = TRUE
)
SIGMA_COR_zero
Invert_SIGMA_COR_zero = solve(SIGMA_COR_zero)
C4 = as.numeric(t(ones_all) %*% Invert_SIGMA_COR_zero %*% ones_all)
W_MVPZERO = Invert_SIGMA_COR_zero %*% ones_all / C4
W_MVPZERO
EXPECTED_RETURN_ZERO_COR = t(W_MVPZERO) %*% μALL
EXPECTED_RETURN_ZERO_COR
# We create a matrix with the previously foudn variances 
var_vector <- c(
  VAR_dl_AAPL,
  VAR_dl_AMZN,
  VAR_dl_META,
  VAR_dl_GM,
  VAR_dl_WMT,
  VAR_dl_JPM
)

names(var_vector) <- c("AAPL", "AMZN", "META", "GM", "WMT", "JPM")
var_vector
str(var_vector)
VARIANCE_ZERO_COR = var_vector %*% W_MVPZERO
VARIANCE_ZERO_COR

# we compute the tangeant portfolio 
# we define the risk free rate 
rf = 0.0001
Excess_Return = μALL - rf*ones_all 
Tangeant Porfolio = Invert_SIGMA * Exc
