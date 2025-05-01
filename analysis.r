library(quantmod)               # For getSymbols(), Ad(), and price data
library(PerformanceAnalytics)  # For return calculation and performance metrics
library(PortfolioAnalytics)    # For portfolio optimization
library(ROI)                   # Optimization engine
library(ROI.plugin.quadprog)  # Solver for quadratic programming
library(ROI.plugin.glpk)
library(ROI.plugin.symphony)
library(CVXR)
library(corrplot)              # For correlation matrix plots

# Define ticker symbols
tickers <- c("AAPL", "MSFT", "GOOGL", "JPM", "XOM")

# Get 5 years of monthly price data
getSymbols(tickers, 
           src = "yahoo", 
           from = Sys.Date() - 365*5)

prices <- do.call(merge, lapply(tickers, function(sym) Ad(get(sym))))
colnames(prices) <- tickers

returns <- na.omit(Return.calculate(prices))

# Performance summary
charts.PerformanceSummary(returns, 
                          main = "Daily Returns",
                          legend.loc = "topleft")

# Risk metrics
table.Stats(returns)
table.AnnualizedReturns(returns)
table.DownsideRisk(returns)

VaR(returns, p = 0.95, method = "historical")
ES(returns, p = 0.95, method = "historical")

# Correlation matrix
cor_matrix <- cor(returns)
corrplot::corrplot(cor_matrix, 
                   method = "circle", 
                   type = "upper")

# Covariance matrix
cov_matrix <- cov(returns)

# Define Portfolio
port_spec <- portfolio.spec(assets = colnames(returns))

# Add constraints
port_spec <- add.constraint(portfolio = port_spec, 
                            type = "full_investment")

port_spec <- add.constraint(portfolio = port_spec, 
                            type = "long_only")

# Add objectives
port_spec <- add.objective(portfolio = port_spec, 
                           type = "risk", 
                           name = "StdDev")

port_spec <- add.objective(portfolio = port_spec, 
                           type = "return", 
                           name = "mean")

# Optimization
opt_result <- optimize.portfolio(R = returns, 
                                 portfolio = port_spec, 
                                 optimize_method = "ROI", 
                                 trace = TRUE)

opt_result
chart.Weights(opt_result)

# Equal-weight portfolio returns
equal_weights <- rep(1 / ncol(returns), ncol(returns))
ew_returns <- Return.portfolio(returns, 
                               weights = equal_weights)

# Optimized portfolio returns
opt_weights <- extractWeights(opt_result)
opt_returns <- Return.portfolio(returns, 
                                weights = opt_weights)

# Combine and plot
combined <- merge.xts(ew_returns, opt_returns)
colnames(combined) <- c("Equal Weight", "Optimized")
charts.PerformanceSummary(combined, main = "Performance: Equal Weight vs Optimized")


# Efficient frontier
ef <- create.EfficientFrontier(returns, 
                               port_spec, 
                               type = "mean-StdDev")

chart.EfficientFrontier(ef, 
                        match.col = "StdDev", 
                        n.portfolios = 25, 
                        main = "Efficient Frontier")


# Ensure 'opt_weights' is a numeric vector
opt_weights <- as.numeric(opt_weights)

# Portfolio volatility 
port_volatility <- sqrt(sum(opt_weights * (cov_matrix %*% opt_weights)))

# Risk contribution for each asset
risk_contrib <- (cov_matrix %*% opt_weights) * opt_weights / port_volatility

# Convert to a data frame
risk_contrib_df <- data.frame(Asset = colnames(returns), RiskContribution = risk_contrib)

print(risk_contrib_df)



# Optimization backtesting
benchmark_returns <- Return.portfolio(R = returns,
                                      weights = equal_weights,
                                      rebalance_on = "years")

colnames(benchmark_returns) <-"benchmark"
table.AnnualizedReturns(benchmark_returns)

base_port_spec <- port_spec

opt_base <- optimize.portfolio.rebalancing(R = returns,
                                           optimize_method = "ROI",
                                           portfolio = base_port_spec,
                                           rebalance_on = "quarters",
                                           training_period = 60,
                                           rolling_window = 60)

base_returns <- Return.portfolio(returns, extractWeights(opt_base))
colnames(base_returns) <- "base"

chart.Weights(opt_base, main = "Unconstrained Weights")


box_port_spec <- port_spec

box_port_spec <- add.constraint(portfolio = box_port_spec,
                                type = "box",
                                min = 0.05, max = 0.4,
                                indexnum = 2)

opt_box = optimize.portfolio.rebalancing(R = returns,
                                         optimize_method = "ROI",
                                         portfolio = box_port_spec,
                                         rebalance_on = "quarters",
                                         training_period = 60,
                                         rolling_window = 60)

box_returns <- Return.portfolio(returns, extractWeights(opt_box))
colnames(box_returns) <- "box"

chart.Weights(opt_box, main = "Box Weights")


ret <- cbind(benchmark_returns, base_returns, box_returns)

table.AnnualizedReturns(ret)

