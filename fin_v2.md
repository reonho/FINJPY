---
title: "BT2201 Finance Project Notebook"
output:
  html_notebook: default
  html_document:
    df_print: paged
  pdf_document: default
---

```{r setup, include=FALSE} 
knitr::opts_chunk$set(warning = FALSE, message = FALSE) 
```

This notebook quantifies the analysis we conducted to arrive at our optimal asset allocations.


## 1. Determine Investment Policy

Which products should be offered to a typical Japanese retail investor


### Risk Appetite

The typical Japanese retail investor is risk averse. Many older Japanese investors lived through the Asian Financial Crisis which saw a huge asset bubble and subsequent crash in Japanese financial markets. Those who invested money right at the peak of the asset bubble would have lost a considerable amount of money. Japan also entered many years of deflation afterwards.

Therefore, a typical Japanese retail investor is distinct from an American or European retail investor in that they have experienced more market downside and less economic euphoria. We expect that they will have a more conservative risk appetite and less tolerance for portfolio drawdown.


### Objective Function

Given the conditions described above, we choose to adopt the *Efficient Risk* or *Markowitz Portfolio*

**Fix Return and Minimise Risk**

(OR find an objective function from here)
https://pyportfolioopt.readthedocs.io/en/latest/MeanVariance.html 

For a start, maximise Sharpe:

$max \ \frac{E(R) - r_f}{SD(R)}$


### Amount
As stated in the preamble, investable amount is **JPY 100,000**.

## 2. Security Analysis

Pull data for ticker described above and exchange rates for currency conversion.
```{r}
require(quantmod)
require(priceR)
```

```{r}
#USA Denominated

us_etf = c(
  'BIV', # US INTERMEDIATE BOND
  'VCSH',# US INVESTMENT GRADE BOND
  'BNDX',# GLOBAL BOND ETF
  'VNQ', #US REIT
  'VT', #GLOBAL STOCK'
  'SPY', #US STOCK (SPDR)
  'VTI',
  'VEA',
  'VGT',
  'XLF',
  'XLE',
  'XLI',
  'XLY',
  'VAW',
  'XLP',
  'VPU',
  'HEWJ',
  'DXJ',
  'DZZ',
  'GLD',
  'BDRY',
  'REM',
  'IYR',
  'EWJ',
  'IVLU',
  'DXJ',
  'BBJP',
  'VWOB'
  
)

# Japan Denominated

j_etf = c(
  '1328.T',
  '1343.T',
  '1345.T',
  '1540.T',
  '1555.T',
  '1615.T',
  '2561.T'
  
)

# AU Denominated

au_etf = c(
  'HVST.AX'
)


getSymbols(us_etf, auto.assign=T)
getSymbols(j_etf, source='yahooj', auto.assign=T)
getSymbols(au_etf, auto.assign=T)
```
```{r}
# USD to JPY Currency
usd_jpy = historical_exchange_rates('USD', to='JPY', start_date='2007-01-01', end_date='2021-11-09')
usd_jpy = as.xts(usd_jpy$one_USD_equivalent_to_x_JPY, order.by=as.Date(usd_jpy$date))

# AUD to JPY Currency
aud_jpy = historical_exchange_rates('AUD', to='JPY', start_date='2007-01-01', end_date='2021-11-09')
aud_jpy = as.xts(aud_jpy$one_AUD_equivalent_to_x_JPY, order.by=as.Date(aud_jpy$date))
```
```{r}
# Convert us_tickers to JPY
us_tickers = sapply(us_etf, FUN=function(x) eval(as.symbol(x)))
us_tickers = sapply(us_tickers, FUN=function(x) x[,6]=x[,6]*usd_jpy) 

# Get JP returns
j_tickers = sapply(j_etf, FUN=function(x) eval(as.symbol(x)))
j_tickers = sapply(j_tickers, FUN=function(x) x[,6]) 

# Convert AU tickers to JPY
# au_tickers = sapply(au_etf, FUN=function(x) eval(as.symbol(x)))
# au_tickers = sapply(au_tickers, FUN=function(x) x[,6]=x[,6]*aud_jpy) 
au_tickers=c(HVST.AX$HVST.AX.Adjusted*aud_jpy)

```

```{r}
# Convert into xts dataframe and merge

us_tickers = do.call(merge, us_tickers)
names(us_tickers) = us_etf

j_tickers = do.call(merge, j_tickers)
names(j_tickers) = j_etf

names(au_tickers)= 'HVST.AX'

all_tickers = cbind(us_tickers, j_tickers)

all_tickers = cbind(all_tickers, au_tickers)
names(all_tickers)[29:35] = j_etf

```
```{r}
means = c()
sds = c()
returns = c()

for (t in names(all_tickers)) {
  ticker = all_tickers[,t]
  prices_monthly <- to.monthly(ticker, name=t, indexAt = "lastof", OHLC = F)
  return <- ROC(prices_monthly)
  returns = cbind(returns,return)
  means = c(means, mean(return, na.rm=T))
  sds = c(sds, sd(return, na.rm=T))  
  
}

```
```{r}
df = data.frame(names(all_tickers), means, sds)
colnames(df) = c('ETF','Mu_Monthly', 'Sigma_Monthly')
df
```
```{r}
library(fPortfolio)
library(purrr)
returns = data.frame(returns)

colnames(returns) <- unlist(map(colnames(returns), function(x){unlist(strsplit(x,'[.]'))[1]}))
covdata = cov(returns, use = "complete.obs")

corrdata = cor(returns, use='complete.obs')
heatmap(corrdata)
```
# Portfolio Construction

```{r}
# Create in-sample and out-sample splits, portfolio weights are based on in-sample
train = as.timeSeries(head(returns[complete.cases(returns),], 149))
test = as.timeSeries(tail(returns[complete.cases(returns),], 30))
data = portfolioData(train)
pSpec = portfolioSpec()

```
```{r}
n = ncol(returns)
setNFrontierPoints(pSpec) <- 20
longFrontier <- portfolioFrontier(data = data, spec = pSpec)
col = qualiPalette(n, "Set1")
tailoredFrontierPlot(longFrontier, return = "mean", risk = "Cov")
grid()
singleAssetPoints(longFrontier, col='red')

```

```{r}
```{r fig.height=10, fig.width=15}

########################################
#      Efficient Frontier              #
########################################




########################################
#      LOW RISK PORTFOLIO              #
########################################

annual_target = 0.07
target_return = (1+annual_target)**(1/12) - 1

# optimise weights
setTargetReturn(pSpec) = target_return
port = efficientPortfolio(data=data, spec=pSpec)

port

# performance on test set
opt_weights <- port@portfolio@portfolio$weights
opt_weights <- as.data.frame(opt_weights)
returns_timeseries <- test
opt_portfolio <- returns_timeseries*opt_weights$opt_weights[match(names(returns_timeseries), rownames(opt_weights))][col(returns_timeseries)]
cumulative_returns = as.xts(cumprod(rowSums(opt_portfolio)+1))


# plot equal weight portfolio
n = length(opt_weights$opt_weights)
equal_weights <- rep(1/n, n)
weights = cbind(equal_weights, opt_weights)
opt_portfolio <- returns_timeseries*weights$equal_weights[match(names(returns_timeseries), rownames(weights))][col(returns_timeseries)]
cumulative_returns_equal = as.xts(cumprod(rowSums(opt_portfolio)+1))


plot(cumulative_returns_equal, type='l', ylim=c(.5,1.8))
lines(cumulative_returns,  col='red')

# ROLLING BACKTEST with rebalancing
lowRiskStrategy <- function(data, spec, constraints, backtest){
  Parameters <- getStrategyParams(backtest)
  strategyPortfolio <- efficientPortfolio(data, spec, constraints)
  return (strategyPortfolio)
}

# get full period returns for equal weighted
full_period_return = as.timeSeries(all_returns[complete.cases(all_returns),])
equal_weight_full_period = full_period_return *weights$equal_weights[match(names(full_period_return), rownames(weights))][col(full_period_return)]
backtest_data = cbind(as.timeSeries(rowSums(equal_weight_full_period)), full_period_return)

backtest_data = backtest_data[complete.cases(backtest_data), ]

defaultBacktest <- portfolioBacktest()
setWindowsHorizon(defaultBacktest) <- "36m"
setSmootherInitialWeights(defaultBacktest) <- rep(1/20, 20)
setStrategyFun(defaultBacktest) <- lowRiskStrategy

pfs = portfolioBacktesting(formula = TS.1~N225+YJ1343+BIV+VCSH+BNDX+VNQ+VT+SPY+YJ1555+HVST+VTI+VEA+VGT+XLF+XLE+XLI+XLY+VAW+XLP+VPU, spec=pSpec, data = backtest_data, backtest = defaultBacktest, trace=FALSE)
pfs_smooth = portfolioSmoothing(pfs)
netPerformance(pfs_smooth)
backtestPlot(pfs_smooth, cex = 0.6, font = 1, family = "mono")
```
```
