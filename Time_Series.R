library(forecast)

Years5 <- ts(c(138, 91, 54, 222, 56), start = 2000, frequency = 1)
print(Years5)
autoplot(Years5)

### Example:Air Passengers
data("AirPassengers")

APass <- AirPassengers
str(APass)
class(APass)
head(APass)
autoplot(APass)

# Seasonal plot of air passengers per month, 1958 to 1960
ggseasonplot(window(APass, start = 1958))

# Air passengers per month arranged by month
ggsubseriesplot(APass)

### Example: Stock Market Volatility
# TS is the daily New York Stock Exchange stock price index from 1990 to 2005.
data("NYSESW", package = "AER")

nyse <- 100 * diff(log(NYSESW))
str(nyse)
library(tidyverse)
library(forecast)

autoplot(nyse) +
  geom_smooth(method = "loess")

# To graph volatility, we estimate the standard deviation at monthly intervals
library(xts)

nyse5 <- as.xts(nyse)
nyse_sd_monthly <- apply.monthly(nyse5, sd)

# monthly SD of daily percentage changes
autoplot(nyse_sd_monthly) +
  geom_hline(yintercept = 1.5, linetype = "dashed")

#### Example: Inflation and unemployment
# data relating to inflation and unemployment
data("USMacroSW", package = "AER")

# ts.intersect helps us avoid blank rows
# When we take the difference of the log of the consumer price index (cpi) 
# we get the rate of inflation. We multiply by 100 to get percentages, 
# and since the data is # quarterly we multiply by 4 to get rates of inflation 
# on a quarterly basis.

usm <- ts.intersect
usm <- ts.intersect(USMacroSW, 4 * 100 * diff(log(USMacroSW[, "cpi"])))

# add the name infl (inflation) to the column names.

colnames(usm) <- c((colnames(USMacroSW)), "infl")
colnames(usm)

library(tidyverse)
library(tsbox) # convert TS object to a TS tibble

usm2 <- ts_tbl(usm)
glimpse(usm2)
table(usm2$id)

# extract inflation and unemployment, and join them
usm_infl <- usm2 %>%
  filter(id == "infl")

usm_unemp <- usm2 %>%
  filter(id == "unemp")

usm3 <- usm_infl %>%
  left_join(usm_unemp, by = "time")
glimpse(usm3)

# lubridate package which works with dates and times
library(lubridate)

usm4 <- usm3 %>%
  mutate(Year = year(time) - 1900,
         after_70 = ifelse(Year < 70, "70before", "after70"),
         decade = ifelse(Year < 60, "1950s",
                         ifelse(Year < 70, "1960s",
                                ifelse(Year < 80, "1970s",
                                       ifelse(Year < 90, "1980s",
                                              ifelse(Year < 100, "1990s",
                                                     "2000s")))))) %>%
  rename(inflation = value.x, unemployment = value.y)

# tradeoff between inflation and unemployment
ggplot(usm4, aes(x = unemployment, y = inflation)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ after_70)

ggplot(usm4, aes(x = unemployment, y = inflation, label = Year)) +
  geom_path(arrow = arrow(angle = 10, type = "closed", ends = "last")) +
  facet_wrap(~ decade)

GGi1 <- ggplot(usm4, aes(x = unemployment, y = inflation, label = Year, colour = decade)) +
  geom_path(arrow (angle = 10, type = "closed", "last"))

GGi2 <- ggplot(usm4, aes(x = unemployment, y = inflation, label = Yeaer, colour = decade)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

library(gridExtra)
grid.arrange(GGi1, GGi2, ncol = 1)


#### Example: Historical Unemployment Data
# macro data related to the United Kingdom dating back
# to 1870, available at the book website.We will plot the unemployment data
library(readxl)
library(forecast)
library(tidyverse)

MacroData15 <- read.csv("MacroData15.csv")

unemp <- ts(MacroData15$Ur, start = 1870)
autoplot(unemp)

p1 <- autoplot(unemp) +
  annotate("text", x = 1916, y = -0.01, label = "World\nWar I") +
  geom_vline(xintercept = c(1914, 1918), linetype = "dotted")
print(p1)

p2 <- p1 + 
  annotate("text", x = 1934, y = 0.17, label = "Great\nDepression") +
  ylim(-0.02, 0.18) +
  annotate("text", x = 1942, y = - 0.01, label = "World\nWar II") +
  geom_vline(xintercept = c(1939, 1945), linetype = "dotted") +
  annotate("text", x = 1973, y = 0.12, label = "Oil\ncrises") +
  annotate("text", x = 1984, y = 0.15, label = "Mrs T")
print(p2)

p3 <- p2 +
  annotate("text", x = 1992, y = 0.13, label = "Leave\nERM") +
  annotate("text", x = 2008, y = 0.11, label = "Fin\nCrisis")
print(p3)

#### Basic TM models
# White Noise
# In a white noise process, the mean and variance are constant, 
# and there is no correlation over time.
library(tidyverse)
library(forecast)

white <- rnorm(300)
white_ts <- ts(white, start = 1)
autoplot(white_ts)

# relationship between the white noise observations and the first lag
white2 <- white[-300]
white2_L <- white[-1]
white_lag <- tibble(white2, white2_L)
glimpse(white_lag)

# white noise versus first lag of white noise
ggplot(white_lag, aes(x = white2_L, y = white2)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

cor(white2_L, white2)

# The lack of correlation between present and lagged values of white_ts is 
# reflected in the plot of the autocorrelation function (ACF):
ggAcf(white_ts)

# Another way of simulating white noise is with the arima.sim function 
# (arima is autoregressive integrated moving average)
autoplot(arima.sim(300, model = list(ar = 0, ma = 0)))

# estimation of white noise model with Arima function
mod_w <- Arima(white_ts, order = c(0, 0, 0))
print(mod_w)

mean(white_ts)
var(white_ts)

white_yt <- arima.sim(300, model = list(ar = 0, ma = 0), mean = 3, sd = 2)
autoplot(white_yt)

mod_wyt <- Arima(white_yt, order = c(0, 0, 0))
print(mod_wyt)

### autoregressive model
# A simple example of an autoregressive process is:
# Today = 0.5 * yesterday + white_noise

xar <- numeric(300)
w <- rnorm(300)
for ( t in 2:300) {
  xar[t] <- 0.5 * xar[t - 1] + w[t]
}

xar_ts <- ts(xar, start = 1)
autoplot(xar_ts)
ggAcf(xar_ts)

# use Arima() to fit a model
mod1 <- Arima(xar_ts, order = c(1, 0, 0))
print(mod1)

# use the checkresiduals() function to check the residuals. 
# if the residuals resemble white noise, our fit is good
checkresiduals(mod1)
# Ljung-Box test p-value is just above 0.05 so we do not reject the hypothesis
# that the residuals are independently distributed.

# generate an autoregressive process using the arima.sim function
xar2 <- arima.sim(list(ar = -0.9), n = 300)
autoplot(xar2)
ggAcf(xar2)
ggPacf(xar2)

### Random walk
# A random walk model is an example of a non-stationary process
# Today = yesterday + noise
# generate random walk

x <- numeric(300)
w <- rnorm(300)

for (t in 2:300) {
  x[t] <- x[t - 1] + w[t]
}

x_ts <- ts(x)
autoplot(x_ts)
ggAcf(x_ts)

# We get white noise if we difference a random walk
autoplot(diff(x_ts))
ggAcf(diff(x_ts))

# generate another two random walks and then plot three random walks together
x2 <- numeric(300)
w <- rnorm(300)
for (t in 2:300) {
  x2[t] <- x2[t - 1] + w[t]
}

x3 <- numeric(300)
w <- rnorm(300)
for (t in 2:300) {
  x3[t] <- x3[t - 1] + w[t]
}

RW_ts <- ts(cbind(x3, x2, x))
autoplot(RW_ts)

### Moving average
# Today = noise + 0.3 * yesterday's noise
xma <- numeric(300)
xma[1] <- 0
w <- rnorm(300)

for (t in 2 :300){
  xma[t] <- w[t] + 0.3 * w[t - 1]
}

xma_ts <- ts(xma)
autoplot(xma_ts)
ggAcf(xma_ts)

# fit a model
summary(Arima(xma_ts, order = c(0, 0, 1)))

### Autoregressiv moving average
# An autoregressive moving average process (arma) combines autoregressive 
# and moving average components.

arma1 <- arima.sim(n = 200, list(order = c(1, 0, 1), 
                                 ar = c(0.6), ma = c(0.4))) + 20
autoplot(arma1)
ggAcf(arma1)

# fit a model, check residuals
modarma <- Arima(arma1, order = c(1, 0, 1))
print(modarma)

checkresiduals(modarma)

# auto.arima function, which uses the Hyndman-Khandakar 
# algorithm to fit an arima model of suitable order

modarma.auto <- auto.arima(arma1)
print(modarma.auto)

checkresiduals(modarma.auto)

### Example: Forecasting inflation
# auto.arima function which incorporates an algorithm
library(forecast)
data("USMacroSW", package = "AER")

usm <- ts.intersect
usm <- ts.intersect(USMacroSW, 4 * 100 * diff(log(USMacroSW[, "cpi"])))

colnames(usm) <- c((colnames(USMacroSW)), "infl")
colnames(usm)

Inflat <- usm[, "infl"]
str(Inflat)

library(gridExtra)

gr1 <- autoplot(Inflat)
gr2 <- ggAcf(Inflat)
grid.arrange(gr1, gr2, ncol = 2)

# acf indicates non-stationarity as it decays very slowly. We can use a formal
# test. In the Dickey-Fuller test, we test whether beta_1 = 1 
# in the equation Yt = beta_0 + beta_1 * Y_t???1 + u_t
# Augmented Dickey-Fuller test is better-it augments the Dickey-Fuller test
# by lags of the difference of Y

library(tseries)
adf.test(Inflat)

# null hypothesis that a unit root is present was not rejected, 
# we difference inflation to achieve stationarity

Infl_diff <- diff(Inflat)
autoplot(Infl_diff)
ggAcf(Infl_diff)
adf.test(Infl_diff)

# first-order autoregressive model,
# Z_t = beta_0 + beta_1 * Z_t???1 + u_t
# we can get a forecast for Z for a period ahead with
# Z_T+1 = beta^EST_0 + beta^EST_1 * Z_T
# where EST denotes estimate.
# forecasts improve with more lags than one.
# auto.arima which is an algorithm that selects a suitable arima model

mod <- auto.arima(Inflat, D = 0, max.Q = 0, max.P = 0)
print(mod)
checkresiduals(mod)

forecast(mod, level = 95)

mod %>%
  forecast(h = 6) %>%
  autoplot()

# a very short and long series might give a good forecast
# If the environment has changed, we may choose to use only a portion
# of the historical data.

mod2 <- auto.arima(window(Inflat, start = 1992), D = 0, max.Q = 0, max.P = 0)
print(mod2)
checkresiduals(mod2)
forecast(mod2, level = 95)

mod2 %>%
  forecast( h = 10) %>%
  autoplot()

### Cointegration
### Simulating spurious regression
# Regressing two non-stationary variables that are actually 
# not related can give us spurious results.
library(forecast)

set.seed(29)
x.col <- rnorm(100)
y.col <- rnorm(100)

for (i in 2:100) {
  x.col[i] <- x.col[i - 1] + rnorm(1)
  y.col[i] <- y.col[i - 1] + rnorm(1)
}

mod.col <- lm(y.col ~ x.col)
summary(mod.col)

resid.col <- mod.col$resid
# separate stochastic trends in x.co1 and y.co1 lead to spurious regression
spur <- ts(cbind(x.col, y.col))
autoplot(spur)
ggAcf(ts(resid.col))

## Simulating cointegration
set.seed(46)
x.co2 <- y.co2 <- rw <- numeric(300)
rw[1] <- 2
for (i in 2:300) {
  rw[i] <- rw[i - 1] + rnorm(1)
  x.co2[i] <- rw[i - 1] + rnorm(1)
  y.co2[i] <- rw[i - 1] + rnorm(1)
}

co2.ts <- ts(x.co2, y.co2)

# When we regress the two variables, 
# we get residuals with minimal autocorrelation

mod.co2 <- lm(y.co2 ~ x.co2)
summary(mod.co2)

# We can test for cointegration formally with po.test(); 
# for x.co2 and y.co2 the null hypothesis of no cointegration is rejected.
library(tseries)

co2.b <- as.matrix(cbind(x.co2, y.co2))
po.test(co2.b)

co1.b <- as.matrix(cbind(x.col, y.col))
po.test(co1.b)

#### Example: Federal funds and bond rate
#  ffr - federal funds rate (interest rate on overnight loans between banks)
# br - 3-yr bond rate (interest rate on a financial asset to be held for 3 yrs)
devtools::install_github("ccolonescu/POE5Rdata")

# activate package
library(POE5Rdata)

usa.ts <- ts(usdata5, start = c(1954, 8), end = c(2016, 12), frequency = 12)
autoplot(usa.ts[, "ffr"])
autoplot(usa.ts[, "br"])

# two series move together, test for cointegration
cobind <- as.matrix(cbind(usa.ts[, "ffr"], usa.ts[, "br"]))
po.test(cobind)
# null hypothesis of no cointegration is rejected

ffr_ts <- usa.ts[, "ffr"]
br_ts <- usa.ts[, "br"]
modc <- lm(br_ts ~ ffr_ts)

# regress br on ffr
library(texreg)

screenreg(list(modc), caption = "Regression of bond rate on the federal
          funds rate.", caption.above = TRUE)

#### Example: Dynamic causal effects of weather
# dynamic causal effect of freezing degree days on orange juice prices in Florida
library("dynlm")
library("AER")
data("FrozenJuice", package = "AER")
head(FrozenJuice)

# price - price of frozen orange juice concentrate
# ppi - producer price index
# fdd - number of freezing degree days
# freezing degree days measure captures both whether
# temperature went below freezing and by how much

autoplot(FrozenJuice, facet = TRUE)
# initial regression of the percentage change in prices in a month on the
# number of freezing degree days in that month

fm_dyn <- dynlm(d(100 * log(price / ppi)) ~ fdd, data = FrozenJuice)
coeftest(fm_dyn, vcov = vcovHC(fm_dyn), type = "HC1")

# One more freezing degree day during a month increases the price of orange juice
# concentrate in that month by 0.47%.
# distributed lag regression
fm_dyn2 <- dynlm(d(100 * log(price / ppi)) ~ L(fdd, 0:6),
                 data = FrozenJuice)
pack <- coeftest(fm_dyn2, vcov = vcovHC(fm_dyn2), type = "HC1")

library(broom)
library(knitr)
kable(tidy(pack), digits = 2, caption = "Effects of frozen degree days
      on orange juice prices")

# An extra freezing degree day is estimated to increase the orange juice concentrate
# price in the next month by 0.15%, two months later by 0.06% and three months later
# by 0.07%











