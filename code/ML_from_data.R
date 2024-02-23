library(tidyverse)

# simple example with synthetic data
# y - nonlinear func of x

x <- seq(from = 0, to = 11, length.out = 100)
y <- x * (10 - x) + 3 * rnorm(100)
gam1 <- tibble(x, y)

# y vs x -> lack of fit
ggplot(gam1, (aes(x = x, y = y))) +
  geom_point() +
  geom_smooth(method = "lm")

# poloynomial regression fit
ggplot(gam1, (aes(x = x, y = y))) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2))

# with a loess smoother
ggplot(gam1, (aes(x = x, y = y))) +
  geom_point() +
  geom_smooth(method = "loess")

# to get some broad intuition about loess, fit seperate lines to regions of x
ggplot(gam1, (aes(x = x, y = y, shape = cut(x, 5)))) +
  geom_point() +
  geom_smooth(method = "lm")

#### Example: GAMS with wages data
# GAMs - are generalized additive models. A GAM allows us to extend the multiple
# linear regression model. If we have a regression model:
# y_i = beta_0 + beta_1 * x_i1 + beta_2 * x_i2 + theta_i
# GAM as: y_i = beta_0 + f_1(x_i1) + f_x(x_i2) + theta_i
# replace each linear component beta_j * x_ij with a smooth non-linear func f_j(x_ij)

library(ISLR)
data("Wage", package = "ISLR")
str(Wage)

ggplot(Wage, aes(x = age, y = wage)) +
  geom_point(alpha = 0.5, col = "grey70") +
  geom_smooth(method = 'gam')


ggplot(Wage, aes(y = wage, x = factor(education))) +
  geom_boxplot() +
  coord_flip()

ggplot(Wage, aes(y = wage, x = factor(year))) +
  geom_boxplot() +
  coord_flip()
# fit a generalized additive model with gam()
library(gam)

Wage$fac_year <- factor(Wage$year)
Wage$fac_education <- factor(Wage$education)
gamMod <- gam(wage ~ lo(age) + fac_year + fac_education, data = Wage)
plot(gamMod, se = TRUE, pch = ".")

### Example: Housing in Texas
library(forecast)
library(ggplot2)
data("txhousing")
texas <- txhousing
str(texas)

# focus on Abilene city
rm(txhousing)
library(tidyverse)

tex_abil <- texas %>%
  filter(city == "Abilene")

ggplot(tex_abil, aes(date, sales)) +
  geom_line() +
  geom_smooth(method = loess)

# convert the sales data for Abilene to a time series (from seasonality)
tex_sales_ts <- ts(tex_abil$sales, frequency = 12, start = 2000)

ggseasonplot(window(tex_sales_ts, start = 2011))

# Seasonal and Trend decomposition using Loess(STL)
# TS data is decomposed into trend, seasonal and remainder components
autoplot(stl(tex_sales_ts, s.window = 7))






















