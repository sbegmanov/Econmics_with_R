library(tidyverse)

# reproduced AJR' results Assenova and Regele
ajr <- read_csv("complete.data.iv.csv")
attach(ajr)
# We motivate the issue of the relationship between institutions and # growth with a scatter plot of the measure of output versus the # measure of institutions, and OLS regressions of output on # institutions plus other variables. The measure of output is
# log GDP per capita PPP in 1995 (logpgp95) and the measure of # institutions is average risk of expropriation, 1985-1995 (avexpr).
# we only consider the data used by AJR, with the dummy variable # called baseco set to 1:

ajrb <- ajr %>%
  filter(baseco == 1)
attach(ajrb)

library(ggrepel)
ggplot(ajrb, aes(x = avexpr, y = logpgp95, label = shortnam)) +
  geom_text_repel(size = 3) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

library(texreg)
ol1 <- lm(logpgp95 ~ avexpr,data = ajrb)
ol2 <- lm(logpgp95 ~ avexpr + lat_abst, data = ajrb)
ol3 <- lm(logpgp95 ~ avexpr + lat_abst + asia + africa + other, data = ajrb)

screenreg(list(ol1, ol2, ol3), caption = "OLS Regressions, Dependent variable
          is log GDP per capita in 1995", caption.above = TRUE)

# we expect reverse causality and there are omitted variables.
# To overcome these issues, AJR propose a theory that can be summarized as follows:
# (potential) settler mortality -> settlements -> early institutions ->
# -> current institutions -> current performance
# They use settler mortality as an instrument for institutions. We plot the reduced
# form relationship between income and settler mortality.

ggplot(ajrb, aes(x = logem4, y = logpgp95, label = shortnam)) +
  geom_text_repel(size = 3) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

olA <- lm(avexpr ~ logem4, data = ajrb)
olB <- lm(avexpr ~ logem4 + lat_abst, data = ajrb)
screenreg(list(olA, olB), caption = "Dependent variable is Average Protection
          Against Expropriation Risk from 1985 to 1995", caption.above = TRUE)

# proceed with the IV estimation, using the AER package
library(AER)

mod1 <- ivreg(logpgp95 ~ avexpr | logem4, data = ajrb)
mod2 <- ivreg(logpgp95 ~ avexpr + lat_abst | logem4 + lat_abst, data = ajrb)
mod3 <- ivreg(logpgp95 ~ avexpr + lat_abst + asia + africa + other | logem4 +
                lat_abst + asia + africa + other, data = ajrb)


library(texreg)
screenreg(list(mod1, mod2, mod3),
          include.rsq = FALSE, include.adjrs = FALSE, include.rmse = FALSE,
          caption = "IV regressions of log GDP per capita",
          caption.above = TRUE)

#### Geography and Growth
# geography directly affects income levels and
# geography affects institutions, and institutions and incomes have 
# a bi-directional relationship
# geography is correlated with settler mortality and with economic output

p1 <- ggplot(ajrb, aes(x = meantemp, y = logem4)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

p2 <- ggplot(ajrb, aes(x = meantemp, y = logpgp95)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

library(gridExtra)
grid.arrange(p1, p2, ncol = 2)

# Geography is also manifest in the continent-wise variation in settler mortality
ggplot(ajrb, aes(x = continent, y = logem4)) +
  geom_boxplot() +
  coord_flip()

ggplot(ajrb, aes(col = continent, x = logem4, y = avexpr, shape = continent)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(linetype = continent))

ggplot(ajrb, aes(col = continent, x = logem4, y = logpgp95, shape = continent)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(linetype = continent))

#### Exclusion restriction simulation
# issue of testing the exclusion restriction of the instrumental variable
# We cannot directly regress the outcome variable on the endogenous variable and
# the instrumental variable to test whether the instrument is valid
# Even if the instrumental variable is valid, such a regression can show that the
# coefficient of the instrumental variable is statistically significant, 
# even though we # are controlling for the endogenous variable.
# Let U be a common cause of X and Y: X <- U -> Y
# and let Z be a valid instrument of Z, i.e. Z -> X -> Y

U <- rnorm(300)
Z <- rnorm(300, mean = 3)
X <- Z + 2 * U + rnorm(300)
Y <- X + 2 * U

modex1 <- lm(Y ~ X)

library(AER)
modex2 <- ivreg(Y ~ X | Z)
screenreg(list(modex1, modex2), caption = "OLS of Y on X (Model 1),
          IV regression of Y on X (Model 2)", caption.above = TRUE)
# The regression of Y on X gives us a biased estimate, but the instrumental variable
# is successful in getting the true effect of X on Y

# Z is a valid instrumental variable, the regression
# of Y on X and Z gives us a statistically significant coefficient of Z

modex3 <- lm(Y ~ X + Z)
screenreg(list(modex3), caption = "Regression of X on Z",
          caption.above = TRUE)
# A variable that is external is necessarily exogenous.























