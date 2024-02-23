add2 <- function(x) {
  x + 2
}

print(add2(x = 2))

# default value in function
mult2 <- function(M = 3){
  2 * M
}
print(mult2())

Scomp <- function(S){
  (2*(S^2)) + (3*S) + 1
}
print(Scomp(333))

# curve function - y = 2 - (x + 2)^2
y <- curve(expr = 2 - (x +2)^2, from = -7, to = 3)
plot(y)

# y = 1/(x - 2) + 3
curve(expr = 1 / (x - 2) + 3, from = 5, to = 5)

# y = (x - 3)(x + 7) from x = -15 to x = 15
y <- curve(expr = (x - 3)*(x + 7), from = -15, to = 15)
print(y)
plot(y)

### Statistical Loss Functions
# The mean of values of a variable J can be viewed as the result of minimizing 
# the squared loss deviations (Paolino 2017):
# And the median of the values of a variable J can be viewed as the result of
# minimizing the absolute loss deviations (Paolino 2017):

J <- c(1, 2, 3, 3, 4)
J2 <- c(1, 2, 3, 3, 9)
mean(J)
median(J)
mean(J2)
median(J2)

LS1 <-  curve(
    (J[1] - x)^2 +
    (J[2] - x)^2 +
    (J[3] - x)^2 +
    (J[4] - x)^2 +
    (J[5] - x)^2,
     1, 7, ylab = "Loss")

LS2 <- curve(
  (J2[1] - x)^2 +
  (J2[2] - x)^2 +
  (J2[3] - x)^2 +
  (J2[4] - x)^2 +
  (J2[5] - x)^2,
  1, 7, add = TRUE, lty = 2, ylab = "Loss")

LS1 <-  curve(
  abs(J[1] - x)^2 +
  abs(J[2] - x)^2 +
  abs(J[3] - x)^2 +
  abs(J[4] - x)^2 +
  abs(J[5] - x)^2,
  1, 7, ylab = "Loss")

LS2 <- curve(
  abs(J2[1] - x)^2 +
  abs(J2[2] - x)^2 +
  abs(J2[3] - x)^2 +
  abs(J2[4] - x)^2 +
  abs(J2[5] - x)^2,
  1, 7, add = TRUE, lty = 2, ylab = "Loss")

### Supply and Demand
# An initial inverse demand curve: pD = (125 - 6q)/8
# A shifted inverse demand curve: pD = (150 - 6q)/8
# A supply curve: pS = (12 + 2q)/5

y <- curve((125 - 6 * x) / 8, 0, 30, ylim = c(0, 20))
y <- curve((150 - 6 * x) / 8, 0, 30, lty = 2, add = TRUE)
y <- curve((12 + (2 * x)) / 5, add = TRUE)

library(tidyverse)

Oil <- readxl::read_xlsx("data/oil.xlsx")
# parsed with specification

cols(
  Year = col_double(),
  Price_nominal = col_double(),
  Price_2020 = col_double()
)

ggplot(Oil, aes(x = Year, y = Price_2020)) +
         geom_line() +
         scale_x_continuous(limits = c(1861, 2020),
                            breaks = seq(1860, 2020, by = 20))

# Cobb-Douglas Production Function - f(x_1, x_2) = A*x^a_1 * x^b_2
library(mosaic)
library(manipulate)

plotFun(A * (L ^ 0.7) * (K ^ 0.3) ~ L & K,
        A = 5, filled = FALSE,
        xlim = range(0, 21),
        ylim = range(0, 100))

plotFun(A * (L ^ 0.7) * (K ^ 0.3) ~ L & K,
        A = 5, filled = FALSE,
        xlim = range(0, 21),
        ylim = range(0, 100),
        surface = TRUE)


# micEcon package, 140 French apple producers(1986)
data("appleProdFr86", package = "micEcon")
dat <- appleProdFr86
rm(appleProdFr86)

# Cap denotes capital (including land); Lab labour; and Mat intermediate materials. 
# Input quantities (q) are calculated from the costs (v) and price indexes (p):

dat$qCap <- dat$vCap / dat$pCap
dat$qLab <- dat$vCap / dat$pLab
dat$qMat <- dat$vMat / dat$pMat

ggplot(dat, aes(y = log(qOut), x = log(qCap))) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(dat, aes(y = log(qOut), x = log(qLab))) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(dat, aes(y = log(qOut), x = log(qMat))) +
  geom_point() +
  geom_smooth(method = "lm")

# Cobb-Douglas production function and tabulate with texreg package
prodCD <- lm(I(log(qOut)) ~ log(qCap) + log(qLab) + log(qMat), data = dat)

library(texreg)
texreg::screenreg(list(prodCD), caption = "Dependent variable is log(qOut)", 
       caption.above = TRUE)

# Fish, growth, harvest equations over time, Numerical Simulation
# Use x to denote fish stock, G to denote fish growth and H to denote harvest.
# x_t = x_t???1 + G_t???1 ??? H_t???1
# G follows a logistic function:
# G_t???1 = rx_t???1[1 ??? (x_t???1/K)]
# H is a fraction h of the fish stock:
# H_t???1 = hx_t???1

logistic <- function(r = 0.05, xinit = 40, h = 0.02) {
  K <- 100
  x <- numeric(101)
  gth <- numeric(101)
  H <- numeric(101)
  x[1] <- xinit
  gth[1] <- 0
  
  for (i in 2:101) {
    xbyk <- x[i - 1] / K
    gth[i - 1] <- r * x[i - 1] * (1 - xbyk)
    H[i - 1] < h * x[i - 1]
    x[i] <- x[i - 1] + gth[i - 1] - H[i - 1]
  }
  library(tidyverse)
  Time <- 1:101
  xtab <- tibble(gth, H, x, Time)
  xtab <- xtab[1:100, ]
  
  gg1 <- ggplot(xtab, aes(y = gth, x = x)) +
    geom_line()
  gg2 <- ggplot(xtab, aes(y = H, x = Time)) +
    geom_line()
  gg3 <- ggplot(xtab, aes(y = x, x = Time)) +
    geom_line()
  mylist <- list(gg1, gg2, gg3)
  return(mylist)
}

logistic(r = 0.3, xinit =  20, h = 0.05)
logistic(r = 2.6, xinit = 20, h = 0.05)

# Example: North See Herring
#  S - fish stock, r - intrinsic rate of growth, L -  its carrying capacity
# S_t = S_t???1 + r S_t???1(1 ??? (S_t???1/L))
# If St???1 = 0 or L, the growth is zero.

S = numeric(15)
S[1] = 2325000
r = 0.8
L = 3200000

for (t in 2:5) {
  S[t] <- S[t - 1] + (S[t - 1] * r) * (1 - S[t - 1] / L)
}
# fish stock increases until it is equal to the carrying capacity
Time <- 1:15
SandTime <- data.frame(S, Time)
ggplot(SandTime, aes(x = Time, y = S)) +
  geom_point()

# Examine the fish stock with fishing
# Fishing harvest depends on the fish stock S2 and fishing capital, K.
# K_t+1 = K_t + n * (aK^b???1_t * S^g_t ??? c_t /p_t )
# S_t+1 = S_t + r * S_t * (1 ??? S_t /L) ??? a * K^b_t * S^g_t
# The equation on the top represents the adjustment of capital to profit-higher
# profits lead to an expansion of capital. We had used the lower equation earlier;
# it represents the biological growth and harvest of fish.

S2 = numeric(15)
K = numeric(15)
S2[1] = 2325000
K[1] = 120
r = 0.8
L = 3200000
a = 0.06157
b = 1.356
g = 0.562
n = 0.1
c <- c(190380, 195840, 198760, 201060, 204880, 206880, 215220, 277820, 382880,
       455340, 565780, 686240, 556580, 721640, 857000)
p <- c(232, 203, 205, 214, 141, 128, 185, 262, 244, 214, 384, 498, 735, 853, 1415)

for (t in 2:15) {
  S2[t] <- S2[t - 1] + (S2[t - 1] * r) * (1 - S2[t - 1] / L) - 
    a * K[t - 1] ^ b * S2[t - 1] ^ g
  K[t] <- K[t - 1] + (n * (a * (K[t - 1] ^ (b - 1)) * (S2[t - 1] ^ g) - c[t - 1] / p[t - 1]))
}

# K vs S2
KandS2 <- data.frame(K, S2)
ggplot(KandS2, aes(x = S2, y = K)) +
  geom_path(arrow = arrow())

# S vs Time
S2andTime <- data.frame(S2, Time)
ggplot(S2andTime, aes(x = Time, y = S2)) +
  geom_line()

# Example: Conrad's Model of a Stock Pollutant
# Wastes may accumulate over time, and this is what we call a stock pollutant

# Commodity Residual Transformation Function(CRTF)
# an industry that produces commodities, and wastes are produced
# along with these commodities.
# F(Q_t, S_t ) = 0
# Q - commodity, S - waste, t - time
# (Q_t ??? m^2) ??? n * S_t = 0 (specific form CRTF)
# m, n - certain values

library(tidyverse)

m <- 10
n <- 10
Q <- 10:20
S <- ((Q - m) ^ 2) / m

qplot(Q, S) +
  geom_line()

# Stock Pollutant, accumulation of the stock pollutant Z
# Z_t+1 ??? Z_t = ???w * Z_t + NS_t

TimeSpan <- 20
Z <- numeric(TimeSpan)
Z[1] <- 1400
gamma <- 0.2
S <- 3
N <- 100
N * S / gamma

for (i in 2:TimeSpan) {
  Z[i] <- Z[i - 1] - (gamma * Z[i - 1]) +
    (N * S)
}
qplot(1:TimeSpan, Z) +
  geom_line()

# Firm's Choice of Commodity Q Given a Tax on Waste S
# pQ_t - revenue of price-taking firm in each period
# tau_tS_t - cost of emitting waste
# assumed to max reveue in each period
# Qt = (np / (2tau_t )) + m

p <- 200
m <- 10
Q_static <- numeric(20)
tau <- seq(from = 100, to = 400, length.out = 20)
Q_static <- (n * p / (2 * tau )) + m
qplot(tau, Q_static) +
  geom_line()

# regulator can choose tau to influence Q
# regulator solves: SUM^infty_t=0 p^t(pNQ_t - cZ^2_t) subject to constraints
# optimal steady state Q is solved
# Q* = sqrt(n^2p(delta + gamma)gamma /(4cN) + m)^3
# delta = discount rate

n <- 10
p <- 200
delta <- c(0.025, 0.05, 0.075, 0.1, 0.125)
gamma <- 0.2
c <- 0.02
N <- 100
m <- 10

Num <- (n^2) * p * (delta + gamma) * gamma
Denom <- 4 * c * N
Qfrac <- (Num / Denom) ^ (1/3)
Qstar <- Qfrac + m
print(Qstar)

qplot(delta, Qstar) +
  geom_line()

Tau_star <- n * p / (2 * (Qstar - m))
qplot(delta, Tau_star) +
  geom_line()

# Simulate X_t = 0.5 * X_t???1 + 5,  X_0 = 12
X <- numeric(10)
X[1] = 12

for(i in 2:10) {
  X[i] <- 0.5 * X[i - 1] + 5
}
print(X)

Time <- 1:10
X_time <- tibble(X, Time)
ggplot(X_time, aes(x = Time, y = X)) +
  geom_line()




