# Toy example
library(tidyverse)
# X_t = X_t???1 + 2 where t denotes time.
# X_0 = 10

X <- numeric(10)
X[1] = 10

for (i in 2:10) {
  X[i] <- X[i - 1] + 2
}
print(X)

Time <- 1:10
X_time <- tibble(X, Time)
ggplot(X_time, aes(x = Time, y = X)) +
  geom_line()

# X_t = 0.5 * X_t???1 + 5, with X_0 = 12
X <- numeric(10)
X[1] = 12

for (i in 2:10) {
  X[i] <- ( 0.5 * X[i - 1] ) + 5
}

Time <- 1:10
X_time <- tibble(X, Time)
ggplot(X_time, aes(x = Time, y = X)) +
  geom_line()

# Global Carbon Stocks
# S - stock of carbon, E - emission,  t - time
# St = S_t???1 + E_t ??? dS_t???1
# dS_t???1 is how much of the stock is absorbed.
# atmospheric stock of carbon is ~ 750 Gt.
# d can be approximated by 0.005.
# If current emissions continue at the same level for the next 500 years...

S <- numeric(500)
E <- numeric(500)
S[1] <- 750
E[1] <- 6.3

for (i in 2:500) {
  E[i] <- E[i - 1]
  S[i] <- S[i - 1] + E[i - 1] - (0.005 * S[i - 1])
}
Time <- 1:500
C_scenario <- tibble(S, Time, E)
ggplot(C_scenario, aes(x = Time, y = S)) +
  geom_line()

#  different growth rates of emissions over the next 50 years
#  If emissions grow at 1.4% a year over the next 50 years,
#  stay at that level for the next 450 years

CO2 <- function(growth = 1.014) {
  S <- numeric(500)
  E <- numeric(500)
  S[1] <- 750
  E[1] <- 6.3
  
  for(i in 2:50) {
    E[i] <- E[i - 1] * growth
    S[i] <- S[i - 1] + E[i - 1] - (0.005 * S[i - 1])
  }
  
  for(i in 51:500) {
    E[i] <- E[i - 1]
    S[i] <- S[i - 1] + E[i - 1] - (0.005 * S[i - 1])
  }
  Time <- 1:500
  C_scenario <- tibble(S, Time, E)
  
  ggplot(C_scenario, aes(x = Time, y = S)) +
    geom_line()
}
CO2(1.014)

# Fish stock







