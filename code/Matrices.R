# simple statistics with vectors
x <- c(1, 2, 3, 4, 5)
length(x)
sum(x)
# transpose of x
t(x)

ones <- c(rep(1, length(x)))
length(ones)
sum(ones)
t(ones)

# %*% - multiplication - to repeat
t(x) %*% ones
sum(x)
t(x) %*% ones / length(x)
mean(x)

# var_x = [1 / (n - 1)] * Sum^n_,i=1 (x_i _ avr(x))^2
# deviation of each value of x from mean(x)
dev.x <- x - mean(x);
dev.x
t(dev.x) %*% dev.x
Var_calc_x <- t(dev.x) %*% dev.x / (length(x) - 1)
Var_calc_x
var(x)


### Matrix Operations
# ncol is number of columns
A <- matrix(c(4, 3, 6, 4), ncol = 2)
print(A)

B <- matrix(c(2, 5, 6, 1), ncol = 2)
print(B)

M <- B + A
print(M)

t(A)
t(B)

# 4w + 6z = 14, 3w + 4z = 10
# A*D = C D - column vectors with w, z elements, C - solumn vectors - 10, 14
# A*D = C, D = A^-1 * C
C = c(14, 10)
D <- solve(A) %*% C  # w = 2, z = 1
print(D)

A %*% D

### Example: Poverty Rate and Relative Income
Poverty_rate <- c(7, 8, 8, 11, 14, 16, 17, 19, 21, 22, 24, 31, 33, 33, 34)
Relative_income <- c(93, 99, 83, 97, 96, 91, 78, 90, 78, 76, 84, 68, 76, 74, 69)
Y <- Poverty_rate
One <- c(rep(1, length(Y)))
X <- cbind(One, Relative_income)

# mean and variance of relative income
RI <- Relative_income
t(RI) %*% One / length(RI)
mean(RI)

var(RI)
dev.RI <- RI - mean(RI)
Var_calc_RI <- t(dev.RI) %*% dev.RI / (length(RI) - 1)
print(Var_calc_RI)

PR <- Poverty_rate
t(PR) %*% One / length(PR)
mean(PR)

dev.PR <- PR - mean(PR)
Var_calc_PR <- t(dev.PR) %*% dev.PR / (length(PR) - 1)
print(Var_calc_PR)
var(PR)

# regression of the poverty rate on relative income
lm(Poverty_rate ~ Relative_income)

data_eu <- data.frame(Poverty_rate = Poverty_rate, Relative_income = Relative_income)
library(ggplot2)
ggplot(data_eu, aes(x = Relative_income, y = Poverty_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# matrix formula for least squares to estimate the regression coefficients
# Coeff = (X^T*X)^-1*X^T*Y

matcoeff <- solve(t(X) %*% X) %*% t(X) %*% Y
print(matcoeff)
