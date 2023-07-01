library(tidyverse)

# remove previous objects
rm(list = ls())

Coin_box <- c("H", "T")
print(Coin_box)

# flippping coins in the Coin_box
sample(x = Coin_box, size = 2, replace = TRUE)
# replace = TRUE - each sample is independent
# replace = FALSE - drawing a head ticket first time, the next time -> tail

# using default of sample function, replace = FALSE
sample(x = Coin_box, size = 2)
sample(x = Coin_box, size = 2)

# error
sample(x = Coin_box, size = 3)

# for reproducibility
set.seed(30)
sample(Coin_box, 6, replace = TRUE)
sample(Coin_box, 6, replace = FALSE) # cannot do

### Binomial Distribution
# tails = 0, heads = 1
Box01 <- c(0, 1)
Samp <- sample(Box01, 30, replace = TRUE)
Samp[1:10]
sum(Samp)

Samp <- sample(Box01, 30, replace = TRUE)
Samp[1:10]
table(Samp)
Samp
sum(Samp)

# 30 draws at a time, sum values and 10 times.

sims <- 10 # simulations
sample_size <- 30

# will store the sum in this vector
sum_1 <- numeric(sims)

for (i in 1:sims) {
  Samp <- sample(Box01, sample_size, replace = TRUE)
  sum_1[i] <- sum(Samp)
}

sum_1 <- tibble(sum_1)
print(sum_1)

ggplot(sum_1, aes(x = sum_1)) +
  geom_bar() +
  xlim(5, 25)

##_______________

Box01 <- c(0, 1)
sims <- 1000
sum_1 <- numeric(sims)

for (i in 1:sims) {
  Samp <- sample(Box01, 30, replace = TRUE)
  sum_1[i] <- sum(Samp)
}

sum_1 <- tibble(sum_1)

ggplot(sum_1, aes(x = sum_1)) +
  geom_bar()

# normal distribution function
ggplot(sum_1, aes(x = sum_1)) +
  geom_density(fill = "grey80", linetype = 3) +
  stat_function(fun = dnorm, args = list(mean = mean(sum_1$sum_1),
                                         sd = sd(sum_1$sum_1)),
                linetype = 1)


### Function for binomial distribution
sumbox01fun <- function(Box01 = c(0, 1), sims = 1000, Size = 30)
{
  sum_1 <- numeric(sims)
  
  for (i in 1:sims) {
    Samp <- sample(Box01, Size, replace = TRUE)
    sum_1[i] <- sum(Samp)
  }
  
  sum_1 <- tibble(sum_1)
  
  ggplot(sum_1, aes(x = sum_1)) +
    geom_bar()
}

sumbox01fun(Box01 = c(0, 1, 1), Size = 20, sims = 100)
sumbox01fun(Box01 = c(0, 1, 1, 1), Size = 20, sims = 1000)

### Sampling Distribution
# Six-sided dice simulation
# a box has the tickets 1 to 6 in it, 10,000 each.
Box <- rep(1:6, 10000)
table(Box)
mean(Box)
sd(Box)

# draw 16 tickets at a time
samp_size <- 16
Samp <- sample(Box, size = samp_size)
print(Samp)

sample_mean <- mean(Samp)
print(sample_mean)
sample_sd <- sd(Samp)
print(sample_sd)

# repeat, each sample different
samp_size <- 16
Samp <- sample(Box, size = sample_size)
print(Samp)

sample_mean <- mean(Samp)
print(samp_size)
sample_sd <- sd(Samp)
print(sample_sd)

# draw samples of size 16 a 100 times
set.seed(34)
samp_size <- 16
simuls <- 100
sample_mean <- numeric(simuls)
sample_sd <- numeric(simuls)

for (i in 1:simuls) {
  Samp <- sample(Box, size = samp_size)
  sample_mean[i] <- mean(Samp)
  sample_sd[i] <- sd(Samp)
}

sample_mean_store <- sample_mean
samp_dist <- tibble(sample_mean, sample_sd)

ggplot(samp_dist, aes(sample_mean)) +
  geom_histogram()

paste(" The mean of the sample means is:", sep = " ", round(mean(sample_mean), 2))
sd(sample_mean)
sd(Box) / sqrt(samp_size)

ggplot(samp_dist, aes(x = sample_mean)) +
  geom_density(fill = "grey80", linetype = 2) +
  stat_function(fun = dnorm, args = list(mean = mean(samp_dist$sample_mean),
                                         sd = sd(samp_dist$sample_mean)),
                linetype = 1)

# sampling distribution of the means ~= normal distribution
ggplot(samp_dist, aes(sample_sd)) +
  geom_histogram()

# mean of the distribution of the sample standard deviations ~= standard deviation of the box
mean(sample_sd)

### Function for sampling distribution
samp_dist_mean_fun <- function( Box = rep(1:6, 10000), samp_size = 16, simuls = 100)
{
  sample_mean <- numeric(simuls)
  sample_sd <- numeric(simuls)
  
  for (i in 1:simuls) {
    Samp <- sample(Box, size = samp_size)
    sample_mean[i] <- mean(Samp)
    sample_sd[i] <- sd(Samp)
  }
  
  samp_dist <- tibble(sample_mean, sample_sd)
  
  gg1 <- ggplot(samp_dist, aes(sample_mean)) +
    geom_histogram(bins = 15)
  
  print(gg1)
  print(paste("The mean of the Box is: ", sep = " ", round(mean(Box), 2)))
  print(paste("The standard deviation of the Box is: ", sep = " ", round(sd(Box), 2)))
  print(paste("The mean of the sample means is: ", sep = " ", round(mean(sample_mean), 2)))
  print(paste("The sd of the sample means is: ", sep = " ", round(sd(sample_mean), 2)))
  mean(sample_mean)
  sd(sample_mean)
  print(paste("sd(Box) / sqrt(samp_size) is: ", sep = " ", round(sd(Box) / sqrt(samp_size), 2)))
}

### end of function
samp_dist_mean_fun(Box = rep(1:6, 10000), samp_size = 3, simuls = 1000)
samp_dist_mean_fun(Box = rep(c(1, 1, 1, 2, 2, 6, 9), 10000), samp_size = c(3, 16, 100), simuls = 1000)

### Sampling Distribution for the T-Statistic
# t-statistic = (mean of sample - mean of box) / standard error of mean

Box <- rep(1:6, 10000)
sd(Box)
mean(Box)

# loop contains a line to calculate t-stat
set.seed(34)
samp_size <- 16
simuls <- 1000
se_samp <- numeric(simuls)
sample_tstat <- numeric(simuls)

for (i in 1:simuls) {
  Samp <- sample(Box, size = samp_size)
  se_samp <- sd(Samp) / sqrt(samp_size)
  sample_tstat[i] <- (mean(Samp) - mean(Box)) / se_samp
}

tstat <- tibble(sample_tstat)

ggplot(tstat, aes(x = sample_tstat)) +
  geom_histogram()

ggplot(tstat, aes(x = sample_tstat)) +
  geom_density(fill = "grey80", linetype = 2) +
  stat_function(fun = dt, args = list(df = samp_size - 1), linetype = 1)

# Inference from one sample

Samp <- sample(Box, size = samp_size)
mean(Samp)
mod_Samp <- lm(Samp ~ 1)
summary(mod_Samp) # tstat is high 'cos plotted t-dist from Box

# plot value of tstat in sample to t-dist
ggplot(tstat, aes(x = sample_tstat)) +
  geom_histogram() +
  geom_vline(xintercept = 10, linetype = 2)

# confidence intervals
confint(mod_Samp)

# A level C confidence interval for a parameter is an interval computed from 

Box <- rep(1:6, 10000)
sd(Box)
mean(Box)

Samp <- sample(Box, size = samp_size)
Conf <- confint(lm(Samp ~ 1), level = 0.95)
print(Conf)

set.seed(34)
samp_size <- 16
simuls <- 100
sample_mean <- numeric(simuls)
conf_lo <- numeric(simuls)
conf_hi <- numeric(simuls)

for (i in 1:simuls) {
  Samp <- sample(Box, size = samp_size)
  sample_mean[i] <- mean(Samp)
  
  Conf <- confint(lm(Samp ~ 1), level = 0.95)
  conf_lo[i] <- Conf[1]
  conf_hi[i] <- Conf[2]
}

samp_index <- 1:simuls
Conf_int <- tibble(conf_lo, conf_hi, samp_index, sample_mean)
head(Conf_int, 20)
Conf_int_20 <- Conf_int[1:20,]
Covers <- 1 - ifelse(conf_lo > 3.5 | conf_hi < 3.5, 1, 0)
sum(Covers) / simuls


ggplot(Conf_int_20) +
  geom_pointrange(aes(x = samp_index,
                      y = sample_mean,
                      ymin = conf_lo,
                      ymax = conf_hi)) +
  geom_hline(yintercept = 3.5, linetype = 2)

### Bootstrap

Box <- rep(1:6, 10000)
sd(Box)
mean(Box)
samp_size <- 100

Box_Sample1 <- sample(Box, size = samp_size)
Box_Sample1[1:10]
mean(Box_Sample1)
sd(Box_Sample1)

# The sampling distribution is the distribution of sample means where each sample

set.seed(34)
simuls <- 10000
Sample_mean <- numeric(simuls)
Bootstrap_mean <- numeric(simuls)

for(i in 1:simuls){
  # sample from the sample
  Boot_sample <- sample(Box_Sample1, size = samp_size, replace = TRUE)
  Bootstrap_mean[i] <- mean(Boot_sample)
  
  # sample from the Box
  Sample <- sample(Box, size = samp_size)
  Sample_mean[i] <- mean(Sample)
}

# different aspects of the bootstrap and sampling distributions

Distribution <- c(Bootstrap_mean, Sample_mean)
Type <- c(rep("Bootstrap", simuls), rep("Sampling", simuls))
str(Distribution)
str(Type)

Boot_sim <- tibble(Distribution, Type)
Boot_sim[5, ]

ggplot(Boot_sim, aes(x = Distribution)) +
  geom_histogram(bins = 15) +
  facet_wrap(Type ~ .)

Boot_sim %>%
  group_by(Type) %>%
  summarize(sd = sd(Distribution), mean = mean(Distribution))

# bootstrap percentile
round(quantile(Bootstrap_mean, probs = c(0.025, 0.975)), 2)

#### Function to Understand Bootstrap

Boot_understand_fun <- function(Box = rep(1:6, 10000), Seed = 34,
                                simuls = 1000, samp_size = 100) {
  set.seed(Seed)
  Box_Sample1 <- sample(Box, size = samp_size)
  Sample_mean <- numeric(simuls)
  Bootstrap_mean <- num(simuls)
  
  # loop
  for (i in 1:simuls) {
    Boot_sample <- sample(Box_Sample1, size = samp_size, replace = TRUE)
    Bootstrap_mean[i] <- mean(Boot_sample)
    
    Sample <- sample(Box, size = samp_size)
    Sample_mean[i] <- mean(Sample)
  }
  
  Distribution <- c(Bootstrap_mean, Sample_mean)
  Type <- c(rep("Bootstrap", simuls), rep("Sampling", simuls))
  Boot_sim <- tibble(Distribution, Type)
  Boot_sim
  
  gg_boot <- ggplot(Boot_sim, aes(x = Distribution)) +
    geom_histogram(bins = 15) +
    facet_wrap(Type ~ .)
  print(gg_boot)
  
  print(paste("The mean of the box =", sep = " ", mean(Box)))
  print(paste("The sd of the box =", sep = " ", round(sd(Box), 2)))
  
  Boot_sim_stats <- Boot_sim %>%
    group_by(Type) %>%
    summarize(sd = round(sd(Distribution), 3),
              mean = round(mean(Distribution), 2))
  print(Boot_sim_stats)
  
  print(paste("The quantiles of the"))
  print(paste("sampling distribution of the mean are:"))
  print(quantile(Sample_mean, probs = c(0.025, 0.975)))
  print(paste("The quantile of the bootstrap distribution of the mean are:"))
  #print(quantile(Bootstrap_mean, probs = c(0.025, 0.975)))
}

# end function
Boot_understand_fun()

### Permutation Tests

scores <- c(10, 11, 12, 13, 14, 15, 16, 17, 18, 19)
student <- c("Student1", "Student2","Student3", "Student4", "Student5",
             "Student6", "Student7","Student8", "Student9", "Student10")
Class <- c("A","A", "A", "A","A","B","B","B", "B","B")

Scores <- tibble(scores, student, Class)
print(Scores)

lm(scores ~ Class)
# Class B and Class A scores are identically distributed ?
# shuffle the tickets from the same box (Class A + Class B)

index_Ap <- sample(1:10, 5)
print(index_Ap)
Scores[index_Ap,]
# remaining  5 students in Class B
Scores[-index_Ap,]

# mean differences
mean(scores[index_Ap]) - mean(scores[-index_Ap])

# shuffling is repeated with a loop
iters <- 999
mean_diff_p <- numeric(iters)

for (i in 1:iters) {
  index_Ap <- sample(1:10, 5); index_Ap
  mean_diff_p[i] <- mean(scores[index_Ap]) - mean(scores[-index_Ap])
}
mean_diff_p <- tibble(mean_diff_p)
mean_diff_p

# plot the permutation distribution of mean differences
ggplot(mean_diff_p, aes(x = mean_diff_p)) +
  geom_bar() +
  geom_vline(xintercept = 5)

# abs value of the mean diff is = or > 5
sum_extreme_values <- sum(abs(mean_diff_p) >= 5)
sum_extreme_values

# p-value (add 1 to numerator and denominator for an improved p-value)
p_value <- (sum_extreme_values + 1) / (iters + 1)
paste("The p-value is", sep = " ", round(p_value, 4))


### Example: Verizon
# ILEC & CLEC clients are equally treated?
# if > 1% of the tests significant
library(tidyverse)
library(resample)
data("Verizon")
glimpse(Verizon)

table(Verizon$Group)

ggplot(Verizon, aes(x = Time)) +
  geom_density() +
  facet_wrap(~ Group)

# log transformation of the distribution and make box plots

ggplot(Verizon, aes(x = Group, y = log10(Time + 1))) +
  geom_boxplot() +
  coord_flip()

Verizon %>%
  group_by(Group) %>%
  summarize(mean_T = round(mean(Time), 1),
            t_mean_T = round(mean(Time, trim = 0.25), 1))
# A trimmed mean is a method of averaging that removes a small percentage of 
# the largest and smallest values before calculating the mean.

library(magrittr)

Ver_CLEC <- Verizon %>%
  filter(Group == "CLEC")

Ver_ILEC <- Verizon %>%
  filter(Group == "ILEC")

# trimmed mean for ILECT 10 times > ILEC's
diff_t_mean <- mean(Ver_ILEC$Time, trim = 0.25) - mean(Ver_CLEC$Time, trim = 0.25)
print(diff_t_mean)

# Permutation Test
# randomization of diff of trimmed means
# H_0: repair times are same

length(Verizon$Time)
length(Ver_ILEC$Time)

iters <- 1000
diff_t_perms <- numeric(iters)

for(i in 1:iters) {
  index <- sample(1:1687, size = 1664, replace = FALSE)
  diff_t_perms[i] <- mean(Verizon$Time[index], trim = 0.25) -
    mean(Verizon$Time[-index], trim = 0.25)
}

# plot permutation dist for diff of trimmed means
ggplot() +
  geom_histogram(aes(diff_t_perms)) +
  geom_vline(xintercept = diff_t_mean, linetype = 2)

# p-value diff in trimmed means
p_value <- (sum(diff_t_perms <= diff_t_mean + 1) / (iters + 1))
paste("The p_value is", sep = " ", round(p_value, 4))

# Bootstrapping CI
# CLEC & ILEC skewed -> trimmed mean is good (trims away extreme obs)
# CI related to diff. of trimmed means
# The bootstrap was devised for estimating statistical accuracy of estimators
# such as trimmed means, where there is no neat algebraic formula

CLEC <- Ver_CLEC$Time
ILEC <- Ver_ILEC$Time

length_c <- length(CLEC)
print(length_c)

length_i <- length(ILEC)
print(length_i)

mean(ILEC) - mean(CLEC)
mean(ILEC, trim = 0.25) - mean(CLEC, trim = 0.25)

# sample with replacement
sims <- 10000
diff_mean <- numeric(sims)
diff_trim_mean <- numeric(sims)

for(i in 1:sims) {
  samp_I <- sample(ILEC, length_i, replace = TRUE)
  samp_C <- sample(CLEC, length_c, replace = TRUE)
  
  diff_mean[i] <- mean(samp_I) - mean(samp_C)
  diff_trim_mean[i] <- mean(samp_I, trim = 0.25) - mean(samp_C, trim = 0.25)
}

ggplot() + 
  geom_histogram(aes(diff_mean)) +
  geom_vline(xintercept = mean(diff_mean), linetype = 2)

ggplot() +
  geom_histogram(aes(diff_trim_mean)) +
  geom_vline(xintercept = mean(diff_trim_mean), linetype = 2)

# bootstrap distr of diff. means is skewed, better use trimmed mean
quantile(diff_trim_mean, probs = c(0.025, 0.975))

### Cautionary Example with Synthetic Data

set.seed(15)
sample_size <- 30

xvar <- rnorm(sample_size, mean = 0.1, sd = 1)
noise <- 2 * rnorm(sample_size)
xobs <- xvar + noise

summary(lm(xobs ~ 1))

# The estimate of the mean of xvar using the observed data xobs is 1.03, an order
# of magnitude greater than the true value, and which is statistically significant.


























