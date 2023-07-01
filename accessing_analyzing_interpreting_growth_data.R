#### Example:Growth
library(tidyverse)
library(maddison)
str(maddison)

mad1 <- maddison %>%
  filter(iso2c == "US")
ggplot(mad1, aes(x = year, y = gdp_pc)) +
  geom_line()

mad2 <- maddison %>%
  filter(iso2c == "US" & year >= as.Date("1800-01-01"))
ggplot(mad2, aes(x = year, y = gdp_pc)) +
  geom_line()
  #scale_y_log10()

mad3 <- maddison %>%
  filter(country %in% c("India", "China", "Argentina",
                        "Japan", "United States", "United Kingdom") &
  year >= as.Date("1800-01-01"))
ggplot(mad3, aes(x = year, y = gdp_pc, colour = country)) +
  geom_line() +
  scale_y_log10()

#### Example: Production Model and Cross country Data
# Penn World Tables.
library(tidyverse)
library(pwt9)
data("pwt9.0")

# production model: Y be given by: Y = AK^a L^1âˆ’a
# A - a production parameter, K, L - labour, capital.
# Dividing by L, we get: Y /L = AK^a/L^a or, in per person terms, y = Ak^1/3
# We get a = 1/3 by observation of factor shares of some economies. 
# a = 1/3 and 1 aˆ’ a = 2/3, which is a fair approximation.
# data on k and y, but not A.
# If A = 1, then the model predicts that predicted y, y_predicted = k^1/3.
# If predicted y is more than real y, this implies that the parameter A is less than one.
# the greater the gap between the predicted y and real y, the greater the role of A in
# overall production, and less of per capita capital.
# We now take the model to data and normalize the data with respect to values 
# for the US, which are taken as 1.

pwt <- force(pwt9.0)
rm(pwt9.0)
pwt <- as_tibble(pwt)

pwt2 <- pwt %>%
  filter(year == 2014)

pwt3 <- pwt2 %>%
  select("cgdpo","emp","pop","ck","country","isocode")

# output per worker, and another, capital per worker:
pwt4 <- pwt3 %>%
  mutate(out_per_worker = cgdpo / pop,
         cap_per_worker = ck / pop)

# pull out for the US
pwt5 <- pwt4 %>%
  filter(isocode == "USA")

USout <- pwt5$out_per_worker
UScap <- pwt5$cap_per_worker

# normalize with respect to US values
pwt6 <- pwt4 %>%
  mutate(out = out_per_worker / USout,
         cap = cap_per_worker / UScap,
         pred = cap ^ (1/3))
# We see our data for a few select countries:
pwt7 <- pwt6 %>%
  filter(country %in% c("United States of America","Japan", "Italy", 
                        "India", "Brazil", "Spain"))

pwt7 %>%
  select("country", "out", "pred", "cap")

# predicted vs real output
ggplot(pwt6, aes(x = out, y = pred)) +
  geom_point(col = "grey40") +
  geom_line(aes(x = out, y = out)) +
  scale_x_log10() +
  scale_y_log10()

# there is a large gap between the predicted output and the real
# output, and it is larger for poorer countries - capital per person is less productive

#### Solow Model Simulation
# benchmark model by growth economists
# production Y = A*K^a*L^1-a
# capital accumulation K_t+1 = K_t + s * Y_t - d*K_t

a <- 1/3
A <- 2
L <- 200
Klow <- 0
Khigh <- 4000
Knumber <- 100
K <- seq(from = Klow, to = Khigh, length.out = Knumber)
Y <- A * (K^a) * (L^(1 - a))
Prod <- data.frame(K, Y)
print(Prod)

# Y versus K
ggplot(Prod, aes(x = K, y = Y)) +
  geom_line()

# Solow diagram - shows how savings and depreciation vary with capital
s <- 0.25
S <- s * Y
d <- 0.1
dep <- d * K
Solow <- data.frame(S, dep, K)

ggplot(Solow) +
  geom_line(aes(x = K, y = S)) +
  geom_line(aes(x = K, y = dep))

# if s increases, the Solow diagram and the steady-state values change
s2 <- 0.35
S2 <- s2 * Y
Solow <- data.frame(S, S2, dep, K)

ggplot(Solow) +
  geom_line(aes(x = K, y = S)) +
  geom_line(aes(x = K, y = dep)) +
  geom_line(aes(x = K, y = S2), linetype = "dashed")

# values of Y change over time
d <- 0.1
L <- 200
a <- 1/3
A <- 2
s <- 0.35
d <- 0.1

Kt <- numeric(100)
Kt[1] <- 500

for (i in 2:100) {
  Kt[i] <- Kt[i - 1] + s * (A * (Kt[i - 1]^a) * (L^(1 - a))) - d * Kt[i - 1]
}

Yt <- A * (Kt^a) * (L^(1 - a))
motion <- data.frame(Yt, Kt)
print(motion)

ggplot(motion, aes(x = 1:100, y = Yt)) +
  geom_line()

ls()
rm(list = ls())
ls()

#### Romer Model Simulation
# The Solow model leads to growth that peters out when it depends on capital accumulation, 
# because of diminishing returns.
# Romer leads us to ideas as a source of economic growth. Ideas are different from
# objects. Ideas are characterized by non-rivalry.

# W - ideas (produced, not subject to diminishing returns)
# production function for objects: Y_t = W_t * (1 - l) * L
# t - time, 1 - l - share of total workers L who produce objects
# production function for ideas: W_t = W_t-1 + z_t * W_t-1 * l * L
# z - productivity parameter, l - share of workers who produce ideas

W <- numeric(100)
Y <- numeric(100)
W[1] <- 100
l <- 0.10
L <- 100
z <- 1/500
Y[1] <- W[1] * (1 - l) * L

for( i in 2:100) {
  W[i] <- W[i - 1] + z * W[i - 1] * l * L
  Y[i] <- W[i] * (1 - l) * L
}

Romer <- data.frame(W, Y, Time = 1:100)
library(ggplot2)
ggplot(Romer, aes(x = Time, y = Y)) +
  geom_line()

rm(list = ls())

#### Example: Growth in recent decades
library(WDI)

new_wdi_cache <- WDIcache()

WDIsearch("gdp.*capita.*PPP")

wdi_data <- WDI(indicator = c("NY.GDP.PCAP.PP.KD"),
                start = 1990,
                end = 2017,
                extra = TRUE)
library(tidyverse)

wdi_data <- wdi_data %>%
  filter(region != "Aggregates")

wdi_data <- wdi_data %>%
  rename(GDP_pc = NY.GDP.PCAP.PP.KD)

wdi <- as_tibble(wdi_data)

# plot boxplots of GDP per capita in 1990, 2004 and 2017
wdi_sel <- wdi %>%
  filter(year == 1990 | year == 2004 | year == 2017)

ggplot(wdi_sel, aes(y = GDP_pc, x = factor(year))) +
  geom_boxplot() +
  coord_flip()

# filter 1990 and 2017
wdi_1990 <- wdi %>%
  filter(year == 1990) %>%
  select(country, year, region, GDP_pc, iso3c) %>%
  rename(GDP_pc_1990 = GDP_pc)
str(wdi_1990)

wdi_2017 <- wdi %>%
  filter(year == 2017) %>%
  select(country, year, region, GDP_pc) %>%
  rename(GDP_pc_2017 = GDP_pc)
str(wdi_2017)

wdi_1990_2017 <- wdi_1990 %>%
  left_join(wdi_2017, by = "country")

str(wdi_1990_2017)
print(wdi_1990_2017)

# scatter plot of Ratio of GDP per capita in 2017 to GDP per capita
# in 1990 versus GDP per capita in 1990 (Fig. 11.10). For most countries the Ratio of
# GDP per capita in 2017 to GDP per capita in 1990 lies in the range 1 to 3.

library(ggrepel)
wdi_1990_2017 <- wdi_1990_2017 %>%
  mutate(ratio = GDP_pc_2017 / GDP_pc_1990)

ggplot(wdi_1990_2017, aes(x = GDP_pc_1990,
                          y = ratio,
                          label = iso3c,
                          colour = region.x)) +
  geom_text(size = 3) +
  scale_x_log10() +
  scale_y_log10() +
  theme(legend.position = "bottom")

# We pull out countries with Ratio of GDP per capita in 2017 to GDP per capita in
# 1990 greater than three
library(knitr)

wdi_1990_2017 %>%
  filter(ratio > 3) %>%
  select(ratio, country) %>%
  arrange(desc(ratio)) %>%
  kable(caption = " Ratio of GDP per capita in 2017 to GDP per capita in 1990,
        countries with ratio greater than 3",
        digits = 2)


# We pull out countries with Ratio of GDP per capita in 2017 to GDP per capita in
# 1990 less than 0.95

wdi_1990_2017 %>%
  filter(ratio < 0.95) %>%
  select(ratio, country) %>%
  arrange(desc(ratio)) %>%
  kable(caption = "Ratio of GDP per capita in 2017 to GDP per capita in 1990,
        countries with ratio less than 0.95", digits = 2)

# medians of GDP per capita region-wise
wdi_median <- wdi %>%
  group_by(year, region) %>%
  summarize( reg_med = median(GDP_pc, na.rm = TRUE))

# plot the median GDP per capita by region in recent decades
ggplot(wdi_median, aes(x = year, y = reg_med)) +
  geom_line() +
  scale_y_log10() +
  facet_wrap(~ region, ncol = 2)









 