library(Lahman)
library(dplyr)
library(ggplot2)

print(LahmanData) # list of data frames
View(LahmanData)
# We use Teams data frame
head(Teams)
View(Teams)


# R - runs, RA - runs allowed, W - Wins, L- Loss
# wpct = R^1.83 / (R^1.83 + RA^2) gives better R 
# wpct = winning percentage

mydata <- Teams %>%
  select(yearID, lgID, teamID, W, L, R, RA) %>%
  filter(yearID == 2014, lgID == "AL") %>%
  mutate(wpct = R^2/(R^2 + RA^2), expwin = round(wpct * (W + L)), diff = W - expwin)

head(mydata)

ggplot(mydata, aes(expwin, W)) +
  geom_point() +
  stat_smooth(method = "lm")

mod <- lm(W ~ expwin, data = mydata)
print(mod)

summary(mod)

