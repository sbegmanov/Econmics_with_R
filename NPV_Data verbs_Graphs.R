# Toy example: Net present value
Amount <- 121
discount_rate <- 0.10
time <- 2

Net_present_value <- Amount / (1 + discount_rate) ^ time
print(Net_present_value)

# NPV of several sums of money
Cost_benefit_profile <- c(-150, 135, 140)
time_profile <- c(0, 1, 2)

Cost_benefit_present_value_profile <- 
  Cost_benefit_profile / ( 1 + discount_rate ) ^ time_profile

Net_present_value <- sum(Cost_benefit_present_value_profile)
print(Net_present_value)
print(round(Net_present_value, digits = 0))

Three <- c(3, 3, 3)
Two <- 2
Five <- Three + Two
print(Five)
# but
Mix <- c(2, 9)
ThreeAndMix <- Three + Mix # doesn't work

##### Tidyverse approach
library(tidyverse)

surv_id <- c(1, 2, 3, 4, 5, 6)
payment <- c(1000, 700, 600, 1200, 800, 500)
hours <- c(7, 5, 3, 6, 7, 4)
gender <- c("F", "M", "F", "M", "M", "M")
age <- c(28, 52, 37, 35, 59, 43)

labour <- tibble(surv_id, payment, hours, gender, age)
print(labour)
glimpse(labour)

write_csv(labour, "labour.csv")
labour2 <- read_csv("labour.csv")

## Parsed with column specification:
cols(
  surv_id = col_double(),
  payment = col_double(),
  hours = col_double(),
  gender = col_character(),
  age = col_double()
)

labour$gender
labour$gender[2:3]
labour[1, ]
labour[, 2]

# f(x, y) -> x %>% f(,y)
labour_filter <- labour %>%
  filter(gender == "F")
print(labour_filter)
# same result
labour_filter <- filter(labour, gender == "F")

labour_mutate <- labour %>%
  mutate(wage = payment / hours)
print(labour_mutate)

labour_arrange <- labour %>%
  arrange(hours)
print(labour_arrange)

labour_select <- labour %>%
  select(hours, gender)
print(labour_select)

labour_summary <- labour %>%
  group_by(gender) %>%
  summarize(mean = mean(hours))
print(labour_summary)


gg1 <- ggplot(data = labour_mutate, aes(x = age, y = wage))
print(gg1)

gg2 <- gg1 +
  geom_point()
print(gg2)

gg3 <- gg1 +
  geom_point(
    aes(colour = gender)
  )
print(gg3)

gg4 <- gg2 +
  facet_wrap(~ gender)
print(gg4)

# linear model
age_wage_fit <- lm(wage ~ age, data = labour_mutate)
print(age_wage_fit)

gg5 <- gg2 +
  geom_smooth( method = "lm")
print(gg5)
