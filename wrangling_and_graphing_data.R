library(tidyverse)
data("anscombe")

ans <- anscombe
str(ans)
glimpse(ans)

ans %>%
  summarize(mean.x1 = mean(x1),
            mean.x2 = mean(x2),
            mean.y1 = mean(y1),
            mean.y2 = mean(y2))

ans %>%
  summarize(sd.x1 = sd(x1),
            sd.x2 = sd(x2),
            sd.y1 = sd(y1),
            sd.y2 = sd(y2))

ans %>%
  summarize ( mean.x3 = mean(x3),
              mean.x4 = mean(x4),
              mean.y4 = mean(y4),
              mean.y3 = mean(y4))

mod1 <- lm(y1 ~ x1, data = ans)
mod2 <- lm(y2 ~ x2, data = ans)
mod3 <- lm(y3 ~ x3, data = ans)
mod4 <- lm(y4 ~ x4, data = ans)

# tabulate the regression results
library(texreg)

texreg(list(mod1, mod2),
       custom.model.names = c("mod1", "mod2"),
       caption = "Regressions of y1 on x1 and y2 on x2",
       caption.above = TRUE)

ggplot(ans, aes(x = x1, y = y1)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


ggplot(ans, aes(x = x2, y = y2)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

texreg(list(mod3, mod4),
       custom.model.names = c("mod3", "mod4"),
       caption = "Regressions of y3 on x3 and y4 on x4",
       caption.above = TRUE)

ggplot(ans, aes(x = x3, y = y3)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

ggplot(ans, aes(x = x4, y = y4)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


### Carbon and Livelihoods Data
ifri <- read_csv("data/ifri_car_liv.csv")
str(ifri)
ifri[79:81, ]

# only 80 rows of data
ifri <- ifri[1:80, ]

ifri <- ifri %>%
  rename(carbon = zbio, liveli = zliv)

ggplot(ifri, aes(x = liveli)) +
  geom_histogram()

ggplot(ifri, aes(y = liveli)) +
  geom_boxplot() +
  coord_flip()

ggplot(ifri, aes(x = carbon)) +
  geom_histogram()

ggplot(ifri, aes(y = carbon)) +
  geom_boxplot() +
  coord_flip()

# carbon vs livelihoods
ggplot(ifri, aes(x = liveli, y = carbon)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

ggplot(ifri, aes(x = factor(ownstate), y = carbon)) +
  geom_boxplot() +
  coord_flip()

ggplot(ifri, aes(x = factor(rulematch), y = liveli)) +
  geom_boxplot() +
  coord_flip()

ggplot(ifri, aes(x = factor(rulematch), y = carbon)) +
  geom_boxplot() +
  coord_flip()

ggplot(ifri, aes(x = lnfsize, y = carbon)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

ggplot(ifri, aes(x = lnfsize, y = liveli)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


# The distribution of carbon is higher when rule match is 1. 
# When rule match is 1, there is a better perception of the rules.


# A variable that combines ownership(state or community) and rule match.

ifri2 <- mutate(ifri, f_own_rule =
                  ifelse(ownstate == 1 & rulematch == 0, "State_low",
                         ifelse(ownstate == 1 & rulematch == 1, "State_high",
                                ifelse(ownstate == 0 & rulematch == 1, "Com_high",
                                       "Com_low"))))
ggplot(ifri2, aes(x = liveli,
                  y = carbon,
                  size = lnfsize,
                  colour = f_own_rule)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


### WDI data on CO2 and per capita income
library(WDI)
# new_wdi_cache <- WDIcache()
WDIsearch("gdp.*capita.*PPP")
wdi_data <- WDI(indicator = c("NY.GDP.PCAP.PP.KD", "EN.ATM.CO2E.PC"),
                start = 2010, end = 2010, extra = TRUE)

library(tidyverse)

wdi_data <- wdi_data %>%
  filter(region != "Aggregates")

wdi_data <- wdi_data %>%
  rename(GDPpercap = NY.GDP.PCAP.PP.KD,
         Emit_CO2percap = EN.ATM.CO2E.PC)

# save it
write_csv(wdi_data, "wdi_CO2_GDP.csv")

# read it
wdi <- read_csv("wdi_CO2_GDP.csv")
str(wdi)

# Graphing the data
summary(wdi$GDPpercap)
# mean > median

ggplot(wdi, aes(x = GDPpercap)) +
  geom_histogram()

ggplot(wdi, aes(y = GDPpercap, x = region)) +
  geom_boxplot() +
  coord_flip() +
  scale_y_log10()

summary(wdi$Emit_CO2percap)

ggplot(wdi, aes(x = Emit_CO2percap)) +
  geom_histogram()

## stat_bin() using bins = 30, pick better
ggplot(wdi, aes(y = Emit_CO2percap, x = region)) +
  geom_boxplot() +
  coord_flip() +
  scale_y_log10()

gg1 <- ggplot(wdi, aes(x = GDPpercap, y = Emit_CO2percap)) +
  geom_point()
print(gg1)

gg2 <- gg1 +
  geom_smooth(se = FALSE) +
  scale_x_log10() +
  scale_y_log10()
print(gg2)

## geom_smooth() - using method = loess and formula y ~ x

#### Mapping the data
library(maps)

dat_map <- map_data("world")
dim(dat_map)
class(dat_map)
head(dat_map)


ggplot(dat_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")

library(countrycode)

dat_map$ccode <- countrycode(dat_map$region,
                            origin = "country.name",
                            destination = "wb")

wdi$ccode <- countrycode(wdi$country,
                        origin = "country.name",
                        destination = "wb")

# merge two sets of data
merged <- full_join(dat_map, wdi, by = "ccode")

# a map of the global distribution of GDP per capita
ggplot(merged, aes(x = long, y = lat,
                   group = group,
                   fill = log10(GDPpercap))) +
  geom_polygon()

# colour gradient
ggplot(merged, aes(x = long, y = lat,
                   group = group,
                   fill = log10(GDPpercap))) +
  geom_polygon() +
  scale_fill_gradient(low = "green",
                      high = "red")

# similar map for CO2
ggplot(merged, aes(x = long, y = lat,
                   group = group,
                   fill = log10(Emit_CO2percap))) +
  geom_polygon() +
  scale_fill_gradient(low = "green",
                      high = "red")
