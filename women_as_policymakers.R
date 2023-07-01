library(devtools)
library(tidyverse)
require(usethis)
install_github("kosukeimai/qss-package", build_vignettes = TRUE)
data("women", package = "qss")
glimpse(women)
attach(women)

# GP = Gram Panchayat

women %>%
  group_by(reserved) %>%
  summarize(count_res = n(),
            mean_female = mean(female),
            mean_water = mean(water))

ggplot(women, aes(y = water, x = factor(reserved))) +
  geom_boxplot() +
  coord_flip()

dat <- data.frame(Y = women$water,
                  Z = women$reserved,
                  cluster = women$GP)
head(dat)

# randomization inference
library(ri2)
library(texreg)

declaration <- with(dat, {
  declare_ra(clusters = cluster)
})

print(declaration)

ri2_out <- conduct_ri(Y ~ Z,
                      sharp_hypothesis = 0,
                      declaration = declaration,
                      data = dat
                      )
summary(ri2_out)
plot(ri2_out)

# linear model, estimate cluster robust SD errors
women$reserved <- factor(women$reserved)

mod_water_r <- lm_robust(water ~ factor(reserved),
                         clusters = GP, data = women)
print(mod_water_r)

screenreg(list(mod_water_r),
          caption = "Effect of women on water",
          ci.force = T, ci.test = NULL,
          caption.above = TRUE)








