#### Simple tree example with synthetic data

x.tree <- c(rep(1:5, 20), rep(6:10, 20), rep(11:15, 20))
y.tree <- c(rep(c(0, 1, 0, 0, 1), 20),
            rep(c(1, 0, 1, 1, 1), 20),
            rep(c(0, 0, 0, 0, 1), 20))
xy.tree <- data.frame(x.tree, y.tree)

library(tidyverse)

ggplot(xy.tree, aes(x = x.tree, y = y.tree)) +
  geom_jitter(height = 0.1, width = 0.1) +
  geom_smooth()

library(rpart)

xy.tree_t <- rpart(y.tree ~ x.tree,
                   data = xy.tree,
                   method = "class")
class(xy.tree_t)

# rpart.plot package to plot the tree
library(rpart.plot)

prp(xy.tree_t, extra = 1)

#### Example: Arsenic in Wells in Bangladesh

wells <- read.delim("wells.dat", header = TRUE, sep = "")

library(tidyverse)

# fit a logistic regressions of switch on distance and arsenic
fit <- glm(switch ~ dist + arsenic,
           family = binomial(link = logit), data = wells)

library(texreg)

screenreg(list(fit), caption = "Logistic regression of switch on distance and arsenic",
       caption.above = TRUE)

library(visreg)

visreg(fit, "dist", scale = "response")
visreg(fit, "arsenic", scale = "response")

# fit a classification tree
library(rpart)

Bang <- rpart(switch ~ dist + arsenic, data = wells, method = "class")

library(rpart.plot)

prp(Bang, extra = 1)

# If arsenic is less than 0.56, then the prediction is of not switching.

ggplot(wells, aes(x = dist, y = arsenic, colour = factor(switch))) +
  geom_point() +
  geom_hline(yintercept = 1.1) +
  geom_hline(yintercept = 0.56) +
  geom_vline(xintercept = 69) +
  geom_vline(xintercept = 82)


#### Example: Home mortgage disclosure act
# dmi=denied mortgage insurance
# pbcr=public bad credit record
# dir=debt payments to total income ratio
# ccs=consumer credit score
library(tidyverse)
library(Ecdat)
data("Hdma")
names(Hdma)[11] <- "condo"
set.seed(111)

hdma <- Hdma %>%
  na.omit()

glimpse(hdma)

library(rpart)

hm.tree <- rpart(deny ~ ., data = hdma, method = "class")

library(rpart.plot)

prp(hm.tree, extra = 1)

library(randomForest)

set.seed(1234)
rf.fit <- randomForest(deny ~ ., data = hdma, importance = TRUE)
print(rf.fit)

# random forest prediction
hdma$random_forest_prediction <- predict(rf.fit)

# predictions from random forest vs output from nature's box
ggplot(hdma, aes(y = random_forest_prediction, x = deny)) +
  geom_jitter(height = 0.2, width = 02.)

# random forest determines which var is "important"

varImpPlot(rf.fit, type = 1)





























