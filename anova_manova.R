# Anova - one dependent variable
# Manova - more dependent variables
# Do Sophomores and Juniors differ in their SAT scores on the
# Math and Reading & Writing Section scores?
# Bonferroni correction
library(readxl)

cooked_turkey <- read_xlsx("data/cooked_turkey.xlsx")
head(cooked_turkey)
attach(cooked_turkey)
# Independent Variables: REP (Representation), TRT (Treatment)
# Dependent Variables: CKG_LOSS (cooking loss), PH(pH), MOIST(moisture after re-cooking),
#                    FAT(fat content), HEX(hexanal content),
#                    MONHEM(bathophenanthroline-chelatable iron contents)
#                    CKG_TIME(cooking time to reach optimal temperature)

# Question to answer: Is there a significant difference in our 7 dependent variable?
yvars <- cbind(CKG_LOSS, PH, MOIST, FAT, HEX, MONHEM, CKG_TIME)
treat_factor <- factor(TRT)
rep_factor <- factor(REP)

# Interpreting MANOVA
mod <- manova(yvars ~ rep_factor + treat_factor, data = cooked_turkey)
print(mod)

# Based on our anova test, using the P-value of 5% as threshold...
# Look at manova (or other tests)

# Wilks Test
summary(mod, test = 'Wilks', intercept = TRUE)

# Roys Test
summary(mod, test = 'Roy', intercept = TRUE)

# Hotelling Lawley Test
summary(mod, test = 'Hotelling-Lawley', intercept = TRUE)

# Pillai Test (considered to be the most powerful test to use)
summary(mod, test = 'Pillai', intercept = TRUE)

# However, we may still be unclear about which groups differ from the other.
# And what effect our represented factors had on each individual factors...
# As a follow-up, we can execute the univariate test statistics
summary.aov(mod)

# One way to check out what best "treatments" are...
par(mfrow = c(3, 3))
boxplot(CKG_LOSS ~ TRT, data = cooked_turkey, xlab = "treatment", ylab = "CKG_LOSS")
boxplot(PH ~ TRT, data = cooked_turkey, xlab = "treatment", ylab = "pH")
boxplot(MOIST ~ TRT, data = cooked_turkey, xlab = "treatment", ylab = "MOIST")
boxplot(FAT ~ TRT, data = cooked_turkey, xlab = "treatment", ylab = "FAT")
boxplot(HEX ~ TRT, data = cooked_turkey, xlab = "treatment", ylab = "HEX")
boxplot(MONHEM ~ TRT, data = cooked_turkey, xlab = "treatment", ylab = "MONHEM")
boxplot(CKG_TIME ~ TRT, data = cooked_turkey, xlab = "treatment", ylab = "CKG_TIME")

# From here, setup contrasts(linear combinations as weights)
# AKA group comparisons to assess which group differ

library(candisc)

model2 <- lm(yvars ~ treat_factor, data = cooked_turkey)
# Make sure that the contrasts sum to
hyp <- matrix(c(0, 1, -1, 0, 0), nrow = 1, ncol = 5)
# Test contrast of having a positive relationship
# of treatment 1 and negative weight to treatment 2.
# We can tell that there is a significant difference here.
# Hence, Treatment 1 and 2 have an effect on the model.

linearHypothesis(model2, hyp)


hyp1 <- matrix(c(0, 0, 0, 1, -1), nrow = 1, ncol = 5)
linearHypothesis(model2, hyp1)
