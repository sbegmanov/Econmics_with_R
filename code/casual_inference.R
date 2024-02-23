# P - pain, M - medicine (cause), individuals - i

library(tidyverse)
library(knitr)

set.seed(22)
beta0 <- 10
betaM <- 3
num <- 400
U_P <- rnorm(num)

# generate dataset Meds,contains M(Medicine), P(person), U_P

set.seed(3)
M <- sample(c(0, 1), num, replace = T)
Indiv <- 1: num
P <- beta0 + betaM * M + U_P
Meds <- tibble(Indiv, M, U_P, P)
kable(round(head(Meds), 2))

# potential outcome for an individual - P_i(m) where i indexes the Indiv and

library(texreg)
modd <- lm(P ~ M, data = Meds)
screenreg(list(modd), caption = "Regression of P on M",
       caption.above = TRUE,
       include.adjrs = FALSE,
       include.rmse = FALSE)


# Randomized assignment of treatment (casual graphs)

Y_po0 <- c(13,6,4,5,6,6,8,8)
print(Y_po0)

Y_po1 <- c(14,0,1,2,3,1,10,9)
print(Y_po1)

Eff <- Y_po1 - Y_po0
print(Eff)

surg <- data.frame(Y_po1, Y_po0, Eff)
print(surg)


# print the data for the perfect doctor
library(xtable)

sable <- xtable(surg,
                caption = "Patient potential outcomes in Rubin's perfect doctor example")
print(sable, caption.placement = "top")

library(skimr)
skiml <- surg %>%
  skim()
skimble <- xtable(skiml[, c(2, 6)], caption = "Means")
print(skimble, caption.placement = "top")

# D_i - treatment, Y_i(0) - i_th person if D_i = 0, Y_i(1)
# Y - for the i_th person by Y_i
# Y_i = D_i * Y_i(1) + (1 - D_i) * Y_i(0)

D <- c(rep(1, 4), rep(0, 4))
print(D)

Y_1 <- D * Y_po1 + (1 - D) * Y_po0
surg_D <- data.frame(D, Y_1)
surble <- xtable(surg_D, caption = "Treatment assignment and observed outcomes")
print(surble, caption.placement = "top")

# Researchers change the independent variable in the treatment group and keep it 
# constant in the control group. Then they compare the results of these groups.
# diff of means between treated and control groups

lm(Y_1 ~ D)

# randomize assignment (Ass)

sample(D, replace = FALSE)
Ass <- sample(D, replace = FALSE)
print(Ass)

# check the sampling distribution of the estimated effect by using a loop
iter <- 70
mean_effect <- numeric(iter)
for (i in 1: iter) {
  Ass <- sample(D, replace = FALSE)
  Out <- Ass * Y_po1 + (1 - Ass) * Y_po0
  mod_r <- lm(Out ~ Ass)
  mean_effect[i] <- mod_r$coeff[2]
}

round(mean(mean_effect), 2)

# plot the sampling distribution
pdoc <- data.frame(mean_effect)

ggplot(pdoc, aes(y = mean_effect)) +
  geom_boxplot() +
  coord_flip()


ggplot(pdoc, aes(x = mean_effect)) +
  geom_histogram(fill = "grey50") +
  geom_vline(xintercept = quantile(mean_effect, probs = c(0.25, 0.5, 0.75)),
             linetype = "dashed")


# Covariate adjustment
# directed acyclic graphs (DAGS)

com.cause <- runif(100, min = 10, max = 20)
x1 <- 2 * com.cause + rnorm(100, 0, 0.5)
y1 <- 2 * com.cause + rnorm(100, 0, 0.5)

m1 <- lm(y1 ~ x1)
m2 <- lm(y1 ~ x1 + com.cause)
screenreg(list(m1, m2), caption = "Common cause", caption.above = TRUE)

# intermediate variable scenario: y2 <- inter <- x2
x2 <- runif(100, min = 10, max = 20)
inter <- 2 * x2 + rnorm(100, 0, 0.5)
y2 <- 2 * inter + rnorm(100, 0, 0.5)
inter1 <- lm(y2 ~ x2)
inter2 <- lm(y2 ~ x2 + inter)
screenreg(list(inter1, inter2), caption = "Intermediate variables",
          caption.above = TRUE)

# collider scenario: y -> collider <- x
x3 <- rnorm(100)
y3 <- rnorm(100)
collider <- 4 * y3 + 4 * x3 + 0.3 * rnorm(100)

m5 <- lm(y3 ~ x3)
m6 <- lm(y3 ~ x3 + collider)
screenreg(list(m5, m6), caption = "Collider", caption.above = TRUE)

# Good control: Controlling for the common cause is good.
# Bad control: Controlling for the intermediary and collider variables is bad.

set.seed(80)
x1 <- rnorm(30)
x2 <- rnorm(30)
x3 <- rnorm(30)
x4 <- rnorm(30)
x5 <- rnorm(30)
x6 <- rnorm(30)
x7 <- rnorm(30)
x8 <- rnorm(30)
x9 <- rnorm(30)
x10 <- rnorm(30)
y <- rnorm(30)

mod1 <- lm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10)
mod2 <- lm(y ~ x2 + x10)
screenreg(list(mod1, mod2), caption = "Selection by statistical significance",
          caption.above = TRUE)

# Experiments
# number X was randomly assigned within a class, with a high X (65) being
# assigned to the treatment group, and a low X (10) being assigned to a control group.

library(tidyverse)

anchor <- read_csv("anchor.csv")

anchor %>%
  group_by(Class, Prompt) %>%
  summarize(count = n(),
            mean_AF = mean(African))

ggplot(anchor, aes(y = African, x = Prompt)) +
  geom_boxplot() +
  facet_wrap(~ Class) + 
  coord_flip()

anchor_12 <- anchor %>%
  filter(Class == "IES") %>%
  dplyr::select(Prompt, African)
anchor_12[1:2, ]

# treatment effect: 74-47
# conduct randomization inference with the data for class IES
anchor1 <- anchor %>%
  filter(Class == "IES")

library(ri2)
require(randomizr)
require(estimatr)
library(texreg)

Y <- anchor1$African
Z <- ifelse(anchor1$Prompt == "High_65", 1, 0)
print(Z)
anchor_IES_P <- data.frame(Y, Z)
print(anchor_IES_P)

# random assignment: 4 of the 8 units
declaration_1 <- declare_ra(N = 8, m = 4)
print(declaration_1)

ri2_IES_P <- conduct_ri(Y ~ Z,
                        data = anchor_IES_P,
                        declaration = declaration_1)
summary(ri2_IES_P)
plot(ri2_IES_P, guide = "none")

# analyse the data for the large class, TERI
anchor2 <- anchor %>%
  filter(Class == "TERI")

Y <- anchor2$African
Z <- ifelse(anchor2$Prompt == "High_65", 1, 0)

anchor_TERI <- data.frame(Y, Z)
print(anchor_TERI)

declaration2 <- declare_ra(N = 28, m = 16)
ri2_TERI <- conduct_ri(Y ~ Z,
                       data = anchor_TERI,
                       declaration = declaration2)
summary(ri2_TERI)
plot(ri2_TERI)

mod2 <- lm(African ~ Prompt, data = anchor2)

screenreg(list(mod2), caption = "Dependent variable is African, class TERI", caption.above = TRUE)

### Educational programme
# outcome was reading test scores
# treatment was exposure to an education television programme

electric <- read.table("electric.dat", header = T)
str(electric)

# Grade. Grade of the student.
# treated.Pretest. Pre-test scores of the treated students.
# control.Pretest. Pre-test scores of the control students.
# treated.Posttest. Post-test scores of the treated students.
# control.Posttest. Post-test scores of the control students.

post.test <- c(electric$treated.Posttest, electric$control.Posttest)
pre.test <- c(electric$treated.Pretest, electric$control.Pretest)
grade <- rep(electric$Grade, 2)
grade <- factor(grade)
rep(c(1, 0), rep(3, 2))

treatment <- rep(c(1, 0),
                 rep(length(electric$treated.Posttest), 2))
treatment <- factor(treatment)
n <- length(post.test)
elec <- tibble(post.test, pre.test, grade, treatment)
print(elec)

# focus on grade==1
elec_1 <- elec %>%
  filter(grade == 1)
# how about grade = 2?

# plot boxplots of post-test scores of treated versus control group
ggplot(elec_1, aes(y = post.test, x = treatment)) +
  geom_boxplot() +
  coord_flip()

# a scatter plot of post-test versus pre-test works well in this case
ggplot(elec_1, aes(x = pre.test, y = post.test, colour = treatment )) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE)

# Because this is an experiment,we need not control for pre-test scores, 
# but including pre-test scores gives more precise estimates.

mod1.1 <- lm(post.test ~ treatment, data = elec_1)
mod1.2 <- lm(post.test ~ pre.test + treatment, data = elec_1)
screenreg(list(mod1.1, mod1.2),
          ci.force = TRUE, ci.test = NULL,
          caption = "Effect of programme on scores",
          caption.above = TRUE)
# the estimate is more precise (narrower confidence interval) in Model 2.

#### Example: Star
# Three treatments were assigned at the classroom level:
# small classes (13-17 students),regular classes (22-25 students), 
# regular classes with an aide who would work with the teacher.


library(tidyverse)
library(POE5Rdata)
data("star")
str(star)
attach(star)

# One of the treatments was using a teaching aide; we ignore these observations
# and focus on the small versus regular comparison.

star <- star %>%
  filter(aide == 0) %>%
  dplyr::select(totalscore, small, tchexper, boy, freelunch, white_asian, schid) %>%
  mutate(small_fac = ifelse(small == 1, "small", "regular"),
         sch_fac = factor(schid))

str(star)
star <- as_tibble(star)
print(star)

star %>%
  group_by(small_fac) %>%
  summarize(mscore = mean(totalscore),
            sdscore = sd(totalscore))

ggplot(star, aes(x = small_fac, y = totalscore)) +
  geom_boxplot() +
  coord_flip()

# diff covariates vary between the regular and small classes

star %>%
  group_by(small_fac) %>%
  summarize(mboy = mean(boy),
            mlunch = mean(freelunch),
            mw_a = mean(white_asian),
            mexper = mean(tchexper))


library(cobalt)

# differences in means of covariates between treated and control 
# groups are small.

love.plot(small ~ boy + freelunch + white_asian + tchexper,
          data = star, stars = "std")

# formal test of balance as follows using a linear probability model

mod_star_check <- lm(small ~ boy + white_asian + tchexper + freelunch,
                     data = star)
library(texreg)

screenreg(mod_star_check, ci.force = TRUE,
          ci.test = NULL, caption = "Checking balance",
          caption.above = TRUE)

mod_star_1 <- lm(totalscore ~ small_fac, data = star)
mod_star_2 <- lm(totalscore ~ small_fac + boy + freelunch + white_asian, data = star)
mod_star_3 <- lm(totalscore ~ small_fac + boy + freelunch + white_asian +
                   tchexper + sch_fac, data = star)

screenreg(list(mod_star_1, mod_star_2, mod_star_3),
          omit.coef = "Intercept|sch_fac",
          ci.force = TRUE, ci.test = NULL,
          caption = "Effect of small class on total scores.",
          caption.above = TRUE)

### Matching - somewhat technical, rely on simulated data
### Simple Example with Synthetic Data
# Assume that x, a binary variable, and w are causes of y. 
# Also w affects y non-linearly.

library(tidyverse)

x <- c(rep(0, 6), rep(1, 6))
w <- c(30, 18, 20, 10, 10, 17, 20, 18, 10, 10, 17, 3)
y <- (10 * x) + w + (0.2 * w^2) + (3 * (rnorm(12, 1, 1)))
wsq <- w^2
dat_mat <- data.frame(y, x, w, wsq)
print(dat_mat)

ggplot(dat_mat, aes(x = w, y = y, shape = factor(x),
                    linetype = factor(x))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "red")

# diff 3 regressions
library(texreg)

mod1 <- lm (y ~ x + w + wsq, data = dat_mat)
mod2 <- lm(y ~ x + w, data = dat_mat)
mod3 <- lm(y ~ x, data = dat_mat)
screenreg(list(mod1, mod2, mod3),
          caption = "Effect of omitting w and wsq",
          ci.force = T, ci.test = NULL,
          caption.above = TRUE)

# matching to match observations for which x = 0 with corresponding
# observations for which x = 1 with exactly the same ws.
library(MatchIt)

match.1 <- matchit(x ~ w, data = dat_mat,
                   method = "exact", replace = FALSE)
print(match.1)

matchit(formula = x ~ w, data = dat_mat, method = "exact",
        replace = FALSE)

# plot covariate balance before and after matching
library(cobalt)

love.plot(match.1, stars = "std")

# extract the matched data
match_dat <- match.data(match.1)
print(match_dat)

mod1 <- lm (y ~ x + w + wsq, data = dat_mat)
mod2 <- lm(y ~ x + w, data = dat_mat)
mod3 <- lm(y ~ x, data = dat_mat)

screenreg(list(mod1, mod2, mod3),
          caption = "Effect of omitting w and wsq",
          ci.force = T, ci.test = NULL,
          caption.above = TRUE)

# Ho et al. (2011) advocate matching as a non-parametric method
# to reduce model dependence
ggplot(match_dat, aes(x = w, y = y, shape = factor(x),
                      linetype = factor(x))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "black")
rm(list = ls())

### Example: Labour Training Programme
# experimental study vs observational study

data("lalonde", package = "MatchIt")
str(lalonde)
attach(lalonde)

# re78 - earnings in 1978

li <- lalonde
love.plot(treat ~ age + educ + race + married +
            nodegree + re74 + re75, data = li, stars = "std")

# covariates are imbalanced, especially black and re74

mod_la1 <- lm(re78 ~ treat, data = li)
mod_la2 <- lm(re78 ~ treat + age + educ + race + married +
                nodegree + re74 + re75, data = li)

screenreg(list(mod_la1, mod_la2),
          caption = "Regression with lalonde data",
          ci.force = T, ci.test = NULL)

# genetic matching
# mahalanobis distance is not optimal
library(Matching)
library(rgenoud)

set.seed(123)
match.li <- matchit(treat ~ age + educ + race + married +
                      nodegree + re74 + re75,
                    data = li, method = "genetic",
                    replace = FALSE, pop.size = 50, print = 0)

print(match.li)

love.plot(match.li, stars = "std")

# covariate balance improved
match_dat <- match.data(match.li)

ggplot(li, aes(x = factor(treat), y = re78)) +
  geom_boxplot() +
  coord_flip()

ggplot(match_dat, aes(x = factor(treat), y = re78)) +
  geom_boxplot() +
  coord_flip()

mod_la_match1 <- lm(re78 ~ treat, data = match_dat)
mod_la_match2 <- lm(re78 ~ treat + age + educ + race + married +
                      nodegree + re74 + re75, data = match_dat)
screenreg(list(mod_la_match1, mod_la_match2),
          caption = "Regression with matched lalonde data",
          ci.force = T, ci.test = NULL)

### Sensitivity analysis
# Y - outcome
# Tr - treatment
# X - covariates

library(rbounds)

Y <- li$re78
Tr <- li$treat
X <- cbind(li$age, li$educ, li$educ, li$race, li$married, li$nodegree,
           li$nodegree, li$re74, li$re75)

BalanceMat <- cbind(li$age, I(li$age^2), li$educ, I(li$educ^2),
                    li$race, li$married, li$nodegree, li$re74,
                    I(li$re74^2), li$re75, I(li$re75^2),
                    I(li$re74 * li$re75), I(li$age * li$nodegree),
                    I(li$educ * li$re74), I(li$educ * 75))
# Genetic matching and weights
gen1 <- GenMatch(Tr = Tr, X = X, BalanceMatrix = BalanceMat, pop.size = 50,
                 data.type.integer = FALSE, print = 0, replace = FALSE)
# Match
mgen1 <- Match(Y = Y, Tr = Tr, X = X, Weight.matrix = gen1, replace = FALSE)
summary(mgen1)

# The psens() function provides a sensitivity analysis:
psens(mgen1, Gamma = 1.5, GammaInc = 0.1)

# The hlsens() provides a sensitivity analysis for the Hodges-Lehmann point
# estimate.
hlsens(mgen1, Gamma = 1.5, GammaInc = 0.1)

### Example: Lead exposure
# fathers worked in a battery manufacturing plant in Oklahoma

library(DOS)
library(rbounds)
library(tidyverse)

data(lead, package = "DOS")
head(lead)

child_lead <- c(lead$control, lead$exposed)
treat <- c(rep("control", 33), rep("exposed", 33))
child_lead_dat <- data.frame(child_lead, treat)

ggplot(child_lead_dat, aes(x = treat, y = child_lead)) +
  geom_boxplot() +
  ylim(0, 80) +
  coord_flip()

library(forcats)

llevel <- c("low", "medium", "high")
lead$F_level <- factor(lead$level, levels = llevel)

ggplot(lead, aes(x = F_level, y = exposed)) +
  geom_boxplot() +
  ylim(0, 80) +
  coord_flip()

ggplot(lead, aes(x = F_level, y = control)) +
  geom_boxplot() +
  ylim(0, 80) +
  coord_flip()

# children whose fathers had high exposure,and within this group,
# compare children on the basis of father's hygiene.

lead$hyg <- ifelse(lead$hyg == "poor", "poor", "ok")

lead %>%
  filter(F_level == "high") %>%
  ggplot(aes(x = hyg, y = exposed)) +
  geom_boxplot() +
  ylim(0, 80) +
  coord_flip()



### Example: Compensation for injury
library(tidyverse)
library(wooldridge)
data("injury")
glimpse(injury)
attach(injury)

# remove observations with missing values
injury <- injury %>%
  na.omit()
summary(injury)

# subset high earners in Kentucky (ky)
inj_ky_h <- injury %>%
  filter(ky == 1, highearn == 1)

# subset low earners in Kentucky (ky)
inj_ky_l <- injury %>%
  filter(ky == 1, highearn == 0)

# "afchange" is a dummy variable for observations after the policy change
# matching for highs and analysis
library(MatchIt)

match.ky.h <- matchit(afchnge ~ male + married + hosp + indust +
                      injtype + age + lprewage,
                      data = inj_ky_h, method = "genetic",
                      replace = FALSE, pop.size = 50, print = 0)
print(match.ky.h)

library(cobalt)

love.plot(match.ky.h, stars = "std")
match_dat_ky_h <- match.data(match.ky.h)

# comparison of log duration between after and before groups in matched data
ggplot(match_dat_ky_h, aes(y = ldurat, x = factor(afchnge))) +
  geom_boxplot() +
  coord_flip()

match_dat_ky_h %>%
  group_by(afchnge) %>%
  summarize(mean_ld = mean(ldurat),
            median_ld = median(ldurat))

# diff in means in the after and before group
library(Matching)
library(rbounds)
attach(match_dat_ky_h)

Y <- ldurat
Tr <- afchnge
X <- cbind(male, married, hosp, indust, injtype, age, lprewage)

gen1 <- GenMatch(Tr = Tr, X = X, pop.size = 50, print = 0)
mgen1 <- Match(Y = Y, Tr = Tr, X = X,
               Weight.matrix = gen1, replace = FALSE)
summary(mgen1)

# At Gamma = 1.3 the p-value exceeds 0.05 in the sensitivity analysis.
psens(mgen1, Gamma = 1.5, GammaInc = 0.1)
# The H-L estimate is 0.25 at Gamma = 0 and the bound crosses zero at Gamma = 1.3.
hlsens(mgen1, Gamma = 1.5, GammaInc = 0.1)
# how about inj_ky_l ?



### Regression Discontinuity
# treatment assignment depends on a cutoff value of a variable
# Synthetic data

library(tidyverse)
set.seed(12)
s_size <- 1000

# running variable - uniform distribution
run <- runif(s_size, min = 10, max = 50)

# treatment, cutpoint = 20
treat <- ifelse(run < 20, 0, 1)

# outcome = 10 treat - 0.4 run + noise
outcome <- 10 * treat - 0.4 * run + 3 * rnorm(s_size)

# data frame
rd_data <- data.frame(treat = factor(treat), run, outcome)

ggplot(rd_data) +
  geom_point(aes(x = run, y = outcome, shape = treat), col = "grey60") +
  geom_smooth(aes(x = run, y = outcome, linetype = treat),col = "black") +
  geom_vline(xintercept = 20)

# Regressing the outcome variable on treatment and the running variable gives us
# an estimate that is close to the true effect (10)

lm(outcome ~ treat + run)

### Example: Minimum legal drinking age (MLDA)
# Did the MLDA of 21 affect death rates in the United States?
# read in Stata data

library(foreign)

mlda = read.dta("AEJfigs.dta")
glimpse(mlda)
attach(mlda)
# We have data on all, death rates from all causes, and agecell, age in months.
# create a dummy variable for age over 21
mlda$over21 = mlda$agecell >= 21

library(ggplot2)

age3 = ggplot(mlda, aes(x = agecell, y = all, colour = over21))+
  geom_point() +
  geom_vline(xintercept = 21)
print(age3)

age4 = age3 +
  stat_smooth(method = "lm") +
  stat_smooth(method = "loess")
print(age4)

# regression discontinuity
library(rddtools)

mlda <- mlda %>%
  na.omit()

# declare regression discontinuity data

rd_data_2 <- rdd_data(y = all, x = agecell, data = mlda, cutpoint = 21)
summary(rd_data_2)

# parametric regression to estimate the treatment effect
reg_para <- rdd_reg_lm(rd_data_2, order = 1)
print(reg_para)
plot(reg_para)

library(mosaic)

mplot(reg_para)

# use a non-parametric regression; first get optimal bandwidth
bw_ik <- rdd_bw_ik(rd_data_2)
print(bw_ik)

# estimate and plot the non-parametric regression
reg_nonpara <- rdd_reg_lm(rdd_object = rd_data_2, bw = bw_ik)
print(reg_nonpara)

# run placebo and sensitivity tests
plotPlacebo(reg_nonpara)

# sensitivity to bandwidth
plotSensi(reg_nonpara, from = 0.05, to = 3, by = 0.15)




### Difference in Difference

library(tidyverse)
library(texreg)
library(wooldridge)
data("jtrain")
attach(jtrain)

# fixed effect
# remove data for 1989 yr
jtrain <- jtrain %>%
  filter(year != 1989)

# plm package for handling panel data
library(plm)

jtrain_p <- pdata.frame(jtrain, index = c("fcode", "year"))

# diff()
jtrain_p$scrap_d <- diff(jtrain_p$scrap)
jtrain_p$grant_d <- diff(jtrain_p$grant)

# effect of grant on scrap
mod_did <- lm(scrap_d ~ grant_d, data = jtrain_p)

library(texreg)
screenreg(list(mod_did), caption = "Dependent variable is scrap",
          caption.above = TRUE)


# Simulation
# Case A - differencing removes fixed effect

ss <- 3000 # sample size
eff <- 3 # effect
fixed <- rnorm(ss)
treat <- ifelse(fixed < 0, 1, 0)
uy1 <- rnorm(ss)
uy0 <- rnorm(ss)
y1 <- fixed + eff * treat + uy1
y0 <- fixed + uy0

# difference of means
m_d1 <- lm(y1 ~ treat)

# controlling for y0
m_d2 <- lm(y1 ~ y0 + treat)

# diff in diff
m_d3 <- lm(I(y1 - y0) ~ treat)
screenreg(list(m_d1, m_d2, m_d3), caption = "Case A results",
          caption.above = TRUE)

# Case B

ss <- 3000
eff <- 3
y0 <- runif(ss, min = 1, max = 4)
treat <- ifelse(y0 < 2.5, 1, 0)
uy1 <- rnorm(ss)
uy0 <- rnorm(ss)
y1 <- 0.3 * y0 + eff * treat + uy1

# difference of means
m_d4 <- lm(y1 ~ treat)

# controlling for y0
m_d5 <- lm(y1 ~ y0 + treat)

# diff in diff
m_d6 <- lm(I(y1 - y0) ~ treat)
screenreg(list(m_d4, m_d5, m_d6), caption = "Case B results",
          caption.above = TRUE)

### Example: Banks in Business

library(readr)
library(tidyverse)

banks <- read_csv("banks.csv")
glimpse(banks)
attach(banks)

# number of banks in business each yr in 6th and 8th districts
bankag <- banks %>%
  group_by(year) %>%
  summarize(bib6m = mean(bib6),
            bib8m = mean(bib8))
head(bankag)

# stack  banks in business for  6th&8th districts in one column.
bankag2 <- gather(bankag, "bty", "num", 2:3)

# filter for yrs 1930-31
bankag3 <- filter(bankag2, year == 1930 | year == 1931)
print(bankag3)

# difference-in-difference estimate is 
# =(120-136)-(132-165) = -16-(-33) = 17.

ggplot(bankag3, aes(x = year, y = num, colour = bty)) +
  geom_line()

ggplot(bankag2, aes(x = year, y = log(num), colour = bty)) +
  geom_line()




# Example: Manski bounds for crime and laws
# effect of right to carry laws on crime
library(tidyverse)

Manski <- read_csv("Manski.csv")

ggplot(Manski) +
  geom_line(aes(x = Year, y = V_mur), linetype = "solid") +
  geom_line(aes(x = Year, y = M_mur), linetype = "dashed") +
  geom_vline(xintercept = 1989, linetype = "dotted")

# crime outcome - Y
# (1) - treatment
# (0) - absence of treatment
# Virginia - VA
# treatment effect = Y_VA1990(1) - Y_VA1990(0)

# Bounds with Maryland as Counterfactual

ggplot(Manski[1:19, ]) +
  geom_line(aes(x = Year, y = V_mur - M_mur))

# Y_VA,d(0) - Y_MD,d(0) = theta_VAMD,d - is between 1 to - 2.7
# From 1990 onwards, use Y_MD,d (0) to fill in for Y_VA,d (0), but raising it by 1 for one 
# bound of the treatment effect and lowering it by 2.7 for another bound of the treatment effect

ggplot(Manski[20:38,]) +
  geom_line(aes(x = Year, y = V_mur), linetype = "solid") +
  geom_line(aes(x = Year, y = M_mur), linetype = "dashed") +
  geom_line(aes(x = Year, y = M_mur + 1), linetype = "dotted") +
  geom_line(aes(x = Year, y = M_mur - 2.7), linetype = "dotted")

ggplot(Manski[20:38,]) +
  geom_line(aes(x = Year, y = V_mur - M_mur - 1), linetype = "solid") +
  geom_line(aes(x = Year, y = V_mur - M_mur + 2.7), linetype = "dashed") +
  geom_hline(yintercept = 0, line = "dotted")

# Bounds based on difference-in-difference

Manski_ts <- ts(Manski[2:3], start = 1970, end = 2007)
Manski_ts2 <- window(Manski_ts, start = 1970, end = 1988)
head(Manski_ts2)

Manski_ts2_d <- diff(Manski_ts2)
diff_V_M <- Manski_ts2_d[, 1] - Manski_ts2_d[, 2]

library(ggfortify)

autoplot(abs(diff_V_M))

# from the series of theta_d+1, we could pick a suitable value, one perhaps is the 0.75
# quantile of the series, which we denote as theta_0.75, (delta_0.75) = 1.155.

quantile(abs(diff_V_M), probs = c(0, 0.25, 0.5, 0.75, 1))
delta_0.75 <- quantile(abs(diff_V_M), probs = c(0.75))
print(delta_0.75)

# |[Y_VA,d(0) - Y_VA,1988(0)] - [Y_MD,d(0) - Y_MD,1988(0)]| =< theta_0.75

Manski_ts3 <- window(Manski_ts, start = 1990, end = 2007)
print(Manski_ts3)

# treatment effect values: 7.75 and 9.64
window(Manski_ts, start = 1988, end = 1988)

TE_strong <- Manski_ts3[, 1] - 7.75 - Manski_ts3[, 2] + 9.64
print(TE_strong)

TE_lb <- TE_strong - delta_0.75
TE_ub <- TE_strong + delta_0.75
Manski_eff <- cbind(TE_lb, TE_strong, TE_ub)
Manski_eff <- round(Manski_eff, 1)
Manski_eff_d <- data.frame(Year = 1990:2007, Manski_eff)
print(Manski_eff_d)

library(xtable)
teff <- xtable(Manski_eff_d,
               caption = "Treatment effect with strong difference-in-difference assumption, and lower and upper bound DID assumption.")
print(teff, caption.placement = "top")

autoplot(Manski_eff, facets = F) +
  geom_hline(yintercept = 0, linetype = "dotted")



#### Instrumental Variables

library(tidyverse)
library(AER)

sample_size = 300
coef_Z = 0.9
viol = 0

Z <- runif(sample_size, min = 1, max = 5)
U <- runif(sample_size, min = 1, max = 5) + viol * Z
X <- U + rnorm(sample_size) + coef_Z * Z
Y <- U + X + rnorm(sample_size)

mod1OLS <- lm(Y ~ Z)
mode2OLS <- lm(Y ~ X + U)

library(texreg)
screenreg(list(mod1OLS, mode2OLS), caption = "OLS results for Y",
          caption.above = TRUE)

# instrumental variable
ModIV <- ivreg(Y ~ X | Z)
screenreg(list(ModIV), caption = "IV results for Y",
          caption.above = TRUE)

# function to carry out IV simulations
IVsamD <- function(sample_size, coef_Z, viol = 0) {
  num_loops = 300
  #sample_size = 30
  #coef_Z = 0.5
  
  OLS1 <- numeric(num_loops)
  OLS2 <- numeric(num_loops)
  IV <- numeric(num_loops)
  
  for (i in 1:num_loops) {
    U <- runif(sample_size, min = 1, max = 5)
    Uy <- rnorm(sample_size)
    Z <- runif(sample_size, min = 1, max = 5) + viol * Uy
    X <- U + rnorm(sample_size) + coef_Z * Z
    Y <- U + X + Uy
    OLS1[i] <- summary(lm(Y ~ Z))$coef[2]
    OLS2[i] <- summary(lm(Y ~ X + U))$coef[2]
    IV[i] <- summary(ivreg(Y ~ X | Z))$coef[2]
  }
  reg_IV <- tibble(OLS1, OLS2, IV)
  print(reg_IV)
  
  library(tidyr)
  reg_IV_s <- reg_IV %>%
    gather(Estimator, value, OLS1:IV)
  print(reg_IV_s)
  
  library(ggplot2)
  ggplot(reg_IV_s, aes(value, colour = Estimator)) +
    geom_density() +
    xlim(c(-1, 2)) +
    geom_vline(xintercept = 1, lty = 2)
  
}

IVsamD(sample_size = 30, coef_Z = 1, viol = 0)

# Large sample size
IVsamD(sample_size = 300, coef_Z = 1, viol = 0)

# weak instrument
IVsamD(sample_size = 300, coef_Z = 0.2, viol = 0)

# exclusion restriction violated
IVsamD(sample_size = 300, coef_Z = 1, viol = 0.5)


### Example: Demand for Cigarettes
library(tidyverse)
library(AER)

data("CigarettesSW", package = "AER")
# Taxes effect on smoking, 

Cig <- CigarettesSW

library(ggplot2)

ggplot(Cig, aes(x = log(packs), y = log(price/cpi))) +
  geom_point() +
  stat_smooth(method = "lm")

# remove fixed effects by differencing

Cig <- Cig %>%
  mutate(rprice = price / cpi)

Cig <- Cig %>%
  mutate(rincome = income / population / cpi)

# taxs = sales tax, tax is tax

Cig <- Cig %>%
  mutate(rtaxs = (taxs - tax) / cpi)

Cig <- Cig %>%
  mutate(rtax = tax / cpi)

# 1985 - 1995 years data

Cig85 <- Cig %>%
  filter(year == 1985)

Cig95 <- Cig %>%
  filter(year == 1995)

# Transmute works like mutate but does not add to the other data
pack_85 <- Cig85 %>%
  transmute(pack_85 = log(packs))

pack_95 <- Cig95 %>%
  transmute(pack_95 = log(packs))

pack_diff <- pack_95$pack_95 - pack_85$pack_85

# long-term elasticity

rprice85 <- Cig85 %>%
  transmute(rprice85 = log(price))

rprice95 <- Cig95 %>%
  transmute(rprice95 = log(rprice))

rpricediff <- rprice95$rprice95 - rprice85$rprice85

# income and taxes

i85 <- Cig85 %>%
  transmute(i85 = log(rincome))

i95 <- Cig95 %>%
  transmute(i95 = log(rincome))

idiff <- i95$i95 - i85$i85

ts85 <- Cig85 %>%
  transmute(ts85 = rtaxs)

ts95 <- Cig95 %>%
  transmute(ts95 = rtaxs)

tsdiff <- ts95$ts95 - ts85$ts85

t85 <- Cig85 %>%
  transmute(t85 = rtax)

t95 <- Cig95 %>%
  transmute(t95 = rtax)

tdiff <- t95$t95 - t85$t85

# convenience function: HC1 covariences
library(estimatr)

hc1 <- function(x) vcovHC(x, type = "HC1")

mod1 <- iv_robust(pack_diff ~ rpricediff + idiff | idiff + tsdiff,
                  diagnostics = TRUE)

mod2 <- iv_robust(pack_diff ~ rpricediff + idiff | idiff + tdiff)

mod3 <- iv_robust(pack_diff ~ rpricediff + idiff | idiff + tsdiff + tdiff,
                   diagnostics = TRUE)

library(texreg)
screenreg(list(mod1, mod2, mod3), caption = "Instrumental variable estimates",
          caption.above = TRUE)

# overidentification test for validity of instruments

summary(mod3)$diagnostic_overid_test

# F-statistic for Model 1 is fine
summary(mod1)$diagnostic_first_stage_fstatistic
# long-run elasticity is  - 0.9
































































