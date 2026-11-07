# Yields of amoebae: one-way ANOVA, written three equivalent ways.
# Data: entozamoeba.txt in this repository. 5 conditions, 10 yields per condition.
# Packages: dplyr, ggplot2
# Lecture(s): L02

library(dplyr)
library(ggplot2)

# Read the data ----------------------------------------------------------------
# Condition is a label, not a number, so it must be stored as a factor.
yields <- read.table(
  "https://raw.githubusercontent.com/athienit/STA4211material/main/entozamoeba.txt",
  col.names = c("Condition", "Yield")
) |>
  mutate(Condition = factor(Condition))

# Look at the data before modeling it ------------------------------------------
yields |>
  group_by(Condition) |>
  summarize(n = n(), mean = mean(Yield), sd = sd(Yield)) |>
  as.data.frame()

ggplot(yields, aes(x = Condition, y = Yield)) +
  geom_point(
    shape = 1, size = 2.5,
    position = position_jitter(width = 0.08, height = 0, seed = 1)
  ) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 5) +
  labs(
    title = "Yields by condition",
    subtitle = "Circles are observations; diamonds are condition means",
    x = "Condition", y = "Yield"
  ) +
  theme_minimal(base_size = 13)

# Model 1: cell means, Y = mu_i + error ----------------------------------------
# The 0 removes the intercept, so each coefficient is one condition's mean.
# Use this fit for its estimated means. Do not use its overall F test or R-squared:
# without an intercept, the F test asks whether all the means are 0, which is not
# the question we care about.
fit_cell <- lm(Yield ~ 0 + Condition, data = yields)
coef(fit_cell)

# Model 2: factor effects with a base group, R's default ------------------------
# The coding matrix shows how each condition is written in terms of indicators.
contr.treatment(5)

# The intercept is the mean of Condition 1, and each coefficient is that
# condition's mean minus the Condition 1 mean.
fit_base1 <- lm(Yield ~ Condition, data = yields)
anova(fit_base1)

# The F critical value and the p-value can also be found directly.
qf(0.95, df1 = 4, df2 = 45)
pf(5.0444, df1 = 4, df2 = 45, lower.tail = FALSE)

summary(fit_base1)

# To match the lecture's tau_3 = 0, choose Condition 3 as the base group. Each t test
# now compares a condition with Condition 3, and three of the four are new comparisons.
contr.treatment(5, base = 3)
fit_base3 <- lm(
  Yield ~ Condition, data = yields,
  contrasts = list(Condition = contr.treatment(5, base = 3))
)
summary(fit_base3)

# Model 3: factor effects with sum-to-zero coding ------------------------------
# The intercept is the average of the 5 condition means, and each coefficient is
# that condition's mean minus this average. R omits the last effect, so we get it
# from the constraint that the effects sum to 0.
contr.sum(5)
fit_sum <- lm(
  Yield ~ Condition, data = yields,
  contrasts = list(Condition = contr.sum)
)
summary(fit_sum)
-sum(coef(fit_sum)[-1]) # the effect of Condition 5

# The models are the same model ------------------------------------------------
# The coefficients mean different things, but every fit gives the same estimated
# condition means, and the fits with an intercept report the same F test.
conditions <- data.frame(Condition = levels(yields$Condition))
data.frame(
  Condition = conditions$Condition,
  cell_means = predict(fit_cell, conditions),
  base_1 = predict(fit_base1, conditions),
  base_3 = predict(fit_base3, conditions),
  sum_zero = predict(fit_sum, conditions)
)

anova(fit_base3)
anova(fit_sum)

# Checking the model assumptions is the next step: see ch16_Yields_check.R.

# Learn more about coding systems (run ?contrasts for R's help page):
# https://stats.oarc.ucla.edu/r/library/r-library-contrast-coding-systems-for-categorical-variables/
