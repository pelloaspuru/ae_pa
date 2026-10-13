# Set a new working directory.
setwd("C:/Users/Aspuru Pello/Desktop/2025_AE_PA")

# Verify the change
#getwd()

# List files in the working directory
#dir()

# Clean working space
rm(list = ls())

## Call Libraries:

library(sandwich)
library(lmtest)
library(ggplot2)
#library(wooldridge)

# Open dataset as df_wages:
df_wages <- read.csv("./AE_week5_Inference/wage1.csv")

# Generate constant variable
df_wages$constant <-1

# Dataset in matrix notation
X <- as.matrix(df_wages[,names(df_wages) %in% c("constant","educ", "exper", "female")])
Y <- as.matrix(df_wages[,names(df_wages) %in% c("wage")])

# vector of residuals
beta_hat = solve(t(X) %*% X) %*% t(X) %*% Y
beta_hat_educ = beta_hat["educ", ]
u_hat_sq = (as.vector(Y - X %*% beta_hat))^2

# Approximation of omega:
Omega_hat <- as.matrix(diag(u_hat_sq))

# Estimation of the robust variance of beta_hat
var_beta_hat <- solve(t(X) %*% X) %*% t(X) %*% Omega_hat %*% X %*% solve(t(X) %*% X)

# Comparing parameter standard errors
SE_hat <- sqrt(diag(var_beta_hat))
SE_hat_educ <- SE_hat["educ"]

# Single-parameter Two-sided hypothesis testing:

alpha <- 0.05
Q_alpha <- qnorm(1-alpha/2)
T_educ <- (beta_hat_educ-0.8)/SE_hat_educ
abs(T_educ) > Q_alpha

## Single-parameter two-sided hypothesis, null=0 testing with R functions: 
mlr <- lm(wage ~ educ + exper +  female, data = df_wages)
robust_vcov <- vcovHC(mlr, type = "HC0")
coeftest(mlr, vcov = robust_vcov)

#
robust_se <- sqrt(diag(robust_vcov))
SE_hat
robust_se

## Single-parameter one-sided hypothesis:
alpha <- 0.05
Q_alpha <- qnorm(alpha)
T_educ <- (beta_hat_educ-0.1)/SE_hat_educ
abs(T_educ) > Q_alpha

# Effect of one standard deviation of a covariate on the output:

mlr_2 <- lm(log(wage) ~ educ + exper +  female, data = df_wages)
robust_vcov_2 <- vcovHC(mlr_2, type = "HC0")
#
beta_hat_educ_2      <- coef(mlr_2)["educ"]
se_educ_robust <- sqrt(diag(robust_vcov_2))["educ"]
#
# effect of +1 SD in exper (in % units)
sd_educ        <- sd(df_wages$educ, na.rm = TRUE)
effect_1sd      <- beta_hat_educ_2 * sd_educ

# Robust confidence intervals
Q_alpha <- qnorm(1-alpha/2)
beta_hat_educ_low <- beta_hat_educ - Q_alpha*SE_hat_educ
beta_hat_educ_high <- beta_hat_educ + Q_alpha*SE_hat_educ

coefci(mlr, vcov = robust_vcov)

# Confidence interval plot:

# robust CIs (e.g., 95%)
ci_mat <- coefci(mlr, vcov = robust_vcov, level = 0.95)

# assemble a tidy frame
df_ci <- data.frame(
  term      = rownames(ci_mat),
  estimate  = coef(mlr)[rownames(ci_mat)],
  conf.low  = ci_mat[, 1],
  conf.high = ci_mat[, 2],
  row.names = NULL
)

# plot
ggplot(df_ci, aes(x = term, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  labs(x = NULL,
       y = "Coefficient (robust 95% CI)",
       title = "Robust (HC0) Confidence Intervals from lm") +
  theme_minimal(base_size = 12)

ggsave("./AE_week5_Inference/conf_int_plot.pdf")

