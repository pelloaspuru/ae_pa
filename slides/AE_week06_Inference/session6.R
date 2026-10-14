## ---- myrcode1
library(sandwich)
library(lmtest)
library(wooldridge)

data("wage1")
wage1 <- read.csv("wage1.csv")
## ---- myrcode2
model <- lm(log(wage) ~ educ + exper +  female + nonwhite, data = wage1)
coeftest(model, vcov. = vcovHC(model, "HC1"))


## ---- myrcode3
(-0.3437118 - (-0.1))/ 0.0374930