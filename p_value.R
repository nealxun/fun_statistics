# objective: show a scenario of deceive p-value
# reference: https://www4.stat.ncsu.edu/~stefanski/NSF_Supported/Hidden_Images/orly_owl_files/orly_owl_Lin_4p_5_flat.txt

# deceive p-value
#dat <- read.table("./Input/orly_owl_Lin_4p_5_flat.txt")
dat <- read.table("https://raw.githubusercontent.com/nealxun/Fun_Stat/master/Input/orly_owl_Lin_4p_5_flat.txt")
#pairs(dat)
#summary(lm(V1 ~ . -1, data = dat))$coef
fit <- lm(V1 ~ . - 1, data = dat)
summary(fit)
plot(predict(fit), resid(fit), pch = '.')

# influential point
n <- 100
x <- c(10, rnorm(n)); y <- c(10, c(rnorm(n)))
plot(x, y, frame = FALSE, cex = 2, pch = 21, bg = "lightblue", col = "black")
abline(lm(y ~ x))
fit <- lm(y ~ x)
summary(fit)
round(dfbetas(fit)[1 : 10, 2], 3)
