options(digits = 4)
rnorm(20)
fit = lm(dist ~ speed, data = cars)
b = coef(fit)

summary(fit)$coefficients

par(mar=c(4, 4, 1, .1))
plot(cars, pch = 20)
abline(fit, col = 'red')

print(citation('knitr'), style = 'html')

