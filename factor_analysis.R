dat <- read.csv("Euroemp.csv", header = TRUE)
dat
X <- dat[, -c(1:2)]
X

cor(X)

# bartlett sphericity test
# install.packages("psych")
library(psych)
cortest.bartlett(cor(X), n = nrow(X))

# reject null (null: equal to identity metrics)

fit <- principal(X, cor = "cor", nfactors = 4, rotate = "none")
fit

fit_varimax <- principal(X, cor = "cor", nfactors = 4, rotate = "varimax")
fit_varimax

fa.diagram(fit_varimax, simple = FALSE, cut = 0.7, digit = 3)
fa.diagram(fit_varimax, simple = FALSE, cut = 0.5, digit = 3)
