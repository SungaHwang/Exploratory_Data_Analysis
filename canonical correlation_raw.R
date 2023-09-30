butterfly <- read.csv("Butterfly colonies.csv", header = TRUE, row.names = 1)
env <- butterfly[, 1:4]

PFPGi0.40_0.60 <- butterfly[, 5] + butterfly[, 6]
gen <- cbind(PFPGi0.40_0.60, butterfly[,7:9])

env.std <- scale(env)
gen.std <- scale(gen)

Rxx <- cor(env.std)
Ryy <- cor(gen.std)
Rxy <- cor(env.std, gen.std)

A <- solve(Ryy) %*% t(Rxy) %*% solve(Rxx) %*% Rxy
CC.butterfly <- eigen(A)

eigenvec.butterfly.Y <- CC.butterfly$vectors
rownames(eigenvec.butterfly.Y) <- colnames(gen.std)
eigenvec.butterfly.X <- solve(Rxx) %*% Rxy %*% eigenvec.butterfly.Y

eigenvec.butterfly.X
eigenvec.butterfly.Y


ccorr <- sqrt(CC.butterfly$values)


# Bartlett's test
n <- nrow(env)
p <- ncol(env)
q <- ncol(gen)
test.stat <- - (n - 0.5*(p + q + 3)) * sum(log(1 - CC.butterfly$values))
test.stat
pvalue <- pchisq(test.stat, df = p * q, lower.tail = FALSE)
pvalue

#cancor

install.packages("candisc")
library(candisc)
CC.butterfly.candisc <- cancor(env.std, gen.std, set.names = c("Environmental", "Genetic"))
summary(CC.butterfly.candisc)
CC.butterfly.coef <- coef(CC.butterfly.candisc, type = "both", standardize = TRUE)


# CC.butterfly.candisc$structure

## compare
a <- eigenvec.butterfly.Y[, 1]
sqrt(t(a) %*% a)

b <- CC.butterfly.coef[[2]][, 1]
b/as.numeric(sqrt(t(b) %*% b))

plot(CC.butterfly.candisc, ellipse = FALSE, abline = FALSE, col = "blue", pch = 16)
abline(h = 0, lty = "dashed")
abline(v = 0, lty = "dashed")

scores.CC <- CC.butterfly.candisc$scores
U1 <- scores.CC$X[, 1]
V1 <- scores.CC$Y[, 1]
identify(U1, V1, labels = rownames(gen), cex = 0.6)
