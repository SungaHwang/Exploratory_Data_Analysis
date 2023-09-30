dat <- read.csv("female_sparrows.csv", header=TRUE)
head(dat)

group1 <- dat[dat$survival == TRUE, -1]
group1
group2 <- dat[dat$survival == FALSE, -1]
group2

n1 <- nrow(group1)
n1
n2 <- nrow(group2)
n2

m1 <- colMeans(group1)
m1
m2 <- colMeans(group2)
m2
c1 <- cov(group1)
c1
c2 <- cov(group2)
c2

# estimate for the pooled covariance matrix
Cp <- ((n1-1)*c1 + (n2-1)*c2)/(n1+n2-2)
Cp

# hotelling's T^2
T2 <- n1 * n2 *(t(m1 - m2) %*% solve(Cp) %*% (m1 - m2))/(n1 + n2)
T2
Fvalue <- (n1 + n2 - p - 1) * T2 / ((n1 + n2 - 2) * p)
pvalue <- 1-pf(Fvalue, p, n1+n2-p-1)
Fvalue
