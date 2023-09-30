dat <- read.csv("female_sparrows.csv", header = TRUE)
head(dat)
y <- dat$survival
X <- dat[,-1]

head(X)
survivors <- X[y == TRUE, ]
nonsurvivors <- X[y == FALSE, ]
n1 <- nrow(survivors)
n2 <- nrow(nonsurvivors)

m1 <- colMeans(survivors)
m2 <- colMeans(nonsurvivors)
S1 <- cov(survivors)
S2 <- cov(nonsurvivors)

S <- ((n1 - 1) * S1 + (n2 - 1) * S2)/(n1 + n2 - 2)


## linear discriminant function
#L1
a1 <- as.numeric(-1/2 * t(m1) %*% solve(S) %*% m1) # intercept
b1 <- as.numeric(solve(S) %*% m1) #coefficient

#L2
a2 <- as.numeric(-1/2 * t(m2) %*% solve(S) %*% m2) # intercept
b2 <- as.numeric(solve(S) %*% m2) #coefficient

res <- rbind(L1 = b1, L2 = b2)
colnames(res) <- colnames(X)
res

new_obs <- dat[1, -1]
L1_new <- a1 + sum(b1 * new_obs)
L2_new <- a2 + sum(b2 * new_obs)


## canoical discriminant function
library(MASS)
fit.lda <- lda(y ~ ., data = X) #min(G-1, p) = 1
fit.lda$scaling


# Skulls
skulls <- read.csv("Egyptian skulls.csv", header = TRUE)
head(skulls)
group <- skulls$Period
X <- skulls[, -1]

## linear discriminant function
my_discrimant <- function(X, group) {
  
  group_name <- unique(group)
  G <- length(group_name)
  n <- length(group)
  p <- ncol(X)
  
  m <- matrix(0, G, p)
  S <- matrix(0, p, p)
  
  for (i in 1:G) {
    y.g <- group[group == group_name[i]]
    X.g <- X[group == group_name[i], ]
    n.g <- length(y.g)
    m[i, ] <- colMeans(X.g)
    S <- S + (n.g - 1) * cov(X.g)/(n - G)
  }
  
  results <- matrix(NA, nrow = G, ncol = p + 1)
  for (i in 1:G) {
    alpha <- as.numeric(-1/2 * t(m[i, ] %*% solve(S) %*% m[i, ]))
    beta <- as.numeric(solve(S) %*% m[i, ])
    results[i, ] <- c(alpha, beta)
  }
  
  colnames(results) <- c("intercept", colnames(X))
  rownames(results) <- group_name
  
  results
}

my_discrimant(X, group)

## canoical discriminant function
skulls.lda <- lda(Period ~ ., data = skulls) #min(G-1, p) = 4
skulls.lda$scaling
pred <- predict(skulls.lda, skulls)
data.frame(group, pred$class)



