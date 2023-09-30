
dat <- read.csv("female_sparrows.csv", header = TRUE)
head(dat) # two groups: survival, nonsurvival

p <- ncol(dat) - 1
group1 <- dat[dat$survival == TRUE, -1]
group2 <- dat[dat$survival == FALSE, -1]

C1 <- cov(group1)
C2 <- cov(group2)
n1 <- nrow(group1)
n2 <- nrow(group2)

# the pooled covariance matrix
C <- ((n1 - 1) * C1 + (n2 - 1) * C2)/(n1 + n2 - 2)
M <- (det(C1)^((n1 - 1)/2) * det(C2)^((n2 - 1)/2))/(det(C)^((n1 + n2 -2)/2))


c1 <- (2*p^2 + 3*p - 1) * (1/(n1-1) + 1/(n2-1) - 1/(n1+n2-2))/(6*(p + 1))
c2 <- (p - 1) * (p + 2) * (1/(n1-1)^2 + 1/(n2-1)^2 - 1/(n1+n2-2)^2)/6
v1 <- p*(p+1)/2
v2 <- (v1+2)/(c2 - c1^2)
b <- (1 - c1 - v1/v2)/v1

Fvalue <- -2 * b * log(M)

# p-value
1 - pf(Fvalue, v1, v2)

#### skull
dat <- read.csv("ex2-skulls.csv", header = TRUE)
x <- dat[, -1]
group <- dat[, 1]

fn_boxM <- function(x, group) {
  
  name_group <- unique(group)
  m <- length(name_group)
  n <- nrow(x)
  
  log_M_num <- 0
  nset <- c()
  Cp <- 0
  
  for (i in 1:m) {
    groupi <- x[group == name_group[i], ]
    Ci <- cov(groupi)
    ni <- nrow(groupi)
    nset[i] <- ni
    log_M_num <- log_M_num + (ni - 1)/2 * log(det(Ci))
    Cp <- Cp + ((ni - 1) * Ci)/(n - m)
  }
  
  log_M_den <- ((n - m)/2) * log(det(Cp))
  log_M <- log_M_num - log_M_den
  
  c1 <- (2*p^2 + 3*p - 1) * (sum(1/(nset - 1)) - 1/(n-m))/(6*(p + 1)*(m-1))
  c2 <- (p - 1) * (p + 2) * (sum(1/(nset - 1)^2) - 1/(n-m)^2)/(6 * (m-1))
  v1 <- p*(p+1)*(m-1)/2
  v2 <- (v1+2)/(c2 - c1^2)
  b <- (1 - c1 - v1/v2)/v1
  
  if (c2 > c1^2) {
    Fvalue <- -2*b*log_M
  }
  
  if (c2 < c1^2) {
    b1 <- (1 - c1 - 2/v2)/v2
    Fvalue <- - (2*b*log_M)/(v1 + 2*b1*log(M))
  }

  pvalue <- 1 - pf(Fvalue, v1, v2)
  return(pvalue)
  
}

fn_boxM(x, group)
