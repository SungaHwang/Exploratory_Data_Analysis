dat <- read.csv(file = "exam1.csv", header = T)
head(dat)
score <- dat$score

N <- length(score)
sscore <- sort(score)
cbind(order = 1:N, sscore)

p <- 0.5
k_p <- N * p + p/3 + 1/3
k1_p <- floor(k_p)
k2_p <- k1_p + 1
w <- k_p - k1_p
q_p <- (1 - w) * sscore[k1_p] + w * sscore[k2_p]
q_p

# my function for quantile
my_quantile <- function(y, p) {
  x <- sort(y)
  k_p <- N * p + p/3 + 1/3
  k1_p <- floor(k_p)
  k2_p <- k1_p + 1
  w <- k_p - k1_p
  q_p <- (1 - w) * sscore[k1_p] + w * sscore[k2_p]
  return(q_p)
}

my_quantile(score, 0.0625)
quantile(score, 0.0625, type = 8)
quantile(score, c(0.5, 0.25, 0.125, 0.0625), type = 8)