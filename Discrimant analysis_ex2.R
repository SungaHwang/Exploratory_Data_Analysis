finance <- read.csv("finance.csv", header =TRUE)
head(finance)

## 선형판별분석
library(MASS)
finance.lda <- lda(y ~ x1 + x2 + x3 + x4, data = finance)
print(finance.lda)

pred1 <- predict(finance.lda, finance)
finance.pred1 <- cbind(finance, pred1$x, pred1$posterior, pred1$class)


predict(finance.lda, data.frame(x1 = -0.5, x2 = -0.4, x3 = 1.02, x4 = 0.45))

finance.mss1 <- table(finance$y, pred1$class)
install.packages("DescTools")
library(DescTools)
Desc(finance.mss1, digits = 2)

# testing homogeneity
library(biotools)
finance.X <- finance[, 2:5]
finance.boxM <- boxM(finance.X, finance$y)
finance.boxM


## Quadratic discriminant analysis
finance.qda <- qda(y ~ x1 + x2 + x3 + x4, data = finance)
pred2 <- predict(finance.qda, finance)
finance.mss2 <- table(finance$y, pred2$class)
Desc(finance.mss2, digits = 2)


install.packages("klaR")
library(klaR)
finance$y <- as.factor(finance$y)
partimat(y ~ x1 + x2 + x3 + x4, data = finance, method = "lda")
partimat(y ~ x1 + x2 + x3 + x4, data = finance, method = "qda")

