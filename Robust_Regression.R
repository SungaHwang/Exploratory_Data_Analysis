# R codes used in the video
data("stackloss")
attach(stackloss)

#Y ~ X
m1 <- lm(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc., data = stackloss)
m1
summary(m1)
res_m1 <- resid(m1)
qqnorm(res_m1)
boxplot(res_m1)

library(MASS)
m2 <- rlm(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc., data = stackloss)
m3 <- lqs(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc., method = "lms", data = stackloss)
m4 <- lqs(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc., method = "lts", quantile = 16, data = stackloss)

res_m2 <- resid(m2)
res_m3 <- resid(m3)
res_m4 <- resid(m4) # LTS

boxplot(res_m1, res_m2, res_m3, res_m4)


sort(abs(res_m4), decreasing = TRUE)

stackloss2 <- stackloss[-c(21, 4, 3, 1), ]
final <- lm(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc., data = stackloss2)
final
summary(final)
qqnorm(resid(final))
boxplot(resid(final))