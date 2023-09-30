getwd()

sparrows <- read.csv("Bumpus_sparrows.csv", header = TRUE)
head(sparrows)
group1 <- sparrows[sparrows$Survivorship == "S", -1]
group2 <- sparrows[sparrows$Survivorship == "NS", -1]

## Q1
colMeans(group1)
colMeans(group2)

## Q2
install.packages("Hotelling")
library(Hotelling)

head(sparrows)
result <- hotelling.test(Total_length + Alar_extent + L_beak_head + 
                 L_humerous + L_keel_sternum ~ Survivorship, data = sparrows)
result

## Q3
group1 #survival
group2 #nonsurvival

cov(group1)
cov(group2)

## Q4
install.packages("heplots")
library(heplots)
result <- boxM(cbind(Total_length, Alar_extent, L_beak_head, 
           L_humerous, L_keel_sternum) ~ Survivorship, data = sprarrows)
result


## Q5
skulls <- read.csv("Egyptian skulls.csv", header = TRUE)
head(skulls)
X <- skulls[, -1]
group <- skulls[, 1]
unique(group)
aggregate(X, by = list(group), FUN = mean)


# my function for distances
my_distance <- function(X, group, type = "euclidean") {
  
  group_name <- unique(group)
  ngroup <- length(group_name)
  
  distance <- matrix(0, nrow = 5, ncol = 5)
  cov_X <- cov(X)
  V <- diag(cov_X)
  p <- ncol(X)
  
  for (i in 1:ngroup) {
    
    for (j in i:ngroup) {
      
      mui <- colMeans(X[group == group_name[i], ])
      muj <- colMeans(X[group == group_name[j], ])
      
      if (type == "euclidean") distance[i, j] <- sqrt(sum((mui - muj)^2)) # euclidean
      if (type == "penrose") distance[i, j] <- sqrt(sum((mui - muj)^2/(p*V))) #penrose
      if (type == "mahalanobis") distance[i, j] <- t(mui - muj) %*% solve(cov_X) %*% (mui - muj) # mahalanobis
      
    }
    
  }
  
  
  colnames(distance) <- rownames(distance) <- group_name
  
  return(t(distance))
  
}

# Euclidean
dist_euclidean <- my_distance(X, group, type = "euclidean")
# Penrose
dist_penrose <- my_distance(X, group, type = "penrose")
# mahalanobis
dist_mahalanobis <- my_distance(X, group, type = "mahalanobis")

## Q6
install.packages("vegan")
library(vegan)
result <- mantel(dist_euclidean, dist_mahalanobis)
result




