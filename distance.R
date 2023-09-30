dat <- read.csv("ex2-skulls.csv", header = TRUE)
dat

head(dat)
group <- dat[, 1]
X <- dat[, -1]



# euclidean
my_euclidean <- function(X, group) {
  
  group_name <- unique(group)
  ngroup <- length(group_name)
  
  distance <- matrix(NA, nrow = 5, ncol = 5)
  
  for (i in 1:ngroup) {
    
    for (j in i:ngroup) {
      
      mui <- colMeans(X[group == group_name[i], ])
      muj <- colMeans(X[group == group_name[j], ])
      
      distance[i, j] <- sqrt(sum((mui - muj)^2))
      
    }
    
  }
  
  colnames(distance) <- rownames(distance) <- group_name
  
  return(distance)
  
}

# penrose
my_penrose <- function(X, group) {
  
  group_name <- unique(group)
  ngroup <- length(group_name)
  
  distance <- matrix(NA, nrow = 5, ncol = 5)
  cov_X <- cov(X)
  V <- diag(cov_X)
  p <- ncol(X)
  
  for (i in 1:ngroup) {
    
    for (j in i:ngroup) {
      
      mui <- colMeans(X[group == group_name[i], ])
      muj <- colMeans(X[group == group_name[j], ])
      
      distance[i, j] <- sqrt(sum((mui - muj)^2/(p*V)))
      
    }
    
  }
  
  colnames(distance) <- rownames(distance) <- group_name
  
  return(distance)
  
}




# mahalanobis
my_mahalanobis <- function(X, group) {
  
  group_name <- unique(group)
  ngroup <- length(group_name)
  
  distance <- matrix(NA, nrow = 5, ncol = 5)
  cov_X <- cov(X)

  for (i in 1:ngroup) {
    
    for (j in i:ngroup) {
      
      mui <- colMeans(X[group == group_name[i], ])
      muj <- colMeans(X[group == group_name[j], ])
      
      distance[i, j] <- t(mui - muj) %*% solve(cov_X) %*% (mui - muj)
      
    }
    
  }
  
  colnames(distance) <- rownames(distance) <- group_name
  
  return(distance)
  
}

#
my_euclidean(scale(X), group)
my_penrose(scale(X), group)
my_mahalanobis(scale(X), group)

##
my_distance <- function(X, group, type = "euclidean") {
  
  group_name <- unique(group)
  ngroup <- length(group_name)
  
  distance <- matrix(NA, nrow = 5, ncol = 5)
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
  
  return(distance)
  
}

my_distance(X, group, type = "euclidean")
my_distance(X, group, type = "penrose")
my_distance(X, group, type = "mahalanobis")

install.packages("vegan")

library(vegan)

dist_euclidean <- my_distance(X, group, type = "euclidean")

dist_mahalanobis <- my_distance(X, group, type = "mahalanobis")



result <- mantel(dist_euclidean, dist_mahalanobis)


result

