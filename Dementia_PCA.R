dat <- read.csv("dementia.csv", header = TRUE)
subdat <- dat[c("ADAS11", "ADAS13", "MMSE", "Ventricles", "Hippocampus", "WholeBrain", "Entorhinal_cortex", "Fusiform_gyrus", "Middle_temporal_gyrus")]
subdat <- na.omit(subdat)
res <- prcomp(subdat, scale.=TRUE)
res

#install.packages("factoextra")
library(factoextra)
fviz_eig(res)
summary(res)
fviz_contrib(res, choice = "var", axes = 1)
fviz_contrib(res, choice = "var", axes = 2)
fviz_pca_var(res, col.var = "cos2", gradient.cols = c("#00AFCC", "#E7B828", "#FD4E97"), repel = TRUE)


loadings <- res$rotation
plot(loadings[,2] ~ loadings[, 1], main = "Principal Component Loadings", pch = 20, col ="grey", xlim = c(-1, 1), ylim = c(-1, 1))
text(y = loadings[, 2], x = loadings[, 1], label = colnames(subdat), cex = 0.8)
for ( i in 1:9) {
  arrows(0, 0, 0.8 * loadings[i, 1], 0.8*loadings[i, 2], length = 0.1)
}

