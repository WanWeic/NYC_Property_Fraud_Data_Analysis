data = read.csv('Final_Data.csv')
data_pca = data[,49:93]
library(ggplot2)
library(factoextra)
library(dplyr)
# PCA

pca_result = prcomp(data_pca, center = T, scale = T)
## if we do not do z-scale manually, we use "center = T, scale = T" and get the same output

plot(pca_result, xlab = "Principal Component")
fviz_eig(pca_result)

## or you can use ggplot to draw the scree plot using table below!
pve=data.frame(PC=seq(1,45,1),pve=pca_result$sdev^2/sum(pca_result$sdev^2))

## plot of proportion of variance explained for all PCs and top 20 PCs

plot(pve, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained")
plot(pve[1:20,], xlab = "Principal Component",
     ylab = "Proportion of Variance Explained")

## According to the analysis above, I would cut at 10th PC

plot(pve[1:10,],xlab='Principal Component',ylab='Proportion of Variance Explained',
     type='b',main='Scree Plot')

pve[1:10,] %>%
  ggplot(aes(x = PC,y = cumsum(pve))) +
  geom_point() +
  xlab("Principal Component") +
  ylab("Cumulative Proportion of Variance Explained")

# Keep PC1 to PC10
PCA = data.frame(pca_result$rotation)
PCA = PCA[,1:10]

# multiply it back to the original data
data_afterpca = data.frame(as.matrix(data_pca) %*% as.matrix(PCA))

# Z-scale the PCA results again
data_scale = scale(data_afterpca)

save(data_scale, file = "data_afterpca.csv")
