
#####################################################
####################################################
####################################################
harmonic_scale <- function(x) {
  return (1/x)
}

library(caret)  #confusion matrix
library(lubridate) #period to seconds
library(pdfCluster) #adjusted rand index
library(fastcluster)  #hclust.vector
library(NbClust) #Find number of optimal clusters
library(mclust) #class_error
library(forcats) #To order factors based on their order of appearance in R
library(plyr) #to use revalue
library(pvclust)
#library(fpc)  #for calinhara
library(factoextra) # for elbow method
library(dendextend)  #to draw dendrogram



df <- read.csv("C:/Users/xseri/Downloads/2019 Ironman World Championship Results.csv", header=TRUE)
df <- df[, 5:8]
df <- df[df$Division == "MPRO" |df$Division =="F35-39" |  df$Division == "F65-69" | df$Division == "M70-74", ]
df <- df[-which(df$Swim == "" | df$Bike == "" | df$Run == ""), ] #for simplicity, remove rows of people that didn't complete the race
df$Swim <- period_to_seconds(hms(df$Swim))
df$Swim <- as.numeric(df$Swim)
df$Bike <- period_to_seconds(hms(df$Bike))
df$Bike <- as.numeric(df$Bike)
df$Run <- period_to_seconds(hms(df$Run))
df$Run <- as.numeric(df$Run)
df$Swim <- 3860/df$Swim   #speed of each person swimming in m/s
df$Bike <- 185070/df$Bike   #speed of each person bike in m/s
df$Run <- 42195/df$Run      #speed of each person in marathon in m/s
colnames(df) <- c('Division', 'Swim_speed', 'Bike_speed', 'Marathon_speed')
rownames(df) <- NULL


# Assuming your data frame is named 'df'
for (i in 109:147) {
  if (df$Division[i] != "F65-69") {
    df[i, ] <- NA  # Replace rows with NA if Division is not "F35-39"
  }
}
for (i in 148:nrow(df)) {
  if (df$Division[i] != "M70-74") {
    df[i, ] <- NA  
  }
}
# Remove rows with NA
df <- df[!is.na(df$Division), ]
row.names(df) <- NULL

df_cl <- df[, 2:ncol(df)]
#df_cl_scaled <- scale_x(df_cl)
df_cl_h <- harmonic_scale(df_cl)


hc = hclust.vector(df_cl_h, method='ward')  #è il ward.D2
res <- NbClust(data = df_cl_h, min.nc = 2, max.nc = 6, method = "ward.D2")
plot(hc, labels=FALSE)
# draw dendogram with red borders around the 5 clusters
rect.hclust(hc, k=4, border="red")

2

# Elbow method
fviz_nbclust(df_cl_h, hcut, method = "wss", hc_method = "ward.D2") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

#gaps statistics
a <- dist(df_cl_h)
# Compute gap statistics using clusGap with Ward's method
gap_results <- clusGap(a, FUNcluster = function(x, k) agnes(x, diss = TRUE, method = "ward"), K.max =10, B = 100)
# Plot the gap statistics
plot(gap_results, main = "Gap Statistics for Cluster Count")
# Find the optimal number of clusters based on the gap statistic
optimal_k <- maxSE(gap_results)$Cluster.number
cat("Optimal number of clusters:", optimal_k, "\n")


# CH index
cluster <- cutree(hc, 4)
#calinhara(df_cl_h, cluster)

# Create scatter plot
ggData <- cbind(df_cl, cluster)
ggData <- cbind(ggData, df$Division)
ggData$cluster <- as.factor(ggData$cluster)
colors <- c("#377EB8","#4DAF4A", "#E41A1C", "#DEC20B")
df$Division <- as.factor(df$Division)
df$Division <- fct_inorder(df$Division)
ggData$cluster <- revalue(ggData$cluster, c("1" = "MPRO", "2" = "F35-39", "3" = "F65-69", "4" = "M70-74"))
#levels(df$Division[df$Division == "MPRO"]) <- 1

#plot reale senza ground truth
plot(df_cl, pch=20, cex=1.5,
     main="Clustering")

#dendrogram with colors
dend <- as.dendrogram(hc)
d1=color_branches(dend,k=4, col = colors)
labels_colors(d1) <- "white"
#d1 <- set(d1, "labels_cex", 0)  # Set the label size to 0 (effectively hiding them)
plot(d1, main = "Cluster dendrogram") # selective coloring of branches :)
#d2=color_branches(d1,k=5) # auto-coloring 5 clusters of branches.
#plot(d2)


#plot reale
plot(df_cl, col = colors[df$Division], pch=20, cex=1.5,
     main="Clustering", oma=c(4,4,6,10))
legend("right", legend = levels(df$Division),col = colors, pch = 16, cex = 0.9,
       title = "Division", box.col = "white")


#plot col mio clustering
pairs(df_cl, col=colors[ggData$cluster], pch=20, cex=1.5,
      main="Clustering", oma=c(4,4,6,10))
par(xpd = TRUE)
legend("right", legend = levels(ggData$cluster),col = colors, pch = 16, cex = 0.9,
       title = "Division", box.col = "white")


df$Division <- as.factor(df$Division)
conf_matrix <- confusionMatrix(ggData$cluster, df$Division)
conf_matrix
c_error <- classError(ggData$cluster, df$Division)
c_error
#adj.rand.index(ggData$cluster, df$Division)


# Create the scatterplot per misclassified
# Define the shapes for misclassified and correctly classified observations
shapes <- ifelse(1:nrow(df_cl) %in% c_error$misclassified, 4, 20)  # 17 represents a different shape

# Plot the pairs with different shapes for misclassified observations
pairs(df_cl, pch = shapes, col = colors[ggData$cluster], main="Clustering", cex=1.5, oma=c(4,4,6,10))
par(xpd = TRUE)
legend("right", legend = levels(df$Division),col = colors, pch = 16, cex = 0.9,
       title = "Division", box.col = "white")





#######################################################
#######################################################
#mdendro
library(mdendro)
library(mclust) #class_error
df <- read.csv("C:/Users/xseri/Downloads/2019 Ironman World Championship Results.csv", header=TRUE)
df <- df[, 5:8]
df <- df[df$Division == "MPRO" |df$Division =="F35-39" |  df$Division == "F65-69" | df$Division == "M70-74", ]
df <- df[-which(df$Swim == "" | df$Bike == "" | df$Run == ""), ] #for simplicity, remove rows of people that didn't complete the race
df$Swim <- period_to_seconds(hms(df$Swim))
df$Swim <- as.numeric(df$Swim)
df$Bike <- period_to_seconds(hms(df$Bike))
df$Bike <- as.numeric(df$Bike)
df$Run <- period_to_seconds(hms(df$Run))
df$Run <- as.numeric(df$Run)
df$Swim <- 3860/df$Swim   #speed of each person swimming in m/s
df$Bike <- 185070/df$Bike   #speed of each person bike in m/s
df$Run <- 42195/df$Run      #speed of each person in marathon in m/s
colnames(df) <- c('Division', 'Swim_speed', 'Bike_speed', 'Marathon_speed')
rownames(df) <- NULL


# Assuming your data frame is named 'df'
for (i in 109:147) {
  if (df$Division[i] != "F65-69") {
    df[i, ] <- NA  # Replace rows with NA if Division is not "F35-39"
  }
}
for (i in 148:nrow(df)) {
  if (df$Division[i] != "M70-74") {
    df[i, ] <- NA  
  }
}
# Remove rows with NA
df <- df[!is.na(df$Division), ]


df_cl <- df[, 2:ncol(df)]

d = dist(df_cl)
lnk <- linkage(d, method = "versatile", par.method = -1)
plot(lnk)
lnk.hcl = as.hclust(lnk)
cluster <- cutree(lnk.hcl, 4)
# Create scatter plot
ggData_mdendro <- cbind(df_cl, cluster)
ggData_mdendro <- cbind(ggData_mdendro, df$Division)
ggData_mdendro$cluster <- as.factor(ggData_mdendro$cluster)
df$Division <- as.factor(df$Division)
df$Division <- fct_inorder(df$Division)
ggData_mdendro$cluster <- revalue(ggData_mdendro$cluster, c("1" = "MPRO", "2" = "F35-39", "3" = "F65-69", "4" = "M70-74"))
colors <- c("#377EB8","#4DAF4A", "#E41A1C", "#DEC20B")


#plot mdendro
plot(df_cl, col = colors[ggData_mdendro$cluster], pch=20, cex=1.5,
     main="Clustering", oma=c(4,4,6,10))
par(xpd = TRUE)
legend("right", legend = levels(ggData_mdendro$cluster),col = colors, pch = 20, cex = 0.9,
       title = "Division", box.col = "white")




# Create a confusion matrix
conf_matrix <- confusionMatrix(ggData_mdendro$cluster, df$Division)
conf_matrix
c_error <- classError(ggData$cluster, df$Division)
c_error