
#################################################################
#https://www.kaggle.com/datasets/andyesi/2019-ironman-world-championship-results
# Scale the values da 1 a 2
scale_x <- function(x) {
  orig_min <- min(x)
  orig_max <- max(x)
  scaled_x <- ((x - orig_min) / (orig_max - orig_min)) * (2 - 1) + 1
  return(scaled_x)
}

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

df <- read.csv("C:/Users/xseri/Downloads/2019 Ironman World Championship Results.csv", header=TRUE)
df <- df[, 5:8]
df <- df[df$Division == "MPRO" |  df$Division == "F35-39"| df$Division == "M70-74", ]
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

df_cl <- df[, 2:ncol(df)]
df_cl_scaled <- scale_x(df_cl)
df_cl_h <- harmonic_scale(df_cl_scaled)



hc = hclust.vector(df_cl_h, method='ward')  #è il ward.D2
plot(hc)
NbClust(data = df_cl_h, min.nc = 2, max.nc = 6, method = "ward.D2")
# draw dendogram with red borders around the 5 clusters
rect.hclust(hc, k=2, border="red")


cluster <- cutree(hc, 3)
# Create scatter plot
ggData <- cbind(df_cl, cluster)
ggData <- cbind(ggData, df$Division)
ggData$cluster <- as.factor(ggData$cluster)
colors <- c("#377EB8","#4DAF4A", "#E41A1C")
df$Division <- as.factor(df$Division)
df$Division <- fct_inorder(df$Division)
ggData$cluster <- revalue(ggData$cluster, c("1" = "MPRO", "2" = "F35-39", "3" = "M70-74"))
#levels(df$Division[df$Division == "MPRO"]) <- 1
#plot reale
plot(df_cl, col = colors[df$Division], pch=16, cex=1.5,
     main="Clustering", oma=c(4,4,6,10))
legend("right", legend = levels(df$Division),col = colors, pch = 16, cex = 0.9,
       title = "Division", box.col = "white")


#plot col mio clustering
pairs(df_cl, col=colors[ggData$cluster], pch=16, cex=1.5,
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


#CONFRONTO CON MDENDRO
library(mdendro)
library(mclust) #class_error
df <- read.csv("C:/Users/xseri/Downloads/2019 Ironman World Championship Results.csv", header=TRUE)
df <- df[, 5:8]
df <- df[df$Division == "MPRO" |  df$Division == "F35-39"| df$Division == "M70-74", ]
#df <- na.omit(df)  #for simplicity, remove rows of people that didn't complete the race
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
df$Division[df$Division == "MPRO"] <- "1"
df$Division[df$Division == "F35-39"] <- "2"
df$Division[df$Division == "M70-74"] <- "3"
colnames(df) <- c('Division', 'Swim_speed', 'Bike_speed', 'Marathon_speed')
rownames(df) <- NULL

df_cl <- df[, 2:ncol(df)]
d = dist(df_cl)
lnk <- linkage(d, method = "versatile", par.method = -1)
plot(lnk)
lnk.hcl = as.hclust(lnk)
cluster <- cutree(lnk.hcl, 3)
# Create scatter plot
ggData_mdendro <- cbind(df_cl, cluster)
ggData_mdendro <- cbind(ggData_mdendro, df$Division)
ggData_mdendro$cluster <- as.factor(ggData_mdendro$cluster)
df$Division <- fct_inorder(df$Division)
ggData_mdendro$cluster <- revalue(ggData_mdendro$cluster, c("1" = "MPRO", "2" = "F35-39", "3" = "M70-74"))
ggData_mdendro <- cbind(df_cl, cluster)
ggData_mdendro$cluster <- as.factor(ggData_mdendro$cluster)
colors <- c("#377EB8","#4DAF4A", "#E41A1C")


#plot mdendro
plot(df_cl, col = colors[ggData_mdendro$cluster], pch=16, cex=1.5,
     main="Clustering")


# Create a confusion matrix
conf_matrix <- confusionMatrix(ggData_mdendro$cluster, df$Division)
conf_matrix
c_error <- classError(ggData$cluster, df$Division)
c_error
#adj.rand.index(ggData$cluster, df$Division)




###################################################################