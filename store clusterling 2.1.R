library(class)
library(factoextra)
library(ggplot2)
library(caret)
library(cluster)
library(gridExtra)


## read and merge raw data
# raw data downloaded from 1010, basic financial matrix by store, change path before running
store.finance <- read.csv("S:/B&M/Analytics & Store Segmentation/DATA/raw/2018Q4_clustering.csv",
                          header = T)
# current active store list, change path before running
# original store ops list located in: Q:\Store Ops\Master Store List
active.store <- read.csv("S:/B&M/Analytics & Store Segmentation/DATA/Active Stores 201808.csv",
                         header = T)
# join tables to eliminate closed stores
stores.kmean <- merge.data.frame(x = store.finance, y = active.store[,1:2], by.x = "Store", by.y = "Store")
stores.kmean$Regsiters <- NULL
# read EU data
stores.kmean.ceu <- read.csv("S:/B&M/Analytics & Store Segmentation/DATA/raw/D2_2018_Q2_clustering_data_eu.csv",
                          header = T)


## prepare for clustering
# use store number as row names
stores.kmean$row <- paste0(stores.kmean$Store)
stores.kmean$Store <- NULL
rownames(stores.kmean) <- stores.kmean$row
stores.kmean$row <- NULL
# same thing for EU
stores.kmean.ceu$row <- paste0(stores.kmean.ceu$Store)
stores.kmean.ceu$Store <- NULL
rownames(stores.kmean.ceu) <- stores.kmean.ceu$row
stores.kmean.ceu$row <- NULL
stores.kmean.ceu$Division <- NULL
stores.kmean.ceu.ori <- stores.kmean.ceu

# split file by divisions
stores.kmean.cna <- stores.kmean[stores.kmean$Division=="NA CLAIRES",]
# delete column "Division"
stores.kmean.cna$Division <- NULL

stores.kmean.ici <- stores.kmean[stores.kmean$Division=="NA ICING",]
stores.kmean.ici$Division <- NULL


## normalizing data
# build a normalizing function to use for each column
normalizing <- function(index,data) {
  (data[,index] - mean(data[,index])) / sd(data[,index])
}
# apply the function to different columns, and out put normalized data into a list
kmeanlist.cna <- lapply(1:8, FUN = normalizing, data = stores.kmean.cna)
kmeanlist.ici <- lapply(1:8, FUN = normalizing, data = stores.kmean.ici)
kmeanlist.ceu <- lapply(1:8, FUN = normalizing, data = stores.kmean.ceu)

# add store numbers and divisions to the normalized list
kmeanlist.cna[[9]] <- rownames(stores.kmean.cna)
# do the same for Icing and EU
kmeanlist.ici[[9]] <- rownames(stores.kmean.ici)
kmeanlist.ceu[[9]] <- rownames(stores.kmean.ceu)
# rename the list with correct column names
names(kmeanlist.cna) <- c(colnames(store.finance[3:10]),colnames(store.finance[2]))
names(kmeanlist.ici) <- c(colnames(store.finance[3:10]),colnames(store.finance[2]))
names(kmeanlist.ceu) <- c(colnames(stores.kmean.ceu[1:8]),"Store")
# convert list to a dataframe
stores.kmean.cna <- as.data.frame(kmeanlist.cna)
rownames(stores.kmean.cna) <- stores.kmean.cna$Store
stores.kmean.cna$Store <- NULL

stores.kmean.ici <- as.data.frame(kmeanlist.ici)
rownames(stores.kmean.ici) <- stores.kmean.ici$Store
stores.kmean.ici$Store <- NULL

stores.kmean.ceu <- as.data.frame(kmeanlist.ceu)
rownames(stores.kmean.ceu) <- stores.kmean.ceu$Store
stores.kmean.ceu$Store <- NULL


## clustering k-mean
# always set seed so the results will remain the same as long as the input data did not change
set.seed(1234)

## finding elbow
# run the whole block of code below to brose the elbow
wss <- sapply(1:20, 
              function(k){kmeans(stores.kmean.cna, k, nstart=100 )$tot.withinss})
elbow.k <- recordPlot()
plot(1:20, wss,
     type = "b", pch = 19, frame = T, 
     xlab = "Number of clusters K",
     ylab = "Total within-clusters sum of squares",
     main = "Elbow Number of Clusters",
     col = "blue",
     xaxt = "n")
axis(1, at = seq(1, 20, by = 1), las = 1)
# run the whole block of code above to brose the elbow

# another way to see elbow
elbow.k.fviz <- fviz_nbclust(stores.kmean.cna, kmeans, method = "wss")

# run the whole block of code below to brose the elbow
wss <- sapply(1:20, 
              function(k){kmeans(stores.kmean.ici, k, nstart=100 )$tot.withinss})
elbow.k <- recordPlot()
plot(1:20, wss,
     type = "b", pch = 19, frame = T, 
     xlab = "Number of clusters K",
     ylab = "Total within-clusters sum of squares",
     main = "Elbow Number of Clusters",
     col = "blue",
     xaxt = "n")
axis(1, at = seq(1, 20, by = 1), las = 1)
# run the whole block of code above to brose the elbow

# another way to see elbow
elbow.k.fviz <- fviz_nbclust(stores.kmean.ici, kmeans, method = "wss")

# same for eu
wss <- sapply(1:20, 
              function(k){kmeans(stores.kmean.ceu, k, nstart=100 )$tot.withinss})
elbow.k <- recordPlot()
plot(1:20, wss,
     type = "b", pch = 19, frame = T, 
     xlab = "Number of clusters K",
     ylab = "Total within-clusters sum of squares",
     main = "Elbow Number of Clusters",
     col = "blue",
     xaxt = "n")
axis(1, at = seq(1, 20, by = 1), las = 1)
# run the whole block of code above to brose the elbow

# another way to see elbow
elbow.k.fviz <- fviz_nbclust(stores.kmean.ceu, kmeans, method = "wss")

# ## another way of deciding which k we should use, corrently not using
# ## try average silhouette
# 
# sil <- rep(0, 20)
# # Compute the average silhouette width for 
# for(i in 2:20){
#   km.res <- kmeans(stores.kmean, centers = i, nstart = 100)
#   ss <- silhouette(km.res$cluster, dist(stores.kmean))
#   sil[i] <- mean(ss[, 3])
# }
# # Plot the  average silhouette width
# plot(1:20, sil,
#      type = "b", pch = 19, frame = T, 
#      xlab = "Number of clusters K",
#      ylab = "Average Silhouette",
#      main = "Avg. Silhouette Number of Clusters",
#      col = "blue",
#      xaxt = "n")
# axis(1, at = seq(1, 20, by = 1), las = 1)
# abline(v = which.max(sil), lty = 2)
# ## end of block



## produce k-mean clustering with selected k
# clustering
k5 <- kmeans(stores.kmean.cna, centers = 5, nstart = 100)
# plot a reduced demention chart, for the idea of how was each clustering group different from each other
kplot5 <- fviz_cluster(k5, data = stores.kmean.cna, labelsize = 0) + ggtitle( "CNA k = 5")

k5i <- kmeans(stores.kmean.ici, centers = 5, nstart = 100)
kplot5i <- fviz_cluster(k5i, data = stores.kmean.ici, labelsize = 0) + ggtitle( "ICING k = 5")

k5e <- kmeans(stores.kmean.ceu, centers = 5, nstart = 100)
kplot5e <- fviz_cluster(k5e, data = stores.kmean.ceu, labelsize = 0) + ggtitle( "CEU k = 5")

# show the plots just made
grid.arrange(kplot5,kplot5i,kplot5e, nrow = 1)


## creating final result
# split the original data by division
stores.kmean.clustered.cna <- stores.kmean[stores.kmean$Division=="NA CLAIRES",]
# write clustering result
stores.kmean.clustered.cna$k5 <- as.character(k5$cluster)
stores.kmean.clustered.ici <- stores.kmean[stores.kmean$Division=="NA ICING",]
stores.kmean.clustered.ici$k5 <- as.character(k5i$cluster)
stores.kmean.clustered.ceu <- stores.kmean.ceu.ori
stores.kmean.clustered.ceu$Division <- "EU CLAIRES"
stores.kmean.clustered.ceu$k5 <- as.character(k5e$cluster)
stores.kmean.clustered.ceu <- stores.kmean.clustered.ceu[,c(9,1:8,10)]
# append results together
stores.kmean.clustered <- rbind(stores.kmean.clustered.cna,stores.kmean.clustered.ici,stores.kmean.clustered.ceu)
stores.kmean.clustered$Store <- row.names(stores.kmean.clustered)


# ## include traffic data in the clustering, not in use
# colnames(stores.kmean.org.backup) <- c("trans_count","sales","quantity","gross_margin","traffic","ads","upt","aur")
# stores.kmean.clustered <- merge(x = stores.kmean.clustered, y = stores.kmean.org.backup, by =0, all = T)
# stores.kmean.clustered$act_traffic <- stores.kmean.clustered$traffic
# stores.kmean.clustered$traffic[stores.kmean.clustered$traffic == 0] <- NA
# stores.kmean.clustered$traffic[!is.na(stores.kmean.clustered$traffic)] <- 1





########################
# output the result to a cvs file
# REMEMBER TO CHANGE FILE PATH!
write.csv(stores.kmean.clustered, "S:/B&M/Analytics & Store Segmentation/DATA/Clustering_Result_2018_no_D4_Q4.csv", row.names = F)
########################


## Density plotting
# create a function to plot density of a demention
dens_plot <- function(i,df) {
  ggplot(df, aes(x=df[,i], fill=k5, color=k5))+
  geom_density(alpha=0.2)+
  xlab(colnames(df[i]))
}
# apply the function to each demention
c_dens_plot <- lapply(2:9, dens_plot, df = stores.kmean.clustered.cna)
# output the group of plots
do.call(grid.arrange,c(c_dens_plot,nrow=2))
# apply the function to each demention
i_dens_plot <- lapply(2:9, dens_plot,df = stores.kmean.clustered.ici)
# output the group of plots
do.call(grid.arrange,c(i_dens_plot,nrow=2))
# apply the function to each demention
e_dens_plot <- lapply(2:9, dens_plot,df = stores.kmean.clustered.ceu)
# output the group of plots
do.call(grid.arrange,c(e_dens_plot,nrow=2))


## plotting result Histogram
h_c <- ggplot(stores.kmean.clustered.cna, aes(x = k5))+
          geom_bar(fill = "purple", alpha = 0.7)+
          geom_text(stat='count', aes(label=..count..), vjust = 2)+
          ggtitle("NA Claire's K-mean Clusting")
h_i <- ggplot(stores.kmean.clustered.ici, aes(x = k5))+
          geom_bar(fill = "blue", alpha = 0.5)+
          geom_text(stat='count', aes(label=..count..), vjust = 2)+
          ggtitle("NA Icing K-mean Clusting")
h_e <- ggplot(stores.kmean.clustered.ceu, aes(x = k5))+
          geom_bar(fill = "green", alpha = 0.5)+
          geom_text(stat='count', aes(label=..count..), vjust = 2)+
          ggtitle("EU Claire's K-mean Clusting")
grid.arrange(h_c,h_i,h_e, nrow=1)
