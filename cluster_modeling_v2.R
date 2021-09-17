#Load Packages
library(here)
library(mclust)
library(raster)
library(rgdal)
library(sp)
library(RColorBrewer)
library(PCAtools)
library(sf)
library(tmap)
library(terra)
library(rgeos)
library(spData)
library(lattice)

#import raw data
dat_raw <- read.csv(here("data/CommunityData-raw-2015.csv"))
#select columns of interest, removed co2_hhs
dat  <- dat_raw[,c(4:12,14:21)]
#convert to dataframe
dat <- as.data.frame(unclass(dat))
#set rownames to city names
rownames(dat)=dat_raw$ME
#look at data
summary(dat)
head(dat)
# remove any records that have NAs
dat = na.omit(dat)
# convert to z-scores
dat <- as.matrix(scale(dat, center = TRUE, scale = TRUE))

#run mclust on 1 to 20 clusters
dat_mc <- Mclust(dat, G=c(1:20))
summary(dat_mc)
plot(dat_mc, what='BIC', ylim=c(-37000,-35000))
plot(dat_mc, what='BIC')
#model type VVE chosen as best, with similar BIC scores for 7-12 clusters
#to do additional model comparisons, generate and compare VVE models with 7-12 clusters
dat_mc9 <- Mclust(dat, G=9, modelNames="VVE")
dat_mc10 <- Mclust(dat, G=10, modelNames="VVE")
dat_mc11 <- Mclust(dat, G=11, modelNames="VVE")
dat_mc12 <- Mclust(dat, G=12, modelNames="VVE")

#extract BIC scores
BICs<-c(dat_mc9$BIC[1],dat_mc10$BIC[1],dat_mc11$BIC[1],dat_mc12$BIC[1])
#plot BIC scores
plot(c(9:12),BICs, pch=16, ylab="BIC Score",xlab="Number of Clusters", type='o')
#calculate change in BIC score, since in this method the goal to to maximize BIC
#we calculate change from highest scoring model
delta_BIC <- max(BICs) - BICs
#calculate BIC weights
w_BIC <- round(exp(-0.5*delta_BIC)/sum(exp(-0.5*delta_BIC)), digits=3)
#make table of results
results_table <- cbind.data.frame(clusters=c(9:12),BIC=BICs,delta_BIC,weight=w_BIC)
#order by delta
results_table <- results_table[order(delta_BIC),] 
rownames(results_table)<-NULL
results_table
#extract classifications, e.g., which city belongs to which cluster
dat_mc10$classification

#write csv file
write.csv(dat_mc10$classification,file=here("results/cluster_assignment.csv"))
data<-read.csv("results/cluster_assignment.csv")

#print table showing probability that MSA belongs to class
class_prob <- round(dat_mc10$z, digits=3)
class_prob

#cluster means
means <- dat_mc10$parameters$mean

####################################################################
# stratigraphic plot of probabilistic cluster assignments

class_prob_sort <- class_prob[order( -class_prob[,1], 
                                     -class_prob[,2],
                                     -class_prob[,3],
                                     -class_prob[,4],
                                     -class_prob[,5],
                                     -class_prob[,6],
                                     -class_prob[,7],
                                     -class_prob[,8],
                                     -class_prob[,9],
                                     -class_prob[,10]),]


heatmap(class_prob_sort, Rowv = NA, Colv = NA, 
        col = colorRampPalette(brewer.pal(8,"Blues"))(3))

# Plot a legend in bottom right part of heatmap
legend(x = "bottomright", legend = c("low", "medium", "high"),
       cex = 0.8, fill = colorRampPalette(brewer.pal(8, "Blues"))(3))


# equal-frequency class intervals -- chunk 1
plotvar = "POP1990"
plotvals <- orcounty_sf$POP1990
title <- "Population 1990"
subtitle <- "Quantile (Equal-Frequency) Class Intervals"
nclr <- 3
plotclr <- brewer.pal(3,"BuPu")
class <- classIntervals(plotvals, nclr, style="quantile")
colcode <- findColours(class, plotclr)

plot(orcounty_sf[plotvar], col=colcode, xlim=c(-124.5, -115), ylim=c(42,47))
title(main=title, sub=subtitle)
legend("bottomright", legend=names(attr(colcode, "table")),
       fill=attr(colcode, "palette"), cex=1.0, bty="n")

##########################################################3
#examine which clusters score highly in different metrics
#new data frame
dat_clust <- as.data.frame(dat)
#add names
dat_clust$ME <- rownames(dat)
rownames(dat_clust) <- NULL
#merge in cluster assignments
dat_clust <- merge(dat_clust,data,by.x="ME",by.y="X")
head(dat_clust)

#Poverty, low
boxplot(dat_clust$Per_Poverty~dat_clust$x, ylab='Poverty %', xlab="Cluster")
abline(h=0)
#Inequality, low
boxplot(dat_clust$Gini~dat_clust$x, ylab='Gini', xlab="Cluster")

#Food insecurity, low
boxplot(dat_clust$FOOD_INS~dat_clust$x, ylab='Food Insecurity', xlab="Cluster")

#Air Quality, high
boxplot(dat_clust$AQI_Good~dat_clust$x, ylab="Air Quality Index", xlab="Cluster")

#Health, low
boxplot(dat_clust$poor_health_percent~dat_clust$x, ylab="Poor Health %", xlab="Cluster")

#Crime, low
boxplot(dat_clust$VIO_CRIME~dat_clust$x, ylab="Violent Crime", xlab="Cluster")





#plot subsets

par(mfrow=c(2,5), mar=c(2.5,5,2.5,3))
boxplot(subset(dat_clust, x=="1")[2:18], horizontal=T, las=1, ylim=c(-4,4), main="Cluster 1")
abline(v=0)
boxplot(subset(dat_clust, x=="2")[2:18], horizontal=T, las=1, ylim=c(-4,4), main="Cluster 2")
boxplot(subset(dat_clust, x=="3")[2:18], horizontal=T, las=1, ylim=c(-4,4), main="Cluster 3")
boxplot(subset(dat_clust, x=="4")[2:18], horizontal=T, las=1, ylim=c(-4,4), main="Cluster 4")
boxplot(subset(dat_clust, x=="5")[2:18], horizontal=T, las=1, ylim=c(-4,4), main="Cluster 5")
boxplot(subset(dat_clust, x=="6")[2:18], horizontal=T, las=1, ylim=c(-4,4), main="Cluster 6")
boxplot(subset(dat_clust, x=="7")[2:18], horizontal=T, las=1, ylim=c(-4,4), main="Cluster 7")
boxplot(subset(dat_clust, x=="8")[2:18], horizontal=T, las=1, ylim=c(-4,4), main="Cluster 8")
boxplot(subset(dat_clust, x=="9")[2:18], horizontal=T, las=1, ylim=c(-4,4), main="Cluster 9")
boxplot(subset(dat_clust, x=="10")[2:18], horizontal=T, las=1, ylim=c(-4,4), main="Cluster 10")
par(mfrow=c(1,1))

#calculate cluster means for different variables
c1m <- data.frame(one=colMeans(subset(dat_clust, x=="1")[2:18]))
c2m <- data.frame(two=colMeans(subset(dat_clust, x=="2")[2:18]))
c3m <- data.frame(three=colMeans(subset(dat_clust, x=="3")[2:18]))
c4m <- data.frame(four=colMeans(subset(dat_clust, x=="4")[2:18]))
c5m <- data.frame(five=colMeans(subset(dat_clust, x=="5")[2:18]))
c6m <- data.frame(six=colMeans(subset(dat_clust, x=="6")[2:18]))
c7m <- data.frame(seven=colMeans(subset(dat_clust, x=="7")[2:18]))
c8m <- data.frame(eight=colMeans(subset(dat_clust, x=="8")[2:18]))
c9m <- data.frame(nine=colMeans(subset(dat_clust, x=="9")[2:18]))
c10m <- data.frame(ten=colMeans(subset(dat_clust, x=="10")[2:18]))

cluster_means <- cbind.data.frame(c1m,c2m,c3m,c4m,c5m,c6m,c7m,c8m, c9m, c10m)
cluster_means$variable <- row.names(cluster_means)
rownames(cluster_means) <- NULL

write.csv(cluster_means, file=here("results/cluster_means.csv"))

cluster_means

par(mfrow=c(2,5))
dotchart(cluster_means$one, labels=cluster_means$variable,xlim=c(-2,2), main='Cluster 1', pch=16)
abline(v=0, lwd=2)
dotchart(cluster_means$two, labels=cluster_means$variable,xlim=c(-2,2), main='Cluster 2', pch=16)
abline(v=0, lwd=2)
dotchart(cluster_means$three, labels=cluster_means$variable,xlim=c(-2,2), main='Cluster 3', pch=16)
abline(v=0, lwd=2)
dotchart(cluster_means$four, labels=cluster_means$variable,xlim=c(-2,2), main='Cluster 4', pch=16)
abline(v=0, lwd=2)
dotchart(cluster_means$five, labels=cluster_means$variable,xlim=c(-2,2), main='Cluster 5', pch=16)
abline(v=0, lwd=2)
dotchart(cluster_means$six, labels=cluster_means$variable,xlim=c(-2,2), main='Cluster 6', pch=16)
abline(v=0, lwd=2)
dotchart(cluster_means$seven, labels=cluster_means$variable,xlim=c(-2,2), main='Cluster 7', pch=16)
abline(v=0, lwd=2)
dotchart(cluster_means$eight, labels=cluster_means$variable,xlim=c(-2,2), main='Cluster 8', pch=16)
abline(v=0, lwd=2)
dotchart(cluster_means$nine, labels=cluster_means$variable,xlim=c(-2,2), main='Cluster 9', pch=16)
abline(v=0, lwd=2)
dotchart(cluster_means$ten, labels=cluster_means$variable,xlim=c(-2,2), main='Cluster 10', pch=16)
abline(v=0, lwd=2)
par(mfrow=c(1,1))


##########################
#Most sustainable cluster

#rotate columns and rows
cluster_means_t <- cluster_means[1:10]
row.names(cluster_means_t) <- cluster_means$variable
cluster_means_t <- data.frame(t(cluster_means_t))
cluster_means_t$cluster <- row.names(cluster_means_t)
row.names(cluster_means_t) <- NULL
cluster_means_t

#clusters scoring the best in the different metrics
best_air <- cluster_means_t[which.max(cluster_means_t$AQI_Good),]
best_edu <- cluster_means_t[which.max(cluster_means_t$Bachelor_Over_25),]
best_pov <- cluster_means_t[which.min(cluster_means_t$Per_Poverty),]
best_ineq <- cluster_means_t[which.min(cluster_means_t$Gini),]
best_hous <- cluster_means_t[which.min(cluster_means_t$Per_Sev_Hous),]
best_stream <- cluster_means_t[which.min(cluster_means_t$Xstreamlengthimpaired),]
best_land <- cluster_means_t[which.max(cluster_means_t$Per_Avg_Land_Cov),]
best_health <- cluster_means_t[which.min(cluster_means_t$poor_health_percent),]
best_water <- cluster_means_t[which.min(cluster_means_t$Z_Water_Index),]
low_index_black <- cluster_means_t[which.min(cluster_means_t$Index_Black),]
low_index_asian <- cluster_means_t[which.min(cluster_means_t$Index_Asian),]
low_index_latino <- cluster_means_t[which.min(cluster_means_t$Index_Latino),]
best_emissions <- cluster_means_t[which.min(cluster_means_t$GHG_Percap),]
best_employmet <- cluster_means_t[which.min(cluster_means_t$UNEMPLOY),]
best_food <- cluster_means_t[which.min(cluster_means_t$FOOD_INS),]
best_crime <- cluster_means_t[which.min(cluster_means_t$VIO_CRIME),]


most_sustainable_clusters <- data.frame(Metric=c(colnames(cluster_means_t[c(1:4,6:17)])),
                                        Best_Cluster=c(
                                        best_air$cluster,
                                        best_edu$cluster,
                                        best_pov$cluster,
                                        best_ineq$cluster,
                                        best_hous$cluster,
                                        best_stream$cluster,
                                        best_land$cluster,
                                        best_health$cluster,
                                        best_water$cluster,
                                        low_index_black$cluster,
                                        low_index_asian$cluster,
                                        low_index_latino$cluster,
                                        best_emissions$cluster,
                                        best_employmet$cluster,
                                        best_food$cluster,
                                        best_crime$cluster))

most_sustainable_clusters

#clusters scoring the worst in different metrics

worst_air <- cluster_means_t[which.min(cluster_means_t$AQI_Good),]
worst_edu <- cluster_means_t[which.min(cluster_means_t$Bachelor_Over_25),]
worst_pov <- cluster_means_t[which.max(cluster_means_t$Per_Poverty),]
worst_ineq <- cluster_means_t[which.max(cluster_means_t$Gini),]
worst_hous <- cluster_means_t[which.max(cluster_means_t$Per_Sev_Hous),]
worst_stream <- cluster_means_t[which.max(cluster_means_t$Xstreamlengthimpaired),]
worst_land <- cluster_means_t[which.min(cluster_means_t$Per_Avg_Land_Cov),]
worst_health <- cluster_means_t[which.max(cluster_means_t$poor_health_percent),]
worst_water <- cluster_means_t[which.max(cluster_means_t$Z_Water_Index),]
high_index_black <- cluster_means_t[which.max(cluster_means_t$Index_Black),]
high_index_asian <- cluster_means_t[which.max(cluster_means_t$Index_Asian),]
high_index_latino <- cluster_means_t[which.max(cluster_means_t$Index_Latino),]
worst_emissions <- cluster_means_t[which.max(cluster_means_t$GHG_Percap),]
worst_employmet <- cluster_means_t[which.max(cluster_means_t$UNEMPLOY),]
worst_food <- cluster_means_t[which.max(cluster_means_t$FOOD_INS),]
worst_crime <- cluster_means_t[which.max(cluster_means_t$VIO_CRIME),]

least_sustainable_clusters <- data.frame(Metric=c(colnames(cluster_means_t[c(1:4,6:17)])),
                                        Worst_Cluster=c(
                                          worst_air$cluster,
                                          worst_edu$cluster,
                                          worst_pov$cluster,
                                          worst_ineq$cluster,
                                          worst_hous$cluster,
                                          worst_stream$cluster,
                                          worst_land$cluster,
                                          worst_health$cluster,
                                          worst_water$cluster,
                                          high_index_black$cluster,
                                          high_index_asian$cluster,
                                          high_index_latino$cluster,
                                          worst_emissions$cluster,
                                          worst_employmet$cluster,
                                          worst_food$cluster,
                                          worst_crime$cluster))

best_and_worst <- data.frame(Metric=most_sustainable_clusters$Metric, Best=most_sustainable_clusters$Best_Cluster, Worst=least_sustainable_clusters$Worst_Cluster)
best_and_worst


#most sustainable MSAs accross all clusters

MSA_best_air <- dat_clust[which.max(dat_clust$AQI_Good),]
MSA_best_edu <- dat_clust[which.max(dat_clust$Bachelor_Over_25),]
MSA_best_pov <- dat_clust[which.min(dat_clust$Per_Poverty),]
MSA_best_ineq <- dat_clust[which.min(dat_clust$Gini),]
MSA_best_hous <- dat_clust[which.min(dat_clust$Per_Sev_Hous),]
MSA_best_stream <- dat_clust[which.min(dat_clust$Xstreamlengthimpaired),]
MSA_best_land <- dat_clust[which.max(dat_clust$Per_Avg_Land_Cov),]
MSA_best_health <- dat_clust[which.min(dat_clust$poor_health_percent),]
MSA_best_water <- dat_clust[which.min(dat_clust$Z_Water_Index),]
MSA_low_index_black <- dat_clust[which.min(dat_clust$Index_Black),]
MSA_low_index_asian <- dat_clust[which.min(dat_clust$Index_Asian),]
MSA_low_index_latino <- dat_clust[which.min(dat_clust$Index_Latino),]


#most sustainable MSAs in each cluster
cluster1 <- data.frame(subset(dat_clust, x=="1"))
cluster2 <- data.frame(subset(dat_clust, x=="2"))
cluster3 <- data.frame(subset(dat_clust, x=="3"))
cluster4 <- data.frame(subset(dat_clust, x=="4"))
cluster5 <- data.frame(subset(dat_clust, x=="5"))
cluster6 <- data.frame(subset(dat_clust, x=="6"))
cluster7 <- data.frame(subset(dat_clust, x=="7"))
cluster8 <- data.frame(subset(dat_clust, x=="8"))
cluster9 <- data.frame(subset(dat_clust, x=="9"))
cluster10 <- data.frame(subset(dat_clust, x=="10"))

# best scoring MSAs by cluster
# c1_best_air <- cluster1[which.max(cluster1$AQI_Good),]
# c1_best_edu <- cluster1[which.max(cluster1$Bachelor_Over_25),]
# c1_best_pov <- cluster1[which.min(cluster1$Per_Poverty),]
# c1_best_ineq <- cluster1[which.min(cluster1$Gini),]
# c1_best_hous <- cluster1[which.min(cluster1$Per_Sev_Hous),]
# c1_best_stream <- cluster1[which.min(cluster1$Xstreamlengthimpaired),]
# c1_best_land <- cluster1[which.max(cluster1$Per_Avg_Land_Cov),]
# c1_best_health <- cluster1[which.min(cluster1$poor_health_percent),]
# c1_best_water <- cluster1[which.min(cluster1$Z_Water_Index),]
# c1_low_index_black <- cluster1[which.min(cluster1$Index_Black),]
# c1_low_index_asian <- cluster1[which.min(cluster1$Index_Asian),]
# c1_low_index_latino <- cluster1[which.min(cluster1$Index_Latino),]

#MSAs most like their average values

#function to compute smallest combined absolute difference from mean
Average_MSA_inClust <- function(x) {
  output = data.frame(Name=x$ME)
  for (i in 2:ncol(x)) {
    output[[i]] <- abs(x[[i]] - mean(x[[i]]))
  }
  output$s_diff = rowSums(output[2:ncol(output)])
  cluster_average = output[which.min(output$s_diff),]
  return(cluster_average)
}

cluster1_average <- Average_MSA_inClust(cluster1)
cluster2_average <- Average_MSA_inClust(cluster2)
cluster3_average <- Average_MSA_inClust(cluster3)
cluster4_average <- Average_MSA_inClust(cluster4)
cluster5_average <- Average_MSA_inClust(cluster5)
cluster6_average <- Average_MSA_inClust(cluster6)
cluster7_average <- Average_MSA_inClust(cluster7)
cluster8_average <- Average_MSA_inClust(cluster8)
cluster9_average <- Average_MSA_inClust(cluster9)
cluster10_average <- Average_MSA_inClust(cluster10)

typical_members <- data.frame(Cluster=c(1:10), Name=c(cluster1_average$Name,
                              cluster2_average$Name,
                              cluster3_average$Name,
                              cluster4_average$Name,
                              cluster5_average$Name,
                              cluster6_average$Name,
                              cluster7_average$Name,
                              cluster8_average$Name,
                              cluster9_average$Name,
                              cluster10_average$Name))
typical_members




###################################################33
##############################
# adjust values for variables so that high=good and low=bad for all variables
##########################################################################

dat_clust_adj <- dat_clust
dat_clust_adj$Per_Poverty <- dat_clust$Per_Poverty * -1
dat_clust_adj$Gini <- dat_clust$Gini * -1
dat_clust_adj$Per_Sev_Hous <- dat_clust$Per_Sev_Hous * -1
dat_clust_adj$Xstreamlengthimpaired <- dat_clust$Xstreamlengthimpaired * -1
dat_clust_adj$poor_health_percent <- dat_clust$poor_health_percent * -1
dat_clust_adj$Z_Water_Index <- dat_clust$Z_Water_Index * -1
dat_clust_adj$Index_Black <- dat_clust$Index_Black * -1
dat_clust_adj$Index_Asian <- dat_clust$Index_Asian * -1
dat_clust_adj$Index_Latino <- dat_clust$Index_Latino * -1
dat_clust_adj$GHG_Percap <- dat_clust$GHG_Percap * -1
dat_clust_adj$UNEMPLOY <- dat_clust$UNEMPLOY * -1
dat_clust_adj$FOOD_INS <- dat_clust$FOOD_INS * -1
dat_clust_adj$VIO_CRIME <- dat_clust$VIO_CRIME * -1


#calculate which are the 'highest scoring' MSAs accross all clusters
dat_clust_adj_sums <- dat_clust_adj
dat_clust_adj_sums$Total <- rowSums(dat_clust_adj_sums[2:18])
#order by Total
dat_clust_adj_sums <- dat_clust_adj_sums[order(-dat_clust_adj_sums$Total),] 
best<-head(dat_clust_adj_sums, n=10)
best<-data.frame(name=best$ME,Total=best$Total)
best


cluster1a <- data.frame(subset(dat_clust_adj, x=="1"))
cluster2a <- data.frame(subset(dat_clust_adj, x=="2"))
cluster3a <- data.frame(subset(dat_clust_adj, x=="3"))
cluster4a <- data.frame(subset(dat_clust_adj, x=="4"))
cluster5a <- data.frame(subset(dat_clust_adj, x=="5"))
cluster6a <- data.frame(subset(dat_clust_adj, x=="6"))
cluster7a <- data.frame(subset(dat_clust_adj, x=="7"))
cluster8a <- data.frame(subset(dat_clust_adj, x=="8"))
cluster9a <- data.frame(subset(dat_clust_adj, x=="9"))
cluster10a <- data.frame(subset(dat_clust_adj, x=="10"))

#### "Highest" scoring MSAs by cluster

best_in_cluster <- function(x){
  cluster_best = x
  cluster_best$Total = rowSums(cluster_best[2:18])
  cluster_best = cluster_best[order(-cluster_best$Total),]
  return(head(cluster_best[c(1,20)],n=10))
}

best_in_cluster(cluster1a)
best_in_cluster(cluster2a)
best_in_cluster(cluster3a)
best_in_cluster(cluster4a)
best_in_cluster(cluster5a)
best_in_cluster(cluster6a)
best_in_cluster(cluster7a)
best_in_cluster(cluster8a)
best_in_cluster(cluster9a)
best_in_cluster(cluster10a)



#install PCAtools package
# library(devtools)
# devtools::install_github('kevinblighe/PCAtools')


#PCA cluster 1
c1_PCA <- pca(cluster1a[2:18],transposed = T)
screeplot(c1_PCA, title="Scree plot Cluster 1")
findElbowPoint(c1_PCA$variance)
biplot(c1_PCA, showLoadings = T, showLoadingsNames = T, hline=0,vline=0, title='Cluster 1')
plotloadings(c1_PCA, title="Cluster 1")

#PCA cluster 2
c2_PCA <- pca(cluster2a[2:18],transposed = T)
screeplot(c2_PCA)
findElbowPoint(c2_PCA$variance)
biplot(c2_PCA, showLoadings = T, showLoadingsNames = T, hline=0,vline=0, title='Cluster 2')
plotloadings(c2_PCA, title="Cluster 2")

#PCA cluster 3
c3_PCA <- pca(cluster3a[2:18],transposed = T)
screeplot(c3_PCA)
findElbowPoint(c3_PCA$variance)
biplot(c3_PCA, showLoadings = T, showLoadingsNames = T, hline=0,vline=0, title='Cluster 3')
plotloadings(c3_PCA, title="Cluster 3")

#PCA cluster 4
c4_PCA <- pca(cluster4a[2:18],transposed = T)
screeplot(c4_PCA)
findElbowPoint(c4_PCA$variance)
biplot(c4_PCA, showLoadings = T, showLoadingsNames = T, hline=0,vline=0, title='Cluster 4')
plotloadings(c4_PCA, title="Cluster 4")

#PCA cluster 5
c5_PCA <- pca(cluster5a[2:18],transposed = T)
screeplot(c5_PCA)
findElbowPoint(c5_PCA$variance)
biplot(c5_PCA, showLoadings = T, showLoadingsNames = T, hline=0,vline=0, title='Cluster 5')
plotloadings(c5_PCA, title="Cluster 5")

#PCA cluster 6
c6_PCA <- pca(cluster6a[2:18],transposed = T)
screeplot(c6_PCA)
findElbowPoint(c6_PCA$variance)
biplot(c6_PCA, showLoadings = T, showLoadingsNames = T, hline=0,vline=0, title='Cluster 6')
plotloadings(c6_PCA, title="Cluster 6")

#PCA cluster 7
c7_PCA <- pca(cluster7a[2:18],transposed = T)
screeplot(c7_PCA)
findElbowPoint(c7_PCA$variance)
biplot(c7_PCA, showLoadings = T, showLoadingsNames = T, hline=0,vline=0, title='Cluster 7')
plotloadings(c7_PCA, title="Cluster 7")

#PCA cluster 8
c8_PCA <- pca(cluster8a[2:18],transposed = T)
screeplot(c8_PCA)
findElbowPoint(c8_PCA$variance)
biplot(c8_PCA, showLoadings = T, showLoadingsNames = T, hline=0,vline=0, title='Cluster 8')
plotloadings(c8_PCA, title="Cluster 8")

#PCA cluster 9
c9_PCA <- pca(cluster9a[2:18],transposed = T)
screeplot(c9_PCA)
findElbowPoint(c9_PCA$variance)
biplot(c9_PCA, showLoadings = T, showLoadingsNames = T, hline=0,vline=0, title='Cluster 9')
plotloadings(c9_PCA, title="Cluster 9")

#PCA cluster 10
c10_PCA <- pca(cluster10a[2:18],transposed = T)
screeplot(c10_PCA)
findElbowPoint(c10_PCA$variance)
biplot(c10_PCA, showLoadings = T, showLoadingsNames = T, hline=0,vline=0, title='Cluster 10')
plotloadings(c10_PCA, title="Cluster 10")


#### PCA on full dataset

Tpca <- pca(dat_clust_adj[2:18],transposed = T)
screeplot(Tpca)
findElbowPoint(Tpca$variance)
PCAtools::biplot(Tpca, x="PC1",y="PC2")
plotloadings(Tpca)

Tpca <- princomp(dat_clust_adj[2:18])
stats::biplot(Tpca)



####################################
#dimension reduction based on cluster modeling
########################################
DR <- MclustDR(dat_mc10, lambda = 1) #setting lambda to 1 gives most separating directions
summary(DR)


plot(c(1:17),DR$evalues,ylab="eigenvalues", xlab="component", type="o", pch=16)

plot(DR, what='pairs',symbols=c("1","2","3","4","5","6","7","8","9","10"),
     colors=c("red","blue","green","goldenrod2","violet","brown","black","grey30","slateblue2","seagreen3"))

plot(DR, what='pairs',symbols=c("1","2","3","4","5","6","7","8","9","10"),
     colors=brewer.pal(10,"Spectral"))

#plot directions for variables
Directions <- data.frame(d1=DR$basis[,1],d2=DR$basis[,2],
                         d3=DR$basis[,3],d4=DR$basis[,4],
                         d5=DR$basis[,5],d6=DR$basis[,6],
                         d7=DR$basis[,7],d8=DR$basis[,8],
                         d9=DR$basis[,9],var=rownames(DR$basis))

par(mfrow=c(3,3))
dotchart(Directions$d1, labels=Directions$var, pch=16, main="Dir1", xlim=c(-0.6,0.6))
abline(v=0, lwd=2)
abline(v=c(0.1,-0.1),lty=2)
dotchart(Directions$d2, labels=Directions$var, pch=16, main="Dir2", xlim=c(-0.6,0.6))
abline(v=0, lwd=2)
abline(v=c(0.1,-0.1),lty=2)
dotchart(Directions$d3, labels=Directions$var, pch=16, main="Dir3", xlim=c(-0.6,0.6))
abline(v=0, lwd=2)
abline(v=c(0.1,-0.1),lty=2)
dotchart(Directions$d4, labels=Directions$var, pch=16, main="Dir4", xlim=c(-0.6,0.6))
abline(v=0, lwd=2)
abline(v=c(0.1,-0.1),lty=2)
dotchart(Directions$d5, labels=Directions$var, pch=16, main="Dir5", xlim=c(-0.6,0.6))
abline(v=0, lwd=2)
abline(v=c(0.1,-0.1),lty=2)
dotchart(Directions$d6, labels=Directions$var, pch=16, main="Dir6", xlim=c(-0.6,0.6))
abline(v=0, lwd=2)
abline(v=c(0.1,-0.1),lty=2)
dotchart(Directions$d7, labels=Directions$var, pch=16, main="Dir7", xlim=c(-0.6,0.6))
abline(v=0, lwd=2)
abline(v=c(0.1,-0.1),lty=2)
dotchart(Directions$d8, labels=Directions$var, pch=16, main="Dir8", xlim=c(-0.6,0.6))
abline(v=0, lwd=2)
abline(v=c(0.1,-0.1),lty=2)
dotchart(Directions$d9, labels=Directions$var, pch=16, main="Dir9", xlim=c(-0.6,0.6))
abline(v=0, lwd=2)
abline(v=c(0.1,-0.1),lty=2)
par(mfrow=c(1,1))


#######################
# Map clusters


# MSA shapefile from - https://www2.census.gov/geo/tiger/TIGER2015/CBSA/
msa_Boundary <-readOGR(here("data/MSA"),"tl_2015_us_cbsa") 
#merge them
merged <- merge(msa_Boundary,data,by.x="NAME",by.y="X")
#remove any cases MSAs with no cluster assignments
merged_clean <- merged[!is.na(merged$x),]
#convert x to factor
merged_clean$Cluster <- as.factor(merged_clean$x)

#use brewer colors
spplot(merged_clean, "x_f", col.regions=brewer.pal(10,'Spectral'))
#try different colors
mycolors <- c('blue3','sienna3','seagreen3', 'darkslateblue',
              'goldenrod2', 'slateblue2','deeppink2', 'chocolate4')
mycolors2 <- c("red","blue","green","goldenrod2","violet","brown","black","grey30","slateblue2","seagreen3")
spplot(merged_clean, "x_f", col.regions=mycolors2)

# png(here("results/clustermap.png"),width=12, height=6, units="in", res=300)
# spplot(merged_clean, "x_f", col.regions=brewer.pal(10,'Spectral'))
# dev.off()

# #USA shapefile
# USA <- readOGR(here("data/US_shapefile"),"cb_2018_us_state_20m")
# spplot(USA[7],add=T)

######################################
# plots with sf and tmap


cluster_sf <- st_as_sf(merged_clean)
cluster_sf
us_map <- tm_shape(us_states) + tm_borders()
cluster_map <- tm_shape(cluster_sf) + tm_fill(col="Cluster", palette="Spectral")
us_map + cluster_map
