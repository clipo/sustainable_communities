#Load Packages
library(here)
library(mclust)
library(raster)
library(rgdal)
library(sp)
library(RColorBrewer)
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("PCAtools")
library(PCAtools)
library(sf)
library(tmap)
library(terra)
library(rgeos)
library(spData)
library(lattice)
library(classInt)


###################################
##### IMPORT DATA
###################################

#import raw data
dat_raw <- read.csv(here("data/CommunityData-raw-2015-v2.csv"))
#select columns of interest, removed co2_hhs
dat  <- dat_raw[,c(4:12,14:21)]
#convert to dataframe
dat <- as.data.frame(unclass(dat))
#set rownames to city names
rownames(dat)=dat_raw$GEOID
#look at data
summary(dat)
head(dat)
# remove any records that have NAs
dat = na.omit(dat)
# # convert to z-scores
dat <- as.matrix(scale(dat, center = TRUE, scale = TRUE))


###################################
##### RUN PCA ON Z-SCORES
###################################

#PCA
dat_comps <- prcomp(dat, center = F, scale=F) #set to FALSE because done pre-PCA
summary(dat_comps)
s=summary(dat_comps)

#get eigenvalues
ev <- dat_comps$sdev^2

#plot loadings of pc1 and 2
plot(dat_comps$x[,1],dat_comps$x[,2], 
     xlab=paste("PCA 1 (", round(s$importance[2,1]*100, 1), "%)", sep = ""), 
     ylab=paste("PCA 2 (", round(s$importance[2,2]*100, 1), "%)", sep = ""), 
     pch=16, col="blue", cex=0.5)
abline(v=0, lwd=2, lty=2)
abline(h=0, lwd=2, lty=2)

#get loadings
l.x <- dat_comps$rotation[,1]*10
l.y <- dat_comps$rotation[,2]*10
arrows(x0=mean(dat_comps$x[,1]), x1=l.x, y0=mean(dat_comps$x[,2]), y1=l.y, col="black", length=0.15, lwd=1.5)
# Label position
l.pos <- l.y # Create a vector of y axis coordinates
lo <- which(l.y < 0) # Get the variables on the bottom half of the plot
hi <- which(l.y > 0) # Get variables on the top half
# Replace values in the vector
l.pos <- replace(l.pos, lo, "1")
l.pos <- replace(l.pos, hi, "3")
text(l.x, l.y, labels=row.names(dat_comps$rotation), col="black", pos=l.pos, cex=1.25)

#plot loadings of pc 3 and 4
plot(dat_comps$x[,3],dat_comps$x[,4], 
     xlab=paste("PCA 3 (", round(s$importance[2,3]*100, 1), "%)", sep = ""), 
     ylab=paste("PCA 4 (", round(s$importance[2,4]*100, 1), "%)", sep = ""), 
     pch=16, col="blue", cex=0.5)
abline(v=0, lwd=2, lty=2)
abline(h=0, lwd=2, lty=2)

#get loadings
l.x <- dat_comps$rotation[,3]*10
l.y <- dat_comps$rotation[,4]*10
arrows(x0=mean(dat_comps$x[,3]), x1=l.x, y0=mean(dat_comps$x[,4]), y1=l.y, col="black", length=0.15, lwd=1.5)
# Label position
l.pos <- l.y # Create a vector of y axis coordinates
lo <- which(l.y < 0) # Get the variables on the bottom half of the plot
hi <- which(l.y > 0) # Get variables on the top half
# Replace values in the vector
l.pos <- replace(l.pos, lo, "1")
l.pos <- replace(l.pos, hi, "3")
text(l.x, l.y, labels=row.names(dat_comps$rotation), col="black", pos=l.pos, cex=1.25)


###################################
##### EXTRACT PCS WITH EIGENVALUES > 1
###################################
dat_pcs <- dat_comps$x[,1:5]

###################################
##### GAUSSIAN MIXTURE MODELING
###################################

#run GMM on 1-20 clusters
dat_pcs_mc <- Mclust(dat_pcs, G=c(1:20))
summary(dat_pcs_mc)
#plot BIC scores
plot(dat_pcs_mc, what='BIC')
#zoom in
plot(dat_pcs_mc, what='BIC', ylim=c(-15250,-15150))
#model type VEI and VEE have similar BIC scores for 8-9 clusters
#to do additional model comparisons
m_VEI_8 <- Mclust(dat_pcs, G=8, modelNames="VEI")
m_VEE_8 <- Mclust(dat_pcs, G=8, modelNames="VEE")
m_VEI_9 <- Mclust(dat_pcs, G=9, modelNames="VEI")
m_VEE_9 <- Mclust(dat_pcs, G=9, modelNames="VEE")
#extract BIC scores
BICs<-c(m_VEI_8$BIC[1],m_VEE_8$BIC[1],m_VEI_9$BIC[1],m_VEE_9$BIC[1])
#calculate change in BIC score, since in this method the goal to to maximize BIC
#we calculate change from highest scoring model
delta_BIC <- max(BICs) - BICs
#calculate BIC weights
w_BIC <- round(exp(-0.5*delta_BIC)/sum(exp(-0.5*delta_BIC)), digits=3)
#make table of results
results_table <- cbind.data.frame(clusters=c("VEI_8","VEE_8","VEI_9","VEE_9"),BIC=BICs,delta_BIC,weight=w_BIC)
#order by delta
results_table <- results_table[order(delta_BIC),] 
rownames(results_table)<-NULL
results_table

#extract classifications, e.g., which city belongs to which cluster, export to csv and reload
write.csv(m_VEE_8$classification,file=here("results/cluster_assignment_pca.csv"))
data<- read.csv(here('results/cluster_assignment_pca.csv'))

###################################
##### MAP CLUSTERS
###################################

# MSA shapefile from - https://www2.census.gov/geo/tiger/TIGER2015/CBSA/
msa_Boundary <-readOGR(here("data/MSA"),"tl_2015_us_cbsa") 
#merge them
merged <- merge(msa_Boundary,data,by.x="GEOID",by.y="X")
#remove any cases MSAs with no cluster assignments
merged_clean <- merged[!is.na(merged$x),]
#convert x to factor
merged_clean$Cluster <- as.factor(merged_clean$x)

cluster_sf <- st_as_sf(merged_clean)
cluster_sf
us_map <- tm_shape(us_states) + tm_borders()
cluster_map <- tm_shape(cluster_sf) + tm_fill(col="Cluster", palette="Set1")
us_map + cluster_map

###################################
##### CLASSIFICATION PROBABILITIES
###################################

#print table showing probability that MSA belongs to class
class_prob <- round(m_VEE_8$z, digits=3)
class_prob

###################################
##### EXAMINE which clusters score highly on different metrics
###################################

#new data frame
dat_clust <- as.data.frame(dat)
#add names
dat_clust$ME <- rownames(dat)
rownames(dat_clust) <- NULL
#merge in cluster assignments
dat_clust <- merge(dat_clust,data,by.x="ME",by.y="X")
head(dat_clust)


#BOXPLOTS OF CLUSTERS

par(mfrow=c(4,2))
par(mar=c(4,4,4,4))
boxplot(subset(dat_clust, x=="1")[2:18], horizontal=T, las=1, ylim=c(-4,4), main="Cluster 1")
abline(v=0, col='red', lwd=2)
boxplot(subset(dat_clust, x=="2")[2:18], horizontal=T, las=1, ylim=c(-4,4), main="Cluster 2")
abline(v=0, col='red', lwd=2)
boxplot(subset(dat_clust, x=="3")[2:18], horizontal=T, las=1, ylim=c(-4,4), main="Cluster 3")
abline(v=0, col='red', lwd=2)
boxplot(subset(dat_clust, x=="4")[2:18], horizontal=T, las=1, ylim=c(-4,4), main="Cluster 4")
abline(v=0, col='red', lwd=2)
boxplot(subset(dat_clust, x=="5")[2:18], horizontal=T, las=1, ylim=c(-4,4), main="Cluster 5")
abline(v=0, col='red', lwd=2)
boxplot(subset(dat_clust, x=="6")[2:18], horizontal=T, las=1, ylim=c(-4,4), main="Cluster 6")
abline(v=0, col='red', lwd=2)
boxplot(subset(dat_clust, x=="7")[2:18], horizontal=T, las=1, ylim=c(-4,4), main="Cluster 7")
abline(v=0, col='red', lwd=2)
boxplot(subset(dat_clust, x=="8")[2:18], horizontal=T, las=1, ylim=c(-4,4), main="Cluster 8")
abline(v=0, col='red', lwd=2)
par(mfrow=c(1,1))

#CLUSTER MEANS
c1m <- data.frame(one=colMeans(subset(dat_clust, x=="1")[2:18]))
c2m <- data.frame(two=colMeans(subset(dat_clust, x=="2")[2:18]))
c3m <- data.frame(three=colMeans(subset(dat_clust, x=="3")[2:18]))
c4m <- data.frame(four=colMeans(subset(dat_clust, x=="4")[2:18]))
c5m <- data.frame(five=colMeans(subset(dat_clust, x=="5")[2:18]))
c6m <- data.frame(six=colMeans(subset(dat_clust, x=="6")[2:18]))
c7m <- data.frame(seven=colMeans(subset(dat_clust, x=="7")[2:18]))
c8m <- data.frame(eight=colMeans(subset(dat_clust, x=="8")[2:18]))
cluster_means <- cbind.data.frame(c1m,c2m,c3m,c4m,c5m,c6m,c7m,c8m)
cluster_means$variable <- row.names(cluster_means)
rownames(cluster_means) <- NULL

#dotchart of cluster means
par(mfrow=c(2,4))
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
par(mfrow=c(1,1))

##########################
#Most sustainable cluster
#################################

#rotate columns and rows
cluster_means_t <- cluster_means[1:8]
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

#most sustainable MSAs in each cluster
cluster1 <- data.frame(subset(dat_clust, x=="1"))
cluster2 <- data.frame(subset(dat_clust, x=="2"))
cluster3 <- data.frame(subset(dat_clust, x=="3"))
cluster4 <- data.frame(subset(dat_clust, x=="4"))
cluster5 <- data.frame(subset(dat_clust, x=="5"))
cluster6 <- data.frame(subset(dat_clust, x=="6"))
cluster7 <- data.frame(subset(dat_clust, x=="7"))
cluster8 <- data.frame(subset(dat_clust, x=="8"))

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

typical_members <- data.frame(Cluster=c(1:8), Name=c(cluster1_average$Name,
                                                      cluster2_average$Name,
                                                      cluster3_average$Name,
                                                      cluster4_average$Name,
                                                      cluster5_average$Name,
                                                      cluster6_average$Name,
                                                      cluster7_average$Name,
                                                      cluster8_average$Name))
typical_members





####################################
#dimension reduction based on cluster modeling !!!! doesn't seem useful for pca based clustering
########################################
# DR <- MclustDR(m_VEE_8, lambda = 1) #setting lambda to 1 gives most separating directions
# summary(DR)
# 
# 
# plot(c(1:5),DR$evalues,ylab="eigenvalues", xlab="component", type="o", pch=16)
# 
# plot(DR, what='pairs',symbols=c("1","2","3","4","5","6","7","8"),
#      colors=c("red","blue","green","goldenrod2","violet","brown","black","grey30"))
# 
# plot(DR, what='pairs',symbols=c("1","2","3","4","5","6","7","8"),
#      colors=brewer.pal(8,"Spectral"))
# 
# #plot directions for variables
# Directions <- data.frame(d1=DR$basis[,1],d2=DR$basis[,2],
#                          d3=DR$basis[,3],d4=DR$basis[,4],
#                          d5=DR$basis[,5],var=rownames(DR$basis))
# 
# par(mfrow=c(2,3))
# dotchart(Directions$d1, labels=Directions$var, pch=16, main="Dir1", xlim=c(-0.6,0.6))
# abline(v=0, lwd=2)
# abline(v=c(0.1,-0.1),lty=2)
# dotchart(Directions$d2, labels=Directions$var, pch=16, main="Dir2", xlim=c(-0.6,0.6))
# abline(v=0, lwd=2)
# abline(v=c(0.1,-0.1),lty=2)
# dotchart(Directions$d3, labels=Directions$var, pch=16, main="Dir3", xlim=c(-0.6,0.6))
# abline(v=0, lwd=2)
# abline(v=c(0.1,-0.1),lty=2)
# dotchart(Directions$d4, labels=Directions$var, pch=16, main="Dir4", xlim=c(-0.6,0.6))
# abline(v=0, lwd=2)
# abline(v=c(0.1,-0.1),lty=2)
# dotchart(Directions$d5, labels=Directions$var, pch=16, main="Dir5", xlim=c(-0.6,0.6))
# abline(v=0, lwd=2)
# abline(v=c(0.1,-0.1),lty=2)
# par(mfrow=c(1,1))