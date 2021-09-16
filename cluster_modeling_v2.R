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
                                        Cluster=c(
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

#most sustainable MSAs

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
c1_diffs = data.frame(Name=cluster1$ME,
                      air=abs(cluster1$AQI_Good - mean(cluster1$AQI_Good)),
                      edu=abs(cluster1$Bachelor_Over_25 - mean(cluster1$Bachelor_Over_25)),
                      pov=abs(cluster1$Per_Poverty - mean(cluster1$Per_Poverty)),
                      ineq=abs(cluster1$Gini - mean(cluster1$Gini)),
                      migration=abs(cluster1$non_migration - mean(cluster1$non_migration)),
                      hous=abs(cluster1$Per_Sev_Hous - mean(cluster1$Per_Sev_Hous)),
                      stream=abs(cluster1$Xstreamlengthimpaired - mean(cluster1$Xstreamlengthimpaired)),
                      land=abs(cluster1$Per_Avg_Land_Cov - mean(cluster1$Per_Avg_Land_Cov)),
                      health=abs(cluster1$poor_health_percent - mean(cluster1$poor_health_percent)),
                      water=abs(cluster1$Z_Water_Index - mean(cluster1$Z_Water_Index)),
                      i_black=abs(cluster1$Index_Black - mean(cluster1$Index_Black)),
                      i_asian=abs(cluster1$Index_Asian - mean(cluster1$Index_Asian)),
                      i_latino=abs(cluster1$Index_Latino - mean(cluster1$Index_Latino)),
                      GHG=abs(cluster1$GHG_Percap - mean(cluster1$GHG_Percap)),
                      unemploy=abs(cluster1$UNEMPLOY - mean(cluster1$UNEMPLOY)),
                      food=abs(cluster1$FOOD_INS - mean(cluster1$FOOD_INS)),
                      crime=abs(cluster1$VIO_CRIME - mean(cluster1$VIO_CRIME)))
c1_diffs$s_diff <- rowSums(c1_diffs[2:ncol(c1_diffs)])
cluster1_average <- c1_diffs[which.min(c1_diffs$s_diff),]
cluster1_average

c2_diffs = data.frame(Name=cluster2$ME,
                      air=abs(cluster2$AQI_Good - mean(cluster2$AQI_Good)),
                      edu=abs(cluster2$Bachelor_Over_25 - mean(cluster2$Bachelor_Over_25)),
                      pov=abs(cluster2$Per_Poverty - mean(cluster2$Per_Poverty)),
                      ineq=abs(cluster2$Gini - mean(cluster2$Gini)),
                      migration=abs(cluster2$non_migration - mean(cluster2$non_migration)),
                      hous=abs(cluster2$Per_Sev_Hous - mean(cluster2$Per_Sev_Hous)),
                      stream=abs(cluster2$Xstreamlengthimpaired - mean(cluster2$Xstreamlengthimpaired)),
                      land=abs(cluster2$Per_Avg_Land_Cov - mean(cluster2$Per_Avg_Land_Cov)),
                      health=abs(cluster2$poor_health_percent - mean(cluster2$poor_health_percent)),
                      water=abs(cluster2$Z_Water_Index - mean(cluster2$Z_Water_Index)),
                      i_black=abs(cluster2$Index_Black - mean(cluster2$Index_Black)),
                      i_asian=abs(cluster2$Index_Asian - mean(cluster2$Index_Asian)),
                      i_latino=abs(cluster2$Index_Latino - mean(cluster2$Index_Latino)),
                      GHG=abs(cluster2$GHG_Percap - mean(cluster2$GHG_Percap)),
                      unemploy=abs(cluster2$UNEMPLOY - mean(cluster2$UNEMPLOY)),
                      food=abs(cluster2$FOOD_INS - mean(cluster2$FOOD_INS)),
                      crime=abs(cluster2$VIO_CRIME - mean(cluster2$VIO_CRIME)))
c2_diffs$s_diff <- rowSums(c2_diffs[2:ncol(c2_diffs)])
cluster2_average <- c2_diffs[which.min(c2_diffs$s_diff),]
cluster2_average

c3_diffs = data.frame(Name=cluster3$ME,
                      air=abs(cluster3$AQI_Good - mean(cluster3$AQI_Good)),
                      edu=abs(cluster3$Bachelor_Over_25 - mean(cluster3$Bachelor_Over_25)),
                      pov=abs(cluster3$Per_Poverty - mean(cluster3$Per_Poverty)),
                      ineq=abs(cluster3$Gini - mean(cluster3$Gini)),
                      migration=abs(cluster3$non_migration - mean(cluster3$non_migration)),
                      hous=abs(cluster3$Per_Sev_Hous - mean(cluster3$Per_Sev_Hous)),
                      stream=abs(cluster3$Xstreamlengthimpaired - mean(cluster3$Xstreamlengthimpaired)),
                      land=abs(cluster3$Per_Avg_Land_Cov - mean(cluster3$Per_Avg_Land_Cov)),
                      health=abs(cluster3$poor_health_percent - mean(cluster3$poor_health_percent)),
                      water=abs(cluster3$Z_Water_Index - mean(cluster3$Z_Water_Index)),
                      i_black=abs(cluster3$Index_Black - mean(cluster3$Index_Black)),
                      i_asian=abs(cluster3$Index_Asian - mean(cluster3$Index_Asian)),
                      i_latino=abs(cluster3$Index_Latino - mean(cluster3$Index_Latino)),
                      GHG=abs(cluster3$GHG_Percap - mean(cluster3$GHG_Percap)),
                      unemploy=abs(cluster3$UNEMPLOY - mean(cluster3$UNEMPLOY)),
                      food=abs(cluster3$FOOD_INS - mean(cluster3$FOOD_INS)),
                      crime=abs(cluster3$VIO_CRIME - mean(cluster3$VIO_CRIME)))
c3_diffs$s_diff <- rowSums(c3_diffs[2:ncol(c3_diffs)])
cluster3_average <- c3_diffs[which.min(c3_diffs$s_diff),]
cluster3_average

c4_diffs = data.frame(Name=cluster4$ME,
                      air=abs(cluster4$AQI_Good - mean(cluster4$AQI_Good)),
                      edu=abs(cluster4$Bachelor_Over_25 - mean(cluster4$Bachelor_Over_25)),
                      pov=abs(cluster4$Per_Poverty - mean(cluster4$Per_Poverty)),
                      ineq=abs(cluster4$Gini - mean(cluster4$Gini)),
                      migration=abs(cluster4$non_migration - mean(cluster4$non_migration)),
                      hous=abs(cluster4$Per_Sev_Hous - mean(cluster4$Per_Sev_Hous)),
                      stream=abs(cluster4$Xstreamlengthimpaired - mean(cluster4$Xstreamlengthimpaired)),
                      land=abs(cluster4$Per_Avg_Land_Cov - mean(cluster4$Per_Avg_Land_Cov)),
                      health=abs(cluster4$poor_health_percent - mean(cluster4$poor_health_percent)),
                      water=abs(cluster4$Z_Water_Index - mean(cluster4$Z_Water_Index)),
                      i_black=abs(cluster4$Index_Black - mean(cluster4$Index_Black)),
                      i_asian=abs(cluster4$Index_Asian - mean(cluster4$Index_Asian)),
                      i_latino=abs(cluster4$Index_Latino - mean(cluster4$Index_Latino)),
                      GHG=abs(cluster4$GHG_Percap - mean(cluster4$GHG_Percap)),
                      unemploy=abs(cluster4$UNEMPLOY - mean(cluster4$UNEMPLOY)),
                      food=abs(cluster4$FOOD_INS - mean(cluster4$FOOD_INS)),
                      crime=abs(cluster4$VIO_CRIME - mean(cluster4$VIO_CRIME)))
c4_diffs$s_diff <- rowSums(c4_diffs[2:ncol(c4_diffs)])
cluster4_average <- c4_diffs[which.min(c4_diffs$s_diff),]
cluster4_average

c5_diffs = data.frame(Name=cluster5$ME,
                      air=abs(cluster5$AQI_Good - mean(cluster5$AQI_Good)),
                      edu=abs(cluster5$Bachelor_Over_25 - mean(cluster5$Bachelor_Over_25)),
                      pov=abs(cluster5$Per_Poverty - mean(cluster5$Per_Poverty)),
                      ineq=abs(cluster5$Gini - mean(cluster5$Gini)),
                      migration=abs(cluster5$non_migration - mean(cluster5$non_migration)),
                      hous=abs(cluster5$Per_Sev_Hous - mean(cluster5$Per_Sev_Hous)),
                      stream=abs(cluster5$Xstreamlengthimpaired - mean(cluster5$Xstreamlengthimpaired)),
                      land=abs(cluster5$Per_Avg_Land_Cov - mean(cluster5$Per_Avg_Land_Cov)),
                      health=abs(cluster5$poor_health_percent - mean(cluster5$poor_health_percent)),
                      water=abs(cluster5$Z_Water_Index - mean(cluster5$Z_Water_Index)),
                      i_black=abs(cluster5$Index_Black - mean(cluster5$Index_Black)),
                      i_asian=abs(cluster5$Index_Asian - mean(cluster5$Index_Asian)),
                      i_latino=abs(cluster5$Index_Latino - mean(cluster5$Index_Latino)),
                      GHG=abs(cluster5$GHG_Percap - mean(cluster5$GHG_Percap)),
                      unemploy=abs(cluster5$UNEMPLOY - mean(cluster5$UNEMPLOY)),
                      food=abs(cluster5$FOOD_INS - mean(cluster5$FOOD_INS)),
                      crime=abs(cluster5$VIO_CRIME - mean(cluster5$VIO_CRIME)))
c5_diffs$s_diff <- rowSums(c5_diffs[2:ncol(c5_diffs)])
cluster5_average <- c5_diffs[which.min(c5_diffs$s_diff),]
cluster5_average

c6_diffs = data.frame(Name=cluster6$ME,
                      air=abs(cluster6$AQI_Good - mean(cluster6$AQI_Good)),
                      edu=abs(cluster6$Bachelor_Over_25 - mean(cluster6$Bachelor_Over_25)),
                      pov=abs(cluster6$Per_Poverty - mean(cluster6$Per_Poverty)),
                      ineq=abs(cluster6$Gini - mean(cluster6$Gini)),
                      migration=abs(cluster6$non_migration - mean(cluster6$non_migration)),
                      hous=abs(cluster6$Per_Sev_Hous - mean(cluster6$Per_Sev_Hous)),
                      stream=abs(cluster6$Xstreamlengthimpaired - mean(cluster6$Xstreamlengthimpaired)),
                      land=abs(cluster6$Per_Avg_Land_Cov - mean(cluster6$Per_Avg_Land_Cov)),
                      health=abs(cluster6$poor_health_percent - mean(cluster6$poor_health_percent)),
                      water=abs(cluster6$Z_Water_Index - mean(cluster6$Z_Water_Index)),
                      i_black=abs(cluster6$Index_Black - mean(cluster6$Index_Black)),
                      i_asian=abs(cluster6$Index_Asian - mean(cluster6$Index_Asian)),
                      i_latino=abs(cluster6$Index_Latino - mean(cluster6$Index_Latino)),
                      GHG=abs(cluster6$GHG_Percap - mean(cluster6$GHG_Percap)),
                      unemploy=abs(cluster6$UNEMPLOY - mean(cluster6$UNEMPLOY)),
                      food=abs(cluster6$FOOD_INS - mean(cluster6$FOOD_INS)),
                      crime=abs(cluster6$VIO_CRIME - mean(cluster6$VIO_CRIME)))
c6_diffs$s_diff <- rowSums(c6_diffs[2:ncol(c6_diffs)])
cluster6_average <- c6_diffs[which.min(c6_diffs$s_diff),]
cluster6_average

c7_diffs = data.frame(Name=cluster7$ME,
                      air=abs(cluster7$AQI_Good - mean(cluster7$AQI_Good)),
                      edu=abs(cluster7$Bachelor_Over_25 - mean(cluster7$Bachelor_Over_25)),
                      pov=abs(cluster7$Per_Poverty - mean(cluster7$Per_Poverty)),
                      ineq=abs(cluster7$Gini - mean(cluster7$Gini)),
                      migration=abs(cluster7$non_migration - mean(cluster7$non_migration)),
                      hous=abs(cluster7$Per_Sev_Hous - mean(cluster7$Per_Sev_Hous)),
                      stream=abs(cluster7$Xstreamlengthimpaired - mean(cluster7$Xstreamlengthimpaired)),
                      land=abs(cluster7$Per_Avg_Land_Cov - mean(cluster7$Per_Avg_Land_Cov)),
                      health=abs(cluster7$poor_health_percent - mean(cluster7$poor_health_percent)),
                      water=abs(cluster7$Z_Water_Index - mean(cluster7$Z_Water_Index)),
                      i_black=abs(cluster7$Index_Black - mean(cluster7$Index_Black)),
                      i_asian=abs(cluster7$Index_Asian - mean(cluster7$Index_Asian)),
                      i_latino=abs(cluster7$Index_Latino - mean(cluster7$Index_Latino)),
                      GHG=abs(cluster7$GHG_Percap - mean(cluster7$GHG_Percap)),
                      unemploy=abs(cluster7$UNEMPLOY - mean(cluster7$UNEMPLOY)),
                      food=abs(cluster7$FOOD_INS - mean(cluster7$FOOD_INS)),
                      crime=abs(cluster7$VIO_CRIME - mean(cluster7$VIO_CRIME)))
c7_diffs$s_diff <- rowSums(c7_diffs[2:ncol(c7_diffs)])
cluster7_average <- c7_diffs[which.min(c7_diffs$s_diff),]
cluster7_average

c8_diffs = data.frame(Name=cluster8$ME,
                      air=abs(cluster8$AQI_Good - mean(cluster8$AQI_Good)),
                      edu=abs(cluster8$Bachelor_Over_25 - mean(cluster8$Bachelor_Over_25)),
                      pov=abs(cluster8$Per_Poverty - mean(cluster8$Per_Poverty)),
                      ineq=abs(cluster8$Gini - mean(cluster8$Gini)),
                      migration=abs(cluster8$non_migration - mean(cluster8$non_migration)),
                      hous=abs(cluster8$Per_Sev_Hous - mean(cluster8$Per_Sev_Hous)),
                      stream=abs(cluster8$Xstreamlengthimpaired - mean(cluster8$Xstreamlengthimpaired)),
                      land=abs(cluster8$Per_Avg_Land_Cov - mean(cluster8$Per_Avg_Land_Cov)),
                      health=abs(cluster8$poor_health_percent - mean(cluster8$poor_health_percent)),
                      water=abs(cluster8$Z_Water_Index - mean(cluster8$Z_Water_Index)),
                      i_black=abs(cluster8$Index_Black - mean(cluster8$Index_Black)),
                      i_asian=abs(cluster8$Index_Asian - mean(cluster8$Index_Asian)),
                      i_latino=abs(cluster8$Index_Latino - mean(cluster8$Index_Latino)),
                      GHG=abs(cluster8$GHG_Percap - mean(cluster8$GHG_Percap)),
                      unemploy=abs(cluster8$UNEMPLOY - mean(cluster8$UNEMPLOY)),
                      food=abs(cluster8$FOOD_INS - mean(cluster8$FOOD_INS)),
                      crime=abs(cluster8$VIO_CRIME - mean(cluster8$VIO_CRIME)))
c8_diffs$s_diff <- rowSums(c8_diffs[2:ncol(c8_diffs)])
cluster8_average <- c8_diffs[which.min(c8_diffs$s_diff),]
cluster8_average

c9_diffs = data.frame(Name=cluster9$ME,
                      air=abs(cluster9$AQI_Good - mean(cluster9$AQI_Good)),
                      edu=abs(cluster9$Bachelor_Over_25 - mean(cluster9$Bachelor_Over_25)),
                      pov=abs(cluster9$Per_Poverty - mean(cluster9$Per_Poverty)),
                      ineq=abs(cluster9$Gini - mean(cluster9$Gini)),
                      migration=abs(cluster9$non_migration - mean(cluster9$non_migration)),
                      hous=abs(cluster9$Per_Sev_Hous - mean(cluster9$Per_Sev_Hous)),
                      stream=abs(cluster9$Xstreamlengthimpaired - mean(cluster9$Xstreamlengthimpaired)),
                      land=abs(cluster9$Per_Avg_Land_Cov - mean(cluster9$Per_Avg_Land_Cov)),
                      health=abs(cluster9$poor_health_percent - mean(cluster9$poor_health_percent)),
                      water=abs(cluster9$Z_Water_Index - mean(cluster9$Z_Water_Index)),
                      i_black=abs(cluster9$Index_Black - mean(cluster9$Index_Black)),
                      i_asian=abs(cluster9$Index_Asian - mean(cluster9$Index_Asian)),
                      i_latino=abs(cluster9$Index_Latino - mean(cluster9$Index_Latino)),
                      GHG=abs(cluster9$GHG_Percap - mean(cluster9$GHG_Percap)),
                      unemploy=abs(cluster9$UNEMPLOY - mean(cluster9$UNEMPLOY)),
                      food=abs(cluster9$FOOD_INS - mean(cluster9$FOOD_INS)),
                      crime=abs(cluster9$VIO_CRIME - mean(cluster9$VIO_CRIME)))
c9_diffs$s_diff <- rowSums(c9_diffs[2:ncol(c9_diffs)])
cluster9_average <- c9_diffs[which.min(c9_diffs$s_diff),]
cluster9_average

c10_diffs = data.frame(Name=cluster10$ME,
                       air=abs(cluster10$AQI_Good - mean(cluster10$AQI_Good)),
                       edu=abs(cluster10$Bachelor_Over_25 - mean(cluster10$Bachelor_Over_25)),
                       pov=abs(cluster10$Per_Poverty - mean(cluster10$Per_Poverty)),
                       ineq=abs(cluster10$Gini - mean(cluster10$Gini)),
                       migration=abs(cluster10$non_migration - mean(cluster10$non_migration)),
                       hous=abs(cluster10$Per_Sev_Hous - mean(cluster10$Per_Sev_Hous)),
                       stream=abs(cluster10$Xstreamlengthimpaired - mean(cluster10$Xstreamlengthimpaired)),
                       land=abs(cluster10$Per_Avg_Land_Cov - mean(cluster10$Per_Avg_Land_Cov)),
                       health=abs(cluster10$poor_health_percent - mean(cluster10$poor_health_percent)),
                       water=abs(cluster10$Z_Water_Index - mean(cluster10$Z_Water_Index)),
                       i_black=abs(cluster10$Index_Black - mean(cluster10$Index_Black)),
                       i_asian=abs(cluster10$Index_Asian - mean(cluster10$Index_Asian)),
                       i_latino=abs(cluster10$Index_Latino - mean(cluster10$Index_Latino)),
                       GHG=abs(cluster10$GHG_Percap - mean(cluster10$GHG_Percap)),
                       unemploy=abs(cluster10$UNEMPLOY - mean(cluster10$UNEMPLOY)),
                       food=abs(cluster10$FOOD_INS - mean(cluster10$FOOD_INS)),
                       crime=abs(cluster10$VIO_CRIME - mean(cluster10$VIO_CRIME)))
c10_diffs$s_diff <- rowSums(c10_diffs[2:ncol(c10_diffs)])
cluster10_average <- c10_diffs[which.min(c10_diffs$s_diff),]
cluster10_average

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

#calculate which are the 'highest scoring' MSAs
dat_clust_adj_sums <- dat_clust_adj
dat_clust_adj_sums$Total <- rowSums(dat_clust_adj_sums[2:18])
#order by Total
dat_clust_adj_sums <- dat_clust_adj_sums[order(-dat_clust_adj_sums$Total),] 
best<-head(dat_clust_adj_sums, n=10)
best<-data.frame(name=best$ME,Total=best$Total)
best


cluster1 <- data.frame(subset(dat_clust_adj, x=="1"))
cluster2 <- data.frame(subset(dat_clust_adj, x=="2"))
cluster3 <- data.frame(subset(dat_clust_adj, x=="3"))
cluster4 <- data.frame(subset(dat_clust_adj, x=="4"))
cluster5 <- data.frame(subset(dat_clust_adj, x=="5"))
cluster6 <- data.frame(subset(dat_clust_adj, x=="6"))
cluster7 <- data.frame(subset(dat_clust_adj, x=="7"))
cluster8 <- data.frame(subset(dat_clust_adj, x=="8"))
cluster9 <- data.frame(subset(dat_clust_adj, x=="9"))
cluster10 <- data.frame(subset(dat_clust_adj, x=="10"))

#### Higest scoring MSAs by cluster

cluster1_best <- cluster1
cluster1_best$Total <- rowSums(cluster1_best[2:18])
#order by total
cluster1_best <- cluster1_best[order(-cluster1_best$Total),] 
head(cluster1_best[c(1,20)], n=10)

cluster2_best <- cluster2
cluster2_best$Total <- rowSums(cluster2_best[2:18])
#order by total
cluster2_best <- cluster2_best[order(-cluster2_best$Total),] 
head(cluster2_best[c(1,20)], n=10)




#install PCAtools package
# library(devtools)
# devtools::install_github('kevinblighe/PCAtools')


#PCA cluster 1
c1_PCA <- pca(cluster1[2:18],transposed = T)
screeplot(c1_PCA, title="Scree plot Cluster 1")
findElbowPoint(c1_PCA$variance)
biplot(c1_PCA, showLoadings = T, showLoadingsNames = T, hline=0,vline=0, title='Cluster 1')
plotloadings(c1_PCA, title="Cluster 1")

#PCA cluster 2
c2_PCA <- pca(cluster2[2:18],transposed = T)
screeplot(c2_PCA)
findElbowPoint(c2_PCA$variance)
biplot(c2_PCA, showLoadings = T, showLoadingsNames = T, hline=0,vline=0, title='Cluster 2')
plotloadings(c2_PCA, title="Cluster 2")

#PCA cluster 3
c3_PCA <- pca(cluster3[2:18],transposed = T)
screeplot(c3_PCA)
findElbowPoint(c3_PCA$variance)
biplot(c3_PCA, showLoadings = T, showLoadingsNames = T, hline=0,vline=0, title='Cluster 3')
plotloadings(c3_PCA, title="Cluster 3")

#PCA cluster 4
c4_PCA <- pca(cluster4[2:18],transposed = T)
screeplot(c4_PCA)
findElbowPoint(c4_PCA$variance)
biplot(c4_PCA, showLoadings = T, showLoadingsNames = T, hline=0,vline=0, title='Cluster 4')
plotloadings(c4_PCA, title="Cluster 4")

#PCA cluster 5
c5_PCA <- pca(cluster5[2:18],transposed = T)
screeplot(c5_PCA)
findElbowPoint(c5_PCA$variance)
biplot(c5_PCA, showLoadings = T, showLoadingsNames = T, hline=0,vline=0, title='Cluster 5')
plotloadings(c5_PCA, title="Cluster 5")

#PCA cluster 6
c6_PCA <- pca(cluster6[2:18],transposed = T)
screeplot(c6_PCA)
findElbowPoint(c6_PCA$variance)
biplot(c6_PCA, showLoadings = T, showLoadingsNames = T, hline=0,vline=0, title='Cluster 6')
plotloadings(c6_PCA, title="Cluster 6")

#PCA cluster 7
c7_PCA <- pca(cluster7[2:18],transposed = T)
screeplot(c7_PCA)
findElbowPoint(c7_PCA$variance)
biplot(c7_PCA, showLoadings = T, showLoadingsNames = T, hline=0,vline=0, title='Cluster 7')
plotloadings(c7_PCA, title="Cluster 7")

#PCA cluster 8
c8_PCA <- pca(cluster8[2:18],transposed = T)
screeplot(c8_PCA)
findElbowPoint(c8_PCA$variance)
biplot(c8_PCA, showLoadings = T, showLoadingsNames = T, hline=0,vline=0, title='Cluster 8')
plotloadings(c8_PCA, title="Cluster 8")

#PCA cluster 9
c9_PCA <- pca(cluster9[2:18],transposed = T)
screeplot(c9_PCA)
findElbowPoint(c9_PCA$variance)
biplot(c9_PCA, showLoadings = T, showLoadingsNames = T, hline=0,vline=0, title='Cluster 9')
plotloadings(c9_PCA, title="Cluster 9")

#PCA cluster 10
c10_PCA <- pca(cluster10[2:18],transposed = T)
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
