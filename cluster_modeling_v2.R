#Load Packages
library(here)
library(mclust)
library(raster)
library(rgdal)
library(sp)
library(RColorBrewer)

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
boxplot(subset(dat_clust, x=="1")[2:17], horizontal=T, las=1, ylim=c(-4,4), main="Cluster 1")
boxplot(subset(dat_clust, x=="2")[2:17], horizontal=T, las=1, ylim=c(-4,4), main="Cluster 2")
boxplot(subset(dat_clust, x=="3")[2:17], horizontal=T, las=1, ylim=c(-4,4), main="Cluster 3")
boxplot(subset(dat_clust, x=="4")[2:17], horizontal=T, las=1, ylim=c(-4,4), main="Cluster 4")
boxplot(subset(dat_clust, x=="5")[2:17], horizontal=T, las=1, ylim=c(-4,4), main="Cluster 5")
boxplot(subset(dat_clust, x=="6")[2:17], horizontal=T, las=1, ylim=c(-4,4), main="Cluster 6")
boxplot(subset(dat_clust, x=="7")[2:17], horizontal=T, las=1, ylim=c(-4,4), main="Cluster 7")
boxplot(subset(dat_clust, x=="8")[2:17], horizontal=T, las=1, ylim=c(-4,4), main="Cluster 8")
boxplot(subset(dat_clust, x=="9")[2:17], horizontal=T, las=1, ylim=c(-4,4), main="Cluster 9")
boxplot(subset(dat_clust, x=="10")[2:17], horizontal=T, las=1, ylim=c(-4,4), main="Cluster 10")
par(mfrow=c(1,1))

#calculate cluster means for different variables
c1m <- data.frame(one=colMeans(subset(dat_clust, x=="1")[2:17]))
c2m <- data.frame(two=colMeans(subset(dat_clust, x=="2")[2:17]))
c3m <- data.frame(three=colMeans(subset(dat_clust, x=="3")[2:17]))
c4m <- data.frame(four=colMeans(subset(dat_clust, x=="4")[2:17]))
c5m <- data.frame(five=colMeans(subset(dat_clust, x=="5")[2:17]))
c6m <- data.frame(six=colMeans(subset(dat_clust, x=="6")[2:17]))
c7m <- data.frame(seven=colMeans(subset(dat_clust, x=="7")[2:17]))
c8m <- data.frame(eight=colMeans(subset(dat_clust, x=="8")[2:17]))
c9m <- data.frame(nine=colMeans(subset(dat_clust, x=="9")[2:17]))
c10m <- data.frame(ten=colMeans(subset(dat_clust, x=="10")[2:17]))

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

#rotate columns and rows
cluster_means_t <- cluster_means[1:10]
row.names(cluster_means_t) <- cluster_means$variable
cluster_means_t <- data.frame(t(cluster_means_t))
cluster_means_t$cluster <- row.names(cluster_means_t)
row.names(cluster_means_t) <- NULL
cluster_means_t

dotchart(cluster_means_t[,1], labels=cluster_means_t$cluster)

#dimension reduction
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
merged_clean$x_f <- as.factor(merged_clean$x)

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


#
library("RCurl")
library(RCurl)
URL <- "https://www2.census.gov/geo/tiger/TIGER2018/CBSA/tl_2018_us_cbsa.zip"
download.file(URL,destfile="data/MSA/MSA.zip",method="libcurl")
unzip(here("data/MSA/MSA.zip"))
msa_Boundary <-readOGR(here("data/MSA"),"tl_2018_us_cbsa") 
merged <- merge(msa_Boundary,data,by.x="NAME",by.y="X")
spplot(merged,"x")

library(leaflet) 
leaflet(merged) %>%
  addPolygons()

library(leaflet) 
pal <- colorFactor(rainbow(8), merged$x,
                   na.color = "transparent")
p_popup <- paste0("<strong>MSA: </strong>", merged$NAME)

leaflet(merged) %>%
  addPolygons(
    stroke = FALSE, # remove polygon borders
    fillColor = ~pal(x), # set fill color with function from above and value
    fillOpacity = 0.8, smoothFactor = 0.5, # make it nicer
    popup = p_popup) %>%
  addTiles() %>%
  addLegend("bottomright",  # location
            pal=pal,    # palette function
            values=~x,  # value to be passed to palette function
            title = 'Cluster') # legend title