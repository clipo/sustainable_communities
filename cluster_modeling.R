library(here)
library(mclust)

#import raw data
dat_raw <- read.csv(here("data/CommunityData-raw-2015.csv"))
#select columns of interest
dat  <- dat_raw[,c(4:ncol(dat_raw))]
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
plot(dat_mc, what='BIC', ylim=c(-38000,-37000))
plot(dat_mc, what='BIC')
#model type VVE chosen as best, with similar BIC scores for 7-12 clusters
#to do additional model comparisons, generate and compare VVE models with 7-12 clusters
dat_mc7 <- Mclust(dat, G=7, modelNames="VVE")
dat_mc8 <- Mclust(dat, G=8, modelNames="VVE")
dat_mc9 <- Mclust(dat, G=9, modelNames="VVE")
dat_mc10 <- Mclust(dat, G=10, modelNames="VVE")
dat_mc11 <- Mclust(dat, G=11, modelNames="VVE")
dat_mc12 <- Mclust(dat, G=12, modelNames="VVE")

#extract BIC scores
BICs<-c(dat_mc7$BIC[1],dat_mc8$BIC[1],dat_mc9$BIC[1],dat_mc10$BIC[1],dat_mc11$BIC[1],dat_mc12$BIC[1])
#plot BIC scores
plot(c(7:12),BICs, pch=16, xlab="Number of Clusters", type='o')
#calculate change in BIC score, since in this method the goal to to maximize BIC
#we calculate change from highest scoring model
delta_BIC <- max(BICs) - BICs
#calculate BIC weights
w_BIC <- round(exp(-0.5*delta_BIC)/sum(exp(-0.5*delta_BIC)), digits=3)
#make table of results
results_table <- cbind.data.frame(clusters=c(7:12),BIC=BICs,delta_BIC,weight=w_BIC)
results_table


#extract classifications, e.g., which city belongs to which cluster
dat_mc8$classification

#write csv file
write.csv(dat_mc8$classification,file=here("results/cluster_assignment.csv"))
data<-read.csv("results/cluster_assignment.csv")

#print table showing probability that city belongs to class
class_prob <- round(dat_mc8$z, digits=3)
class_prob
#######################

library("raster")
library("rgdal")
library("sp")
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
