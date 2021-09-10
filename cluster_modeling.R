library(here)
library(mclust)

#import raw data
dat_raw <- read.csv(here("data/CommunityData-raw-2015.csv"))
#select columns of interest
dat  <- dat_raw[,c(4:ncol(dat_raw))]
#convert to dataframe
dat = as.data.frame(unclass(dat))
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


#print table showing probability that city belongs to class
class_prob <- round(dat_mc8$z, digits=3)
class_prob

