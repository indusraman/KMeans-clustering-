# Load the necessary packages
install.packages("arulesViz")
install.packages("arules")
install.packages("corrplot")
library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(arules)
library(arulesViz)
library(plyr)
library(caret)
library(corrplot)
# b) Load dataset
#define the filename
filename<-"C:/Users/indupravs/Downloads/DataScienceProjects/MallCustomers/Mall_Customers.csv"
# load the CSV file from the local directory
dataset <- read.csv(filename, header=TRUE)
#Splitting is not needed for unsupervised clustering case studies
# 2. Summarize Data
summary(dataset)
str(dataset)
dataset$Genre<-as.factor(dataset$Genre)
str(dataset)
# a) Descriptive statistics and data visualisations

# correlation plot
correlations <- cor(dataset[,3:5])
corrplot(correlations, method="circle")
corrplot(correlations, method="number")
corrplot(correlations, method="color")

print(correlations)
# Boxplots 
# Create separate boxplots for each attribute
par(mfrow=c(1,3))
for(i in 3:5) {
  boxplot(dataset[,i], main=names(dataset)[i])
}

#Histograms

par(mfrow=c(1,3))
for(i in 3:5) {
  hist(dataset[,i], main=names(dataset)[i])
}

#Density Plots
# create a layout of simpler density plots by attribute
par(mfrow=c(1,3))
for(i in 3:5) {
  plot(density(dataset[,i]), main=names(dataset)[i])
}

#barplots 
#Gender
counts <- table(dataset[,2])
name <- names(dataset)[2]
barplot(counts,  main="Gender Distirbution",
        col=c("lightgreen","lavender"),
        legend = rownames(counts))
print(counts)
#pie chart for gender
pct=round(counts/sum(counts)*100)
lbs=paste(c("Female","Male")," ",pct,"%",sep=" ")
library(plotrix)
pie3D(counts,labels=lbs,
      main="Pie Chart Depicting Ratio of Female and Male",col=c("lightgreen","lavender"))

# 3. Prepare Data
# a) Data Cleaning
#missing data check
# load packages
library(Amelia)
library(mlbench)
# load dataset
# create a missing map
missmap(dataset, col=c("lightgreen", "violet"), legend=TRUE)
#mulitvariate analysis

pairs(dataset$Genre~.,dataset[3:5],col=dataset$Genre)

# box and whisker plots for each attribute by class value

x <- dataset[,3:5]
y <- dataset[,2]
featurePlot(x=x, y=y, plot="box")

# density plots for each attribute by class value
x <- dataset[,3:5]
y <- dataset[,2]
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)



#Identifying the optimal number of clusters using one of the three methods
#elbow method

library(purrr)
set.seed(123)
# function to calculate total intra-cluster sum of square 
iss <- function(k) {
  kmeans(dataset[,3:5],k,iter.max=100,nstart=100,algorithm="Lloyd" )$tot.withinss
}
k.values <- 1:10
iss_values <- map_dbl(k.values, iss)
plot(k.values, iss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total intra-clusters sum of squares")

#average silhouette method

library(cluster) 
library(gridExtra)
library(grid)
k2<-kmeans(dataset[,3:5],2,iter.max=100,nstart=50,algorithm="Lloyd")
s2<-plot(silhouette(k2$cluster,dist(dataset[,3:5],"euclidean")))

k3<-kmeans(dataset[,3:5],3,iter.max=100,nstart=50,algorithm="Lloyd")
s3<-plot(silhouette(k3$cluster,dist(dataset[,3:5],"euclidean")))
k4<-kmeans(dataset[,3:5],4,iter.max=100,nstart=50,algorithm="Lloyd")
s4<-plot(silhouette(k4$cluster,dist(dataset[,3:5],"euclidean")))
#take the clusters with the highest average width
k5<-kmeans(dataset[,3:5],5,iter.max=100,nstart=50,algorithm="Lloyd")
s5<-plot(silhouette(k5$cluster,dist(dataset[,3:5],"euclidean")))
k6<-kmeans(dataset[,3:5],6,iter.max=100,nstart=50,algorithm="Lloyd")
s6<-plot(silhouette(k6$cluster,dist(dataset[,3:5],"euclidean")))

k7<-kmeans(dataset[,3:5],7,iter.max=100,nstart=50,algorithm="Lloyd")
s7<-plot(silhouette(k7$cluster,dist(dataset[,3:5],"euclidean")))
install.packages("NbClust")
install.packages("factoextra")
library(NbClust)
library(factoextra)

fviz_nbclust(dataset[,3:5], kmeans, method = "silhouette")

#Gap Statistic Method

set.seed(125)
stat_gap <- clusGap(dataset[,3:5], FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(stat_gap)

#Now, let us take k = 6 as our optimal cluster –

k6<-kmeans(dataset[,3:5],6,iter.max=100,nstart=50,algorithm="Lloyd")

k6


#Visualizing the Clustering Results using the First Two Principle Components

pcclust=prcomp(dataset[,3:5],scale=FALSE) #principal component analysis
summary(pcclust)
pcclust$rotation[,1:2]

set.seed(1)
ggplot(dataset, aes(x =Annual.Income..k.., y = Spending.Score..1.100.)) + 
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")

ggplot(dataset, aes(x =Spending.Score..1.100., y =Age)) + 
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")



kCols=function(vec){cols=rainbow (length (unique (vec)))
return (cols[as.numeric(as.factor(vec))])}
digCluster<-k6$cluster; dignm<-as.character(digCluster); # K-means clusters
plot(pcclust$x[,1:2], col =kCols(digCluster),pch =19,xlab ="K-means",ylab="classes")
legend("bottomleft",unique(dignm),fill=unique(kCols(digCluster)))

