library(plyr)
library(FactoMineR)

################################# SERIAL VERSION ###############################

# Function to obtain the y coordinate of elbow graph

k_means<- function(x){
  data<-computers
  return(kmeans(data, x)$tot.withinss)}

initial<-Sys.time()

# Read the data file as a data.frame:
computers = read.csv("computers.csv",sep = ";", dec = ",")

# Adapt non numerical variables to (0,1): cd, multi, premium

computers$cd <- revalue(computers$cd, c("yes"=1))
computers$cd <- revalue(computers$cd, c("no"=0))
computers$cd <- as.integer(computers$cd)

computers$premium <- revalue(computers$premium, c("yes"=1))
computers$premium <- revalue(computers$premium, c("no"=0))
computers$premium <- as.integer(computers$premium)

computers$multi <- revalue(computers$multi, c("yes"=1))
computers$multi <- revalue(computers$multi, c("no"=0))
computers$multi <- as.integer(computers$multi)

# Delete id column
computers<-computers[,-1]

# Store sd and mean of price to use it later
sd_price<-sd(computers$price)
mean_price<-mean(computers$price)

# Scale data
computers <- scale(computers)

# Number of clusters to try
n<-9

# Obtain y-values 
wss<-list()
for (x in c(1:n)){
  wss[x]<-k_means(x+1)
}

# Cluster with optimum k value, for example 4
bestkmeans <- kmeans(computers, 4)

# Obtain coords of the first two dimensions from PCA 
pca<-PCA(computers, scale.unit = FALSE, ncp = 2, graph=FALSE)
coords <-pca$ind$coord

# Serial execution time
last<-Sys.time()
print("Serial execution time:")
print(last-initial)

################################# PLOTS #######################################

# Elbow graph
plot(2:(n+1), wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares", 
     main="Elbow graph", col = hsv(0.5, seq(0,.50,length.out = 12), .75))

# 2 dimensions plot
plot(coords, col=bestkmeans$cluster, main="Clusters in 2D",xlab="Dimension 1",
     ylab="Dimension 2")

# Expensive cluster
centers_df<-data.frame(bestkmeans$centers)
expensive_cluster<-centers_df[centers_df$price >= max(centers_df$price),]
expensive_cluster

# Price of the expensive cluster without standarization
expensive_cluster$price*(sd_price)+mean_price

# Heat map
heatmap(bestkmeans$centers, main="Heatmap for the clusters centroids")