library(caret)
data("iris")
par=createDataPartition(y=iris$Species,p=0.75,list=FALSE)
train=iris[par,]
test=iris[-par,]
training.data=scale(train[-5])
summary(training.data)

iris.kmeans=kmeans(training.data[,-5],centers = 3,iter.max = 1000)
train$cluster=as.factor(iris.kmeans$cluster)

library(NbClust)
nc=NbClust(training.data,min.nc = 2,max.nc = 15,method="kmeans")
nc
nc$Best.nc
par(mfrow=c(1,2))
best=table(nc$Best.nc[1,])
barplot(best,xlab="# of cluster",ylab="criteria",main="Home alone")

library(cluster)
dista=dist(training.data)
sk=silhouette(iris.kmeans$cluster,dista)
plot(sk)

