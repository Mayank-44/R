library(dbscan)
library(fpc)
library(plyr)
library(cluster)

#===================================SIZE OF CLUSTER==============================================

size_of_cluster=5
cluster_size<- sample(50:100,size_of_cluster)
library(MASS) #for mvrnorm

#===============================================NORMAL DISTRIBUTION===================================================

norm1<-data.frame(mvrnorm(cluster_size[1],mu=c(-1,-5),Sigma = diag(2)),rep(1))
names(norm1)<-c("x","y","label")
plot(x=norm1$x,y=norm1$y,pch=20)

#========================================== NORMAL DISTRIBUTION==========================================================

norm2<-data.frame(mvrnorm(cluster_size[2],mu=c(-5,1),Sigma = diag(2)),rep(2))
names(norm2)<-c("x","y","label")
plot(x=norm2$x,y=norm2$y,pch=20)

#======================================= NORMAL DISTRIBUTION=========================================================

norm3<-data.frame(mvrnorm(cluster_size[3],mu=c(-6,-6),Sigma = diag(2)),rep(3))
names(norm3)<-c("x","y","label")
plot(x=norm3$x,y=norm3$y,pch=20)

#==============================================EXPONENTIAL DISTRIBUTION=================================================

x1<-data.frame(rexp(cluster_size[4],rate = 1.5))
names(x1)<-c("x")
x2<-data.frame(rexp(cluster_size[4],rate =1.8 ),rep(4))
names(x2)<-c("y","label")
expdata<-cbind(x1,x2)
plot(x=expdata$x,y=expdata$y,pch=20)

#========================================= UNIFORM DISTRIBUTION=========================================================

x1<-data.frame(runif(cluster_size[5],min =-6 , max = -1))
names(x1)<-c("x")
x2<-data.frame(runif(cluster_size[5],min = 3,max = 5),rep(5))
names(x2)<-c("y","label")
uniformd<-cbind(x1,x2)

#===========================================DATASET=====================================================================
par(bg = "grey")
dataset<- rbind(norm1,norm2,norm3,expdata,uniformd)
color=c("red", "blue", "yellow","black", "magenta")
plot(x=dataset$x,y=dataset$y,col=color[dataset$label],pch=20,xlab = "x-coordinate",ylab = "y-coordinate",main="GIVEN DATASET " )
legend("bottomright",legend=c("normal","normal","normal","exponential","uniform"),pch=20,col=color)
text(-2,-2,"No.of data points :")
text(-2,-3,sum(cluster_size))

#======================================creating purity function===========================================

purity<-function(clust,siz)
{
  n<-siz
  result_cluster<-clust  
  
  #======================================= confusion matrix========================================
  t1<-table( result_cluster,dataset$label)
  colnames(t1)<-c("c1","c2","c3","c4","c5")
  
  each<-sample(1:n,n)
  #==================================== intialization of array===================================
  
  for(i in 1:n)
  {
    each[i]=0
  }
  
  #================================================calculating purity===============================
  for(i in 1:n)
  {
    for(j in 1:size_of_cluster)
    {
      if(each[i]<t1[i,j])
        each[i]=t1[i,j]
      
    }
  }  
  s1<-sum(each)
  total<-sum(cluster_size)
  purity<-s1/total
}

# =======================================================k-means================================

result1<-kmeans(dataset[,1:2],size_of_cluster)
result1
kpurity<-purity(result1$cluster,size_of_cluster)
kpurity

#=======================================kmeans SSE===============================================

SSE<-0
for(i in 1:nrow(dataset))
{
  SSE<-SSE+ (result1$center[result1$cluster[i]]-dataset[i,1])^2+(result1$center[result1$cluster[i],2]-dataset[i,2])^2
  
}

SSE
par(bg = "grey")
color=c("red", "blue", "yellow","black", "magenta")
plot(x=dataset$x,y=dataset$y,col=color[result1$cluster],pch=20,xlab = "x-coordinate",ylab = "y-coordinate",main="kmeans " )
points.default(result1$centers,pch=21,bg="magenta",cex=1.5)
legend("bottomright",legend=levels(factor(result1$cluster)),pch=20,col=color)
text(x=-6,y=0,labels=paste("PURITY =",kpurity))
text(x=-6,y=-2,labels=paste("SSE =", SSE))



#============================================ HIERACHICAL CLUSTERING=============================

library(cluster)
clusters <- hclust(dist(dataset[,1:2]))
clusters
plot(clusters)
hclust_result<-cutree(clusters,5)
heirachical_purity<-purity(hclust_result,5)
heirachical_purity

##hierarchical SSE
ax<-numeric(5)
ay<-numeric(5)
an<-numeric(5)
for(i in 1:nrow(dataset))
{
  ax[hclust_result[i]]<-ax[hclust_result[i]]+dataset[i,1]
  ay[hclust_result[i]]<-ay[hclust_result[i]]+dataset[i,2]
  an[hclust_result[i]]<-an[hclust_result[i]]+1
  
}


for(i in 1:size_of_cluster)
{
ax[i]<-ax[i]/an[i]
ay[i]<-ay[i]/an[i]
}

SSEh<-0
for(i in 1:nrow(dataset))
{
  SSEh<-SSEh+ (ax[hclust_result[i]]-dataset[i,1])^2+(ay[hclust_result[i]]-dataset[i,2])^2
  
}

SSEh

par(bg = "grey")
color=c("red", "blue", "yellow","black", "magenta")
plot(x=dataset$x,y=dataset$y,col=color[hclust_result],pch=20,xlab = "x-coordinate",ylab = "y-coordinate",main="hierarchical clustering " )
legend("bottomright",legend=levels(factor(hclust_result)),pch=20,col=color)
text(x=-2,y=0,labels=paste("Purity =",heirachical_purity))
text(x=-2,y=-2,labels=paste("SSE =", SSEh))

#=========================================PURITY OF DBSCAN=====================================

puritydb<-function(clust,siz)
{
  n<-siz
  result_cluster<-clust  
  ## confusion matrix
  t1<-table( result_cluster,dataset$label)
  colnames(t1)<-c("t1","t2","t3","t4","t5")
  each<-numeric(n+1)
  ##calculating purity
  for(i in 1:n+1)
  {
    for(j in 1:5)
    {
      if(each[i]<t1[i-1,j])
        each[i]=t1[i-1,j]
    }
  }  
  s1<-sum(each)
  total<-sum(cluster_size)
  purity<-s1/total
}


#===================================DENSITY BASED CLUSTERING====================================

DBSCAN_result<-fpc::dbscan(dataset[,1:2],eps = 1,MinPts = 8)
DBSCAN_result
#plot(x=dataset$x,y=dataset$y,bg=(color)[unclass(DBSCAN_result$cluster+1)],xlab = "x-coordinate",ylab = "y-coordinate",main="dbscan",pch=21)
a1<-count(DBSCAN_result$cluster)
n<-max(a1$x)
n
density_purity<-puritydb(DBSCAN_result$cluster,n)
density_purity

#====================================SSE OF DBSCAN==============================================

axd<-numeric(n+1)
ayd<-numeric(n+1)
and<-numeric(n+1)
for(i in 1:nrow(dataset))
{  
axd[DBSCAN_result$cluster[i]+1]<-axd[DBSCAN_result$cluster[i]+1]+dataset[i,1]
ayd[DBSCAN_result$cluster[i]+1]<-ayd[DBSCAN_result$cluster[i]+1]+dataset[i,2]
and[DBSCAN_result$cluster[i]+1]<-and[DBSCAN_result$cluster[i]+1]+1

}

for(i in 1:n+1)
{ 
axd[i]<-axd[i]/and[i]
ayd[i]<-ayd[i]/and[i]
}

SSEd<-0
for(i in 1:nrow(dataset))
{
  SSEd<-SSEd+ (axd[DBSCAN_result$cluster[i]+1]-dataset[i,1])^2+(ayd[DBSCAN_result$cluster[i]+1]-dataset[i,2])^2
  
}

SSEd
par(bg = "grey")
color=c("blue", "black", "yellow","red", "magenta","orange","brown")
plot(x=dataset$x,y=dataset$y,bg=(color)[unclass(DBSCAN_result$cluster+1)],pch=21,xlim=c(-10,10),ylim=c(-10,10),xlab = "x-coordinate",ylab = "y-coordinate",main="DBSCAN" )
legend("bottomright",legend=levels(factor(DBSCAN_result$cluster)),pch=20,xlim=c(-10,10),ylim=c(-10,10),col=color)
text(x=0,y=8,labels=paste("Purity =",density_purity))
text(x=0,y=6,labels=paste("SSE =", SSEd))