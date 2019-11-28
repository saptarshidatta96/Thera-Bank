setwd("C:\\Users\\Saptarshi Datta\\Desktop\\R PROGRAMMING")
Data=read.csv("Thera Bank.csv", header=TRUE)


Data=Data[,-c(1,5)]

attach(Data)

Data$Mortgage=ifelse(Data$Mortgage>0,1,0)
Data$Experience..in.years.=ifelse(Data$Experience..in.years.<0,0,Data$Experience..in.years.)
Data$Education=as.factor(Data$Education)
Data$Personal.Loan=as.factor(Data$Personal.Loan)
Data$Securities.Account=as.factor(Data$Securities.Account)
Data$CD.Account=as.factor(Data$CD.Account)
Data$Online=as.factor(Data$Online)
Data$CreditCard=as.factor(Data$CreditCard)
##Data$ZIP.Code=as.factor(Data$ZIP.Code)
Data$Mortgage=as.factor(Data$Mortgage)

summary(Data)
str(Data)
is.na(Data)
Data1=na.omit(Data)
Data=Data1


library(funModeling)
library(tidyverse) 
library(Hmisc)

basic_eda <- function(data)
{
  glimpse(data)
  df_status(data)
  freq(data) 
  profiling_num(data)
  plot_num(data)
  describe(data)
  
}
basic_eda(Data)

boxplot(Data)
x=cor(Data[,c(1,2,3,4,5)])
library(corrplot)
corrplot(x,method="number")

library(cluster)
seed=10000
set.seed(seed)
gower_matrix <-daisy(Data,metric="gower")
dist <-gower_matrix
pamxx <-pam(dist,k=3)
sil1 = silhouette(pamxx$clustering,dist)
plot(sil1,col=c("red","blue"))
summary(sil1)
print(pamxx$silinfo$avg.width)
clusplot(Data,pamxx$clustering,color=TRUE,shade=TRUE,label= 2, lines = 1)
Data$cluster=pamxx$clustering
custprofile=aggregate(Data,list(Data$cluster),FUN ="mean")

set.seed(seed)
a=rep(0,10)
for(j in 1:100)
{
  
    pamxx <-pam(dist,j)
    sil = silhouette(pamxx$clustering,dist)
    print(j)
    print(pamxx$silinfo$avg.width)
  
}
plot(c(1:10),a$avg.width)


Data.scaled=scale(Data[,c(1,2,3,5,6,7,8,9,10,11,12,13)])
DistMatrix.scaled=dist(Data.scaled,method='euclidean')
print(DistMatrix.scaled)
#apply(DistMatrix.scaled,2,mean)
#apply(DistMatrix.scaled,2,sd)

cluster=hclust(DistMatrix.scaled,method='average')
plot(cluster)
cluster$height
rect.hclust(cluster,k=2,border='red')

Data$h.clust=cutree(cluster,k=2)
profile=aggregate(Data[,-c(4)],list(Data$h.clust),FUN='mean')
print(profile)

seed=1000
set.seed(seed)
cluster2=kmeans(x=Data.scaled,centers=2, nstart =5)
print(cluster2)
library(cluster)
clusplot(Data,cluster2$cluster,color=TRUE,shade=TRUE,label= 2, lines = 1)

a=rep(0,10)

for(j in 1:10)
{
  set.seed(seed)
  cluster3=kmeans(x=Data.scaled,centers=j,nstart=5)
  a[j]=cluster3$tot.withinss
  print(j)
  print(cluster3$tot.withinss)
}
plot(c(1:10),a,type='b')
print(a)

library(NbClust)
nc=NbClust(Data[,c(-14)],min.nc=2,max.nc=5,method='kmeans')

set.seed(seed)
cluster4=kmeans(x=Data.scaled,centers=2, nstart=5)
print(cluster4)
clusplot(Data.scaled,cluster4$cluster,color=TRUE,shade=TRUE,label=2,line=1)
Data$cluster=cluster4$cluster
custprofile=aggregate(Data,list(Data$cluster),FUN="mean")


