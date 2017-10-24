library(foreach)
library(doParallel)
library(parallel)
# load in data and looFit
library(tidyverse)
library(forcats)
library(lubridate)
library(stringr)
library(cluster)
library(gridExtra)
library(knitr)

library(maps)
library(ggplot2)
library(dplyr)
library('Rcpp')
sourceCpp('Rcpp_demo.cpp')
# load the data
#ling_data <- read.table('data/lingBinary.Rdata')
load("data/lingBinary.RData")

#set up the number of cores
nCores <- 20
registerDoParallel(nCores) 

#R version

lingdata = lingBinary[-c(1:6)]
rownames(lingdata) <- 1:nrow(lingdata)
m = 0.3
N = 100
kmax = 10
datasize = round(m*45152)
cumfreq <- matrix(rep(0,N*71),nrow=N,ncol=71)
foreach(k= 1:kmax) %dopar% { #outer loop
corrmatrix <- c()
for (i in 1:N){ #the second loop
sub1 = sample(1:45152,datasize)
sub2 = sample(1:45152,datasize)
common = intersect(sub1,sub2)
c_length = length(common)
c_char = as.character(common)
lingdata1 = lingdata[sub1,]
lingdata2 = lingdata[sub2,]
# k-means using all three variables
#kmeans.raw <- kmeans(scale(lingdata), centers = k)
# spectral clustering using first 2 pc only
kmeanssub1 <- kmeans(lingdata1, centers = k)
kmeanssub2 <- kmeans(lingdata2, centers = k)
neighbor1 = matrix(rep(0,c_length*c_length),nrow=c_length,ncol=c_length)
neighbor2 = matrix(rep(0,c_length*c_length),nrow=c_length,ncol=c_length)
# get the similarity matrix
for (ii in 1:k){ 
  vector1 <- c()
  vector2 <- c()
  for (j in 1:c_length){
    if (kmeanssub1$cluster[c_char[j]]==ii){
      vector1 <- c(vector1,j)
    }
    if (kmeanssub2$cluster[c_char[j]]==ii){
      vector2 <- c(vector2,j)
    }
  neighbor1[vector1,vector1] <- 1
  neighbor2[vector2,vector2] <- 1
  }
}



msum = neighbor1 + neighbor2
cij1 = length(which(neighbor1 == 1))
cij2 = length(which(neighbor2 == 1))
cij = length(which(msum==2))
cij1 = cij1 - c_length
cij2 = cij2 - c_length
cij = cij - c_length
corr = cij/(sqrt(cij1)*sqrt(cij2))
corrmatrix <- c(corrmatrix,corr)
}

breaks = seq(0.3,1, by=0.01)
duration.cut = cut(corrmatrix,breaks,right=FALSE)
duration.freq = table(duration.cut)
#cumfreq[k,] = c(0,cumsum(duration.freq))

picname = paste("hist_3_1", as.character(k))
jpeg(picname)
hist(corrmatrix, main = paste("k=",k),col = "blue")
dev.off()
breaks = seq(0.3,1, by=0.01)
picname = paste("curve_3_1",as.character(k))
jpeg(picname)
plot(breaks,c(0,cumsum(duration.freq)),ylab = "cumulative")
lines(breaks,c(0,cumsum(duration.freq)))
dev.off()
}

#breaks = seq(0.3,1, by=0.01)
#jpeg("curve")
#for (rr in 1:10){
#plot(breaks,c(0,cumsum(duration.freq)),ylab = "cumulative")
#lines(breaks,result1[rr,])
#par(new=TRUE)
#}
#dev.off()
#par(new=FALSE)


#C++ to reduce the time
#save the cluster vector
cumfreq2 <- matrix(rep(0,N*71),nrow=N,ncol=71)
corrmatrix <- c()
foreach(k=1:kmax) %dopar% {
for (i in 1:N){
  sub1 = sample(1:45152,datasize)
  sub2 = sample(1:45152,datasize)
  common = intersect(sub1,sub2)
  c_length = length(common)
  c_char = as.character(common)
  lingdata1 = lingdata[sub1,]
  lingdata2 = lingdata[sub2,]
  # k-means using all three variables
  #kmeans.raw <- kmeans(scale(lingdata), centers = k)
  # spectral clustering using first 2 pc only
  kmeanssub1 <- kmeans(lingdata1, centers = k)
  kmeanssub2 <- kmeans(lingdata2, centers = k)
  neighbor1 = matrix(rep(0,c_length*c_length),nrow=c_length,ncol=c_length)
  neighbor2 = matrix(rep(0,c_length*c_length),nrow=c_length,ncol=c_length)
  clustertype1 <- c()
  clustertype2 <- c()
  for (ii in 1:c_length){
    clustertype1 <- c(clustertype1,kmeanssub1$cluster[c_char[ii]])
    clustertype2 <- c(clustertype2,kmeanssub2$cluster[c_char[ii]])
  }
  #CPP to get the distance matrix
  l1 <- DistanceCPP(clustertype1, clustertype2)
  l2 <- DistanceCPP(clustertype1, clustertype1)
  l3 <- DistanceCPP(clustertype2, clustertype2)
  corr = l1/(sqrt(l2)*sqrt(l3))
  corrmatrix <- c(corrmatrix,corr)
}
picname = paste("hist_3_2", as.character(k))
jpeg(picname)
hist(corrmatrix, main = paste("k=",k),col = "blue")
dev.off()
breaks = seq(0.3,1, by=0.01)
duration.cut = cut(corrmatrix,breaks,right=FALSE)
duration.freq = table(duration.cut)
picname = paste("curve_3_2",as.character(k))
jpeg(picname)
plot(breaks,c(0,cumsum(duration.freq)),ylab = "cumulative")
lines(breaks,c(0,cumsum(duration.freq)))
dev.off()
#output <-cumfreq2
}

#jpeg("curve_5_2")
#for (rr in 1:kmax){
  #plot(breaks,result2[rr,],ylab = "cumulative")
  #lines(breaks,result2[rr,])
  #par(new=TRUE)
#}
#dev.off()
#par(new=FALSE)
