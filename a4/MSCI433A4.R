#MSCI 433 Assignment 4
library(dplyr)
library(ggplot2)
library(caret)
library(cluster)
#Q1. 

dailykosCSV = read.csv("/Users/mayyaral-atari/Desktop/work/uni/msci433/assignments/a4/DailyKos.csv")
dkDist = dist(dailykosCSV, method="euclidean")
dkscluster = hclust(dkDist, method="ward.D2")
dkCutTree = cutree(dkscluster, k = 6, h = NULL)
cluster1 = dailykosCSV %>% filter(dkCutTree == 1)
cluster2 = dailykosCSV %>% filter(dkCutTree == 2)
cluster3 = dailykosCSV %>% filter(dkCutTree == 3)
cluster4 = dailykosCSV %>% filter(dkCutTree == 4)
cluster5 = dailykosCSV %>% filter(dkCutTree == 5)
cluster6 = dailykosCSV %>% filter(dkCutTree == 6)

plot(dkCutTree)

#Q1.a.iv) 
val1 = tail(sort(colMeans(cluster1)))
print(val1)
val2 = tail(sort(colMeans(cluster2)))
print(val2)
val3 = tail(sort(colMeans(cluster3)))
print(val3)
val4 = tail(sort(colMeans(cluster4)))
print(val4)
val5 = tail(sort(colMeans(cluster5)))
print(val5)
val6 = tail(sort(colMeans(cluster6)))
print(val6)



#Q1.b)i)



kmeansDK = kmeans(dailykosCSV, centers = 6)
kmcluster2 = dailykosCSV %>% filter(kmeansDK$cluster == 1)
kmcluster1 = dailykosCSV %>% filter(kmeansDK$cluster == 2)
kmcluster5 = dailykosCSV %>% filter(kmeansDK$cluster == 3)
kmcluster4 = dailykosCSV %>% filter(kmeansDK$cluster == 4)
kmcluster6 = dailykosCSV %>% filter(kmeansDK$cluster == 5)
kmcluster3 = dailykosCSV %>% filter(kmeansDK$cluster == 6)



#Q1.b)ii)
kmval1 = tail(sort(colMeans(kmcluster1)))
print(kmval1)
kmval2 = tail(sort(colMeans(kmcluster2)))
print(kmval2)
kmval3 = tail(sort(colMeans(kmcluster3)))
print(kmval3)
kmval4 = tail(sort(colMeans(kmcluster4)))
print(kmval4)
kmval5 = tail(sort(colMeans(kmcluster5)))
print(kmval5)
kmval6 = tail(sort(colMeans(kmcluster6)))
print(kmval6)


#Q1.c)

#Hierical clustering with 4 clusters
dkCutTreePartC = cutree(dkscluster, k = 4, h = NULL)
partCcluster1PartCHC = dailykosCSV %>% filter(dkCutTreePartC == 1)
partCcluster2PartCHC = dailykosCSV %>% filter(dkCutTreePartC == 2)
partCcluster3PartCHC = dailykosCSV %>% filter(dkCutTreePartC == 3)
partCcluster4PartCHC = dailykosCSV %>% filter(dkCutTreePartC == 4)
plot(dkCutTreePartC)

PartCval1 = tail(sort(colMeans(partCcluster1PartCHC)))
print(PartCval1)
PartCval2 = tail(sort(colMeans(partCcluster2PartCHC)))
print(PartCval2)
PartCval3 = tail(sort(colMeans(partCcluster3PartCHC)))
print(PartCval3)
PartCval4 = tail(sort(colMeans(partCcluster4PartCHC)))
print(PartCval4)

#kmeans clustering 4 clusters
partckmeansDK = kmeans(dailykosCSV, centers = 4)
partckmcluster1 = dailykosCSV %>% filter(partckmeansDK$cluster == 1)
partckmcluster2 = dailykosCSV %>% filter(partckmeansDK$cluster == 2)
partckmcluster3 = dailykosCSV %>% filter(partckmeansDK$cluster == 3)
partckmcluster4 = dailykosCSV %>% filter(partckmeansDK$cluster == 4)
table(partckmeansDK$cluster)

partckmval1 = tail(sort(colMeans(partckmcluster1)))
print(partckmval1)
partckmval2 = tail(sort(colMeans(partckmcluster2)))
print(partckmval2)
partckmval3 = tail(sort(colMeans(partckmcluster3)))
print(partckmval3)
partckmval4 = tail(sort(colMeans(partckmcluster4)))
print(partckmval4)
#################################################################################
#Q2)
hubwayCSV = read.csv("/Users/mayyaral-atari/Desktop/work/uni/msci433/assignments/a4/HubwayTrips.csv")
#Q2.a.ii)
hubwayinfo = preProcess(hubwayCSV)
normalizehubway = predict(hubwayinfo,hubwayCSV)
print(summary(normalizehubway))
#Q2.b.i)
kmeansHW = kmeans(normalizehubway, centers = 10)
hubwaykmcluster1 = normalizehubway %>% filter(kmeansHW$cluster == 1)
hubwaykmcluster2 = normalizehubway %>% filter(kmeansHW$cluster == 2)
hubwaykmcluster3 = normalizehubway %>% filter(kmeansHW$cluster == 3)
hubwaykmcluster4 = normalizehubway %>% filter(kmeansHW$cluster == 4)
hubwaykmcluster5 = normalizehubway %>% filter(kmeansHW$cluster == 5)
hubwaykmcluster6 = normalizehubway %>% filter(kmeansHW$cluster == 6)
hubwaykmcluster7 = normalizehubway %>% filter(kmeansHW$cluster == 7)
hubwaykmcluster8 = normalizehubway %>% filter(kmeansHW$cluster == 8)
hubwaykmcluster9 = normalizehubway %>% filter(kmeansHW$cluster == 9)
hubwaykmcluster10 = normalizehubway %>% filter(kmeansHW$cluster == 10)
tapply(hubwaykmcluster1$Duration, hubwaykmcluster1, mean)

nonormalizedkmeansHW = kmeans(hubwayCSV, centers = 1)
nonnormalizedhubwaykmcluster1 = hubwayCSV %>% filter(nonormalizedkmeansHW$cluster == 1)
tapply(nonnormalizedhubwaykmcluster1$Duration, nonormalizedkmeansHW$cluster, mean)
#Q2.c.i)
hubwaykmcluster1centriod = colMeans(hubwaykmcluster1)
print(hubwaykmcluster1centriod)

partCkmeansHW = kmeans(normalizehubway, centers = 10)
partChubwaykmcluster1 = normalizehubway %>% filter(partCkmeansHW$cluster == 1)
partChubwaykmcluster2 = normalizehubway %>% filter(partCkmeansHW$cluster == 2)
partChubwaykmcluster3 = normalizehubway %>% filter(partCkmeansHW$cluster == 3)
partChubwaykmcluster4 = normalizehubway %>% filter(partCkmeansHW$cluster == 4)


#Q2.c.ii)
partCnonormalizedkmeansHW = kmeans(hubwayCSV, centers = 4)
nonpartCnonnormalizedhubwaykmcluster1 = hubwayCSV %>% filter(partCnonormalizedkmeansHW$cluster == 1)
tapply(nonpartCnonnormalizedhubwaykmcluster1$Duration, partCnonormalizedkmeansHW$cluster, mean)
nonpartCnonnormalizedhubwaykmcluster2 = hubwayCSV %>% filter(partCnonormalizedkmeansHW$cluster == 2)
tapply(nonpartCnonnormalizedhubwaykmcluster2$Duration, partCnonormalizedkmeansHW$cluster, mean)
nonpartCnonnormalizedhubwaykmcluster3 = hubwayCSV %>% filter(partCnonormalizedkmeansHW$cluster == 3)
tapply(nonpartCnonnormalizedhubwaykmcluster3$Duration, partCnonormalizedkmeansHW$cluster, mean)
nonpartCnonnormalizedhubwaykmcluster4 = hubwayCSV %>% filter(partCnonormalizedkmeansHW$cluster == 4)
tapply(nonpartCnonnormalizedhubwaykmcluster4$Duration, partCnonormalizedkmeansHW$cluster, mean)


#################################################################################
#Q3)

#Q3.a.i)
paroleCSV = read.csv("/Users/mayyaral-atari/Desktop/work/uni/msci433/assignments/a4/Parole.csv")
ggplot(data = paroleCSV, aes(x = Age)) + geom_histogram(binwidth=5)
#Q3.a.ii)
ggplot(data = paroleCSV, aes(x = Age)) + geom_histogram(binwidth=5,color="blue")

#Q3.b.i)
ggplot(data = paroleCSV, aes(x = Age)) + geom_histogram(binwidth=5) 
+ facet_grid(Male~.)


#Q3.b.ii)
ggplot(data = paroleCSV, aes(x = Age)) + geom_histogram(binwidth=5) + facet_grid(.~ Male)

#Q3.b.iii)
ggplot(data = paroleCSV, aes(x = Age, fill = "Male")) + geom_histogram(binwidth=5,position = "identity",alpha=0.5)

#Q3.c)
#Q3.c.i)
ggplot(data = paroleCSV, aes(x = TimeServed)) + geom_histogram(binwidth=1)

#Q3.c.ii)
ggplot(data = paroleCSV, aes(x = TimeServed)) + geom_histogram(binwidth=.1)

#Q3.c.iii)
ggplot(data = paroleCSV, aes(x = TimeServed)) + geom_histogram(binwidth=1) + facet_grid(Crime ~ .)
#Q3.c.iv)
ggplot(data = paroleCSV, aes(x = TimeServed, fill = Crime)) + geom_histogram(binwidth=5,position = "identity",alpha=0.5)









