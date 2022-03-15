#MSCI 433 Assignment 4
library(dplyr)
library(ggplot)
library(ggplot2)
#Q1. 

dailykosCSV = read.csv("/Users/mayyaral-atari/Desktop/work/uni/msci433/assignments/a4/DailyKos.csv")
#dailykosCSV
dkDist = dist(dailykosCSV, method="euclidean")
koscluster = hclust(dkDist, method="ward.D2")
kosHierClust
hierGroups = cutree(koscluster, k = 6, h = NULL)
cluster1 = dailykosCSV %>% filter(hierGroups == 1)

cluster2 = dailykosCSV %>% filter(hierGroups == 2)

cluster3 = dailykosCSV %>% filter(hierGroups == 3)

cluster4 = dailykosCSV %>% filter(hierGroups == 4)

cluster5 = dailykosCSV %>% filter(hierGroups == 5)

cluster6 = dailykosCSV %>% filter(hierGroups == 6)

plot(hierGroups)

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

kmcluster1 = dailykosCSV %>% filter(kmeansDK$cluster == 1)

kmcluster2 = dailykosCSV %>% filter(kmeansDK$cluster == 2)

kmcluster3 = dailykosCSV %>% filter(kmeansDK$cluster == 3)

kmcluster4 = dailykosCSV %>% filter(kmeansDK$cluster == 4)

kmcluster5 = dailykosCSV %>% filter(kmeansDK$cluster == 5)

kmcluster6 = dailykosCSV %>% filter(kmeansDK$cluster == 6)
table(kmeansDK$cluster)



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

#################################################################################
#Q2)








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
ggplot(data = paroleCSV, aes(x = TimeServed, fill = "Crime")) + geom_histogram(binwidth=5,position = "identity",alpha=0.5)









