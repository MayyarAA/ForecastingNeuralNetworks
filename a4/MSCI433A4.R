#MSCI 433 Assignment 4
library(dplyr)
#Q1. 

dailykosCSV = read.csv("/Users/mayyaral-atari/Desktop/work/uni/msci433/assignments/a4/DailyKos.csv")
#dailykosCSV
kosDist = dist(dailykosCSV, method="euclidean")
kosHierClust = hclust(kosDist, method="ward.D2")
kosHierClust
hierGroups = cutree(kosHierClust, k = 6, h = NULL)
HierCluster1 = dailykosCSV %>% filter(hierGroups == 1)

HierCluster2 = dailykosCSV %>% filter(hierGroups == 2)

HierCluster3 = dailykosCSV %>% filter(hierGroups == 3)

HierCluster4 = dailykosCSV %>% filter(hierGroups == 4)

HierCluster5 = dailykosCSV %>% filter(hierGroups == 5)

HierCluster6 = dailykosCSV %>% filter(hierGroups == 6)

plot(hierGroups)
