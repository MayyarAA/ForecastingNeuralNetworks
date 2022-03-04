# Q1.
library(caTools)
library(rpart) 
library(rpart.plot)
#Q1.1.a)i)
# Read in the data


inputValuesFromCSV = read.csv("/Users/mayyaral-atari/Downloads/Letters.csv")
str(inputValuesFromCSV)
inputValuesFromCSV$isB <- NA
inputValuesFromCSV$isB = as.factor(inputValuesFromCSV$Letter == "B")

set.seed(1000)
split = sample.split(inputValuesFromCSV$isB,SplitRatio = 0.5)
trainingSet = subset(inputValuesFromCSV,split=TRUE)
testingSet = subset(inputValuesFromCSV,split=FALSE)
table(testingSet$isB)

#Q1.1.a)ii)CART

CARTLettersV1 = rpart(isB ~ . - Letter,data =trainingSet,method="class" )
predictionsForBQ1AII = predict(CARTLettersV1,newdata=testingSet,type="class")
table(testingSet$isB,predictionsForBQ1AII)
