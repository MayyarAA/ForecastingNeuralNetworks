# Q1.
library(caTools)
library(rpart) 
library(rpart.plot)
library(randomForest)
#Q1.1.a)
# Read in the data


lettersCSV = read.csv("/Users/mayyaral-atari/Downloads/Letters.csv")
lettersCSV$isB = as.factor(lettersCSV$Letter == "B")
set.seed(1000)
spl = sample.split(lettersCSV$isB, SplitRatio = 0.5)
trainingSet = subset(lettersCSV, spl == TRUE)
testingSet = subset(lettersCSV, spl == FALSE)
table(testingSet$isB)


#Q1.1.a)ii)CART

CARTLettersV1 = rpart(isB ~ . - Letter,data =trainingSet,method="class" )
predictionsForBQ1AII = predict(CARTLettersV1,newdata=testingSet,type="class")
table(testingSet$isB,predictionsForBQ1AII)

#Q1.1.a)iii)Random Forest model
RandomForestQ1AIII = randomForest(isB ~ . - Letter,data=trainingSet)
predictionsForQ1AIII = predict(RandomForestQ1AIII,newdata=testingSet)
table(testingSet$isB,predictionsForQ1AIII)

#Q.1.b)

#Q.1.b.i)
sampleSplitForQ1BI = sample.split(lettersCSV$Letter, SplitRatio = 0.5)
testingSetPartB1 = subset(lettersCSV, sampleSplitForQ1BI == FALSE)
trainingSetPartB1 = subset(lettersCSV, sampleSplitForQ1BI == TRUE)
table(trainingSetPartB1$Letter)

#Q.1.b.ii)
sampleSplitForQ1BII = sample.split(lettersCSV$Letter, SplitRatio = 0.7)
testingSetPartBII = subset(lettersCSV, sampleSplitForQ1BII == FALSE)
trainingSetPartBII = subset(lettersCSV, sampleSplitForQ1BII == TRUE)

MODELQ1BII = rpart(Letter ~ . - isB, data=trainingSetPartBII, method="class")
predictionsForQ1BII = predict(MODELQ1BII,newdata=testingSetPartBII,type="class")
table(testingSetPartBII$Letter,predictionsForQ1BII)



#Q.1.b.iii)
lettersCSV$Letter = as.factor(lettersCSV$Letter)
sampleSplitForQ1BIII = sample.split(lettersCSV$Letter, SplitRatio = 0.7)
testingSetPartBIII =  subset(lettersCSV, sampleSplitForQ1BIII == FALSE)
trainingSetPartBIII =  subset(lettersCSV, sampleSplitForQ1BIII == TRUE)
RandomForestQ1BIII = randomForest(Letter ~ . - isB,data=trainingSetPartBIII,ntree = 150, nodesize = 10)
predictionsForQ1AIII = predict(RandomForestQ1BIII,newdata=testingSetPartBIII)
table(testingSetPartBIII$Letter,predictionsForQ1AIII)
(236+224+238+224)/nrow(test)


#Q2.

#Q2.a
#/Users/mayyaral-atari/Desktop/work/uni/msci433/assignments/a3/StateData1.csv

#Q2.a.i)
inputStateCSV = read.csv("/Users/mayyaral-atari/Desktop/work/uni/msci433/assignments/a3/StateData1.csv")
ModelLinearRegressionQ2AI = lm(LifeExp ~ + Population + Murder + Frost + Income +Illiteracy + Area + HighSchoolGrad,data =inputStateCSV ) 
summary(ModelLinearRegressionQ2AI)


#Q2.a.ii)
ModelLinearRegressionQ2AII = lm(LifeExp ~ + Population + Murder + Frost + HighSchoolGrad,data =inputStateCSV ) 
summary(ModelLinearRegressionQ2AII)


#Q.2.b)

#Q.2.b.i)

stateDataSampleQ2BI = sample.split(inputStateCSV$LifeExp,SplitRatio = 0.6)
stateDataTestQ2BI = subset(inputStateCSV,stateDataSampleQ2BI==FALSE)
stateDataTrainQ2BI =subset(inputStateCSV,stateDataSampleQ2BI==TRUE)

treeQ2BIBucket10 = rpart(LifeExp ~ + Population + Murder + Frost + Income +Illiteracy + Area + HighSchoolGrad,method = "class",data = stateDataTrainQ2BI , minbucket = 1)
predictionQ2BIBucket10 = predict(treeQ2BIBucket10,newdata =stateDataTestQ2BI ,method = "class")
SSE = sum((stateDataTestQ2BI$LifeExp -predictionQ2BIBucket10)^2 )
SST = sum(( stateDataTestQ2BI$LifeExp- mean(stateDataTestQ2BI$LifeExp))^2)
valQ2BIBucket10 = (1-(SST/SSE))
print(valQ2BIBucket10)
prp(treeQ2BIBucket15)



#Q.2.b.ii)
treeQ2BIIBucket15 = rpart(LifeExp ~ + Population + Murder + Frost + Income +Illiteracy + Area + HighSchoolGrad,method = "class",data = stateDataTrainQ2BI , minbucket = 5)
predictionQ2BIIBucket15 = predict(treeQ2BIIBucket15,newdata =stateDataTestQ2BI ,method = "class")

SSE = sum((stateDataTestQ2BI$LifeExp -predictionQ2BIIBucket15)^2 )
SST = sum(( stateDataTestQ2BI$LifeExp- mean(stateDataTestQ2BI$LifeExp))^2)
valQ2BIIBucket15 = (1-(SST/SSE))
print(valQ2BIIBucket15)


#Q.2.b.iii)

treeQ2BIIIBucket15 = rpart(LifeExp ~ + Population + Murder   +Illiteracy  ,method = "class",data = stateDataTrainQ2BI , minbucket = 5)
predictionQ2BIIIBucket15 = predict(treeQ2BIIIBucket15,newdata =stateDataTestQ2BI ,method = "class")

SSE = sum((stateDataTestQ2BI$LifeExp -predictionQ2BIIIBucket15)^2 )
SST = sum(( stateDataTestQ2BI$LifeExp- mean(stateDataTestQ2BI$LifeExp))^2)
print((1-(SST/SSE)))
prp(treeQ2BIIIBucket15)


