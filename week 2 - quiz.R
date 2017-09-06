##############
### Quiz 2 ###
##############

# Question 1

library(AppliedPredictiveModeling)
data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

dim(training)
dim(testing)
## Question 2



library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

qplot(seq_along(mixtures$CompressiveStrength), CompressiveStrength, data = mixtures, color =)

qplot(seq_along(mixtures$CompressiveStrength), CompressiveStrength, data = mixtures, color = cut2(mixtures$BlastFurnaceSlag, g = 4))

qplot(seq_along(mixtures$CompressiveStrength), CompressiveStrength, data = mixtures, color = cut2(mixtures$FlyAsh))

qplot(seq_along(mixtures$CompressiveStrength), CompressiveStrength, data = mixtures, color = cut2(mixtures$Water, g = 4))

qplot(seq_along(mixtures$CompressiveStrength), CompressiveStrength, data = mixtures, color = cut2(mixtures$Superplasticizer, g = 4))

qplot(seq_along(mixtures$CompressiveStrength), CompressiveStrength, data = mixtures, color = cut2(mixtures$CoarseAggregate, g = 4))

qplot(seq_along(mixtures$CompressiveStrength), CompressiveStrength, data = mixtures, color = cut2(mixtures$FineAggregate, g = 4))

qplot(seq_along(mixtures$CompressiveStrength), CompressiveStrength, data = mixtures, color = cut2(mixtures$Age))

head(mixtures)


### Question 3

library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

qplot(Superplasticizer, data = mixtures)



#### Question 4

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

head(adData)

trainSub <- training[,grepl("^IL",names(training))]

head(trainSub)


preProcQ4 <- preProcess(trainSub,method ="pca", thresh = 0.8 )


##### Question 5

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

# Isolate Variables starting with IL
trainSub <- training[,grepl("^IL",names(training)) | grepl("diagnosis", names(training))]
testSub <- testing[,grepl("^IL",names(testing)) | grepl("diagnosis", names(testing))]


prproc <- preProcess(trainSub[,-1], method="pca", thresh = 0.8)

trainPC <- predict(prproc, trainSub[,-1])

modelFit <- train(x = trainPC, y = training$diagnosis, method ="glm")




modelReg <-train(diagnosis ~., method ="glm", data =trainSub)


testPC <- predict(prproc, testSub[,-1])
confusionMatrix(testing$diagnosis, predict(modelFit,testPC))
confusionMatrix(testing$diagnosis, predict(modelReg,testSub))




