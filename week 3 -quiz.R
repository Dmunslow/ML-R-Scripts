#####################
### Quiz 3 Script ###
#####################



#### Question 1 #####

library(AppliedPredictiveModeling)
library(caret)

data(segmentationOriginal)

head(segmentationOriginal)

table(segmentationOriginal$Case)

training <- segmentationOriginal[segmentationOriginal$Case == "Train",]
testing <- segmentationOriginal[segmentationOriginal$Case == "Test",]

dim(training); dim(testing)

set.seed(125)

q1modFit <- train(Class ~., data = training, method = "rpart")

print(modFit$finalModel)

# Non fancy tree
plot(q1modFit$finalModel, uniform = T, main = "classification tree")
text(q1modFit$finalModel, use.n = T, all = T, cex =.8)

# Use rattle to print tree to answer question
library(rattle)

fancyRpartPlot(q1modFit$finalModel)


###### Question 3 ##########

library(pgmm)
data(olive)

olive = olive[,-1]

head(olive)

q2modFit <- train(Area ~., data = olive, method ="rpart")

newdata = as.data.frame(t(colMeans(olive)))

predict(q2modFit, newdata = newdata)


########### Question 4 ################

library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

names(SAheart)

set.seed(13234)

q3modFit <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl,
                  data = trainSA, method = "glm", family="binomial")

# Function to calculate misclassification rate
missClass = function(values,prediction){
            
    sum(((prediction > 0.5)*1) != values)/length(values)
}

missClass(testSA$chd, predict(q3modFit, testSA))

missClass(trainSA$chd, predict(q3modFit, trainSA))



################## Question 5 ######################
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

vowel.test$y <- as.factor(vowel.test$y)
vowel.train$y <- as.factor(vowel.train$y)

set.seed(33833)

library(randomForest)
q4modFit <- randomForest(y~., data = vowel.train)

order(varImp(q4modFit), decreasing = T)
