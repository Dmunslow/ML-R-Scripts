################################
### Training control options ###
################################
library(caret)
library(kernlab)
data(spam)

inTrain <- createDataPartition(y = spam$type, p = 0.75, list = FALSE)

training <- spam[inTrain,]
testing <- spam[-inTrain,]

set.seed(32343)

modelFit <- train(type ~., data = training, method ="glm")



args(trainControl)

## Need to set the seed!!!!
## setting the seed will insure that the same random numbers again

## if you are resampling you can set seet for resamples with the seed argument
## in the train control function


########## Seed Example

set.seed(1235)

modelFit2 <- train(type~., data =training, method = "glm")

modelFit2