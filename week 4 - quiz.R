###############
#### Quiz 4 ###
###############

library(AppliedPredictiveModeling)
library(caret)
library(ElemStatLearn)
library(pgmm)
library(rpart)
library(gbm)
library(lubridate)
library(forecast)
library(e1071)


########### Question 1 ##################

## ELEMSTATLEARN package
data(vowel.train)

data(vowel.test)

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

set.seed(33833)

str(vowel.train)

q1mod1 <- train(y~., method = "rf",data = vowel.train, list = F)

q1mod2 <- train(y~., method = "gbm",data = vowel.train, verbose = F)

# Predict values for both models
mod1Pred <- predict(q1mod1, vowel.test)
mod2Pred <- predict(q1mod2, vowel.test)

# Show accuracy
confusionMatrix(table(mod1Pred, vowel.test$y))
confusionMatrix(table(mod2Pred, vowel.test$y))

# subset test set to observations where models agree on prediction
testAgreement <- vowel.test[mod2Pred == mod1Pred,]

# predict values for subsetted test set
agreePred <- predict(q1mod2, testAgreement)

# test accuracy
confusionMatrix(table(agreePred, testAgreement$y))

############# Question 2 ######################
set.seed(3433)

data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)

inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]

training = adData[ inTrain,]

testing = adData[-inTrain,]

set.seed(62433)

### Fit models
q2mod1 <- train(diagnosis~., method = "rf", data = training, list = F)

q2mod2 <- train(diagnosis~., method = "gbm", data = training, verbose = F)

q2mod3 <- train(diagnosis~., method = "lda", data = training)

### Predict with models

q2mod1Pred <- predict(q2mod1, testing)

q2mod2Pred <- predict(q2mod2, testing)

q2mod3Pred <- predict(q2mod3, testing)

#### combine data frames

stackedData <- data.frame(q2mod1Pred, q2mod2Pred, q2mod3Pred, diagnosis = testing$diagnosis)

head(stackedData)

### Ramdom forrest model fit new predictors

stackedMod <- train(diagnosis~., method = "rf", data = stackedData, list = F)


stackPred <- predict(stackedMod, testing)

# Test accuracy of model
confusionMatrix(table(stackPred, testing$diagnosis))

## Compare to accuracy of other models 

# RF
confusionMatrix(table(q2mod1Pred, testing$diagnosis))

# GBM
confusionMatrix(table(q2mod2Pred, testing$diagnosis))

# LDA
confusionMatrix(table(q2mod3Pred, testing$diagnosis))


##################### Question 3 ##################################

set.seed(3523)

library(AppliedPredictiveModeling)

data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]

training = concrete[ inTrain,]

testing = concrete[-inTrain,]

set.seed(233)

head(concrete)
## Fit lasso model

q3mod <- train(CompressiveStrength~., method = "lasso", data = training)


q3enet <- enet(as.matrix(concrete[,-9]), concrete$CompressiveStrength)

plot(q3enet)

###### Question 4 ########################################

fileUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv"

download.file(fileUrl, "./quiz4.csv")

head(q4data)

library(lubridate) # For year() function below

dat = read.csv("./quiz4.csv", header = T)

dat <- dat[,-1]

training = dat[year(dat$date) < 2012,]

testing = dat[(year(dat$date)) > 2011,]

tstrain = ts(training$visitsTumblr)

tstest = ts(testing$visitsTumblr)

tstest

plot(tstrain)

q4mod <- bats(tstrain)

q4fcast <- forecast(q4mod, h = dim(testing)[1], level = 95)

sum(q4fcast$lower < testing$visitsTumblr & testing$visitsTumblr < q4fcast$upper)/
    length(testing$visitsTumblr)

################# Question 5 #######################################

set.seed(3523)

library(AppliedPredictiveModeling)

data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]

training = concrete[ inTrain,]

testing = concrete[-inTrain,]

set.seed(325)

q5mod <- svm(CompressiveStrength ~., data = training)


q5predictions <- predict(q5mod, testing)

#RMSE equation
sqrt(mean( (testing$CompressiveStrength - q5predictions)^2))
