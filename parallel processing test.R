################################
### Parallel processing test ###
################################

library(AppliedPredictiveModeling)
library(caret)
library(ElemStatLearn)
library(pgmm)
library(rpart)
library(gbm)
library(lubridate)
library(forecast)
library(e1071)
library(randomForest)

############################### Multi Thread code ############################
library(parallel)
library(doParallel)
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)
##############################################################################

### Example
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
set.seed(62433)

### Fit models single threaded

# 21.84
system.time( train(diagnosis~., method = "rf", data = training, list = F))

# 5.11
system.time(train(diagnosis~., method = "gbm", data = training, verbose = F))

# 0.75
system.time(train(diagnosis~., method = "lda", data = training))


### Fit models Multithread

# 18.95
system.time( train(diagnosis~., method = "rf", data = training, list = F, allowParallel = TRUE))

### Using Caret paRF
system.time( train(diagnosis~., method = "paRF", data = training, list = F))

# 3.13
system.time(train(diagnosis~., method = "gbm", data = training, verbose = F, allowParallel = TRUE))

# 0.65
system.time(train(diagnosis~., method = "lda", data = training, allowParallel = TRUE))

##### End parallel processing ########

stopCluster(cluster)
registerDoSEQ()

