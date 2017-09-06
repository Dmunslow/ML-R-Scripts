###############################
### Data Slicing with Caret ###
###############################

library(caret)
library(kernlab)

data(spam)

# creates indicator variable for training set
inTrain <- createDataPartition(y = spam$type, p = 0.75, list = FALSE)

training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)


###### Cross-validation
set.seed(32323)

# Create set of folds, we tell it the outcome we want to split on (spam$type)
# list = TRUE, returns each indices of a particular fold as a list
# returnTrain
folds <- createFolds(y = spam$type, k = 10 ,  list = TRUE, returnTrain = TRUE)

# Check the length of each fold
sapply(folds, length)

# Which elements from the spam data set are in the first 10
# elements of the first fold
folds[[1]][1:10]

# We see that it's elements 1-10, so it has split the folds in order


#### We can instead return just the test set samples

# Set returnTrain to FALSE instead
folds <- createFolds(y = spam$type, k = 10 ,  list = TRUE, returnTrain = FALSE)

# Check the length of each fold
sapply(folds, length)

# Which elements from the spam data set are in the first 10
# elements of the first fold
folds[[1]][1:10]


#################### Resampling
set.seed(32323)

folds <-createResample(y = spam$type, times = 10, list = TRUE)

sapply(folds, length)

# because we are resampling we may get some of the same values back
folds[[1]][1:10]


############################# Time Slices
set.seed(32323)

tme <- 1:1000

# Create slices with about 20 items per sample, and then predict the next ~10
folds <- createTimeSlices(y = tme, initialWindow = 20, horizon = 10)

names(folds)

# creates folds in order
folds$train[[1]]


# next 10 samples 
folds$test[[1]]


















