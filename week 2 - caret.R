#################
####  Caret  ####
#################

library(caret)
library(kernlab)

data(spam)

# 75% of data for training, the rest for testing
inTrain <- createDataPartition(y = spam$type, p = 0.75, list = FALSE)

training <- spam[inTrain,]
testing <- spam[-inTrain,]

dim(training)


set.seed(32343)

# Trying to predict type, "." denotes all features in data
modelFit <- train(type ~., data = training, method ="glm")


modelFit

# Look at model it fitted
modelFit$finalModel

# now we can predict on the test data

predictions <- predict(modelFit, newdata = testing)
predictions

# We can test these predictions using confusionMatrix
# pass it the list of predictions, the actual outcome on the test matrix
confusionMatrix(predictions, testing$type)


