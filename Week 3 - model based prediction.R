##############################
### Model based prediction ###
##############################

library(ggplot2)
library(caret)


data(iris)

names(iris)

table(iris$Species)


inTrain <- createDataPartition(y = iris$Species, p =0.7, list=FALSE)

training <- iris[inTrain,]
testing <- iris[-inTrain,]

dim(training)

dim(testing)

# Build linear discriminate analysis model

modlda<- train(Species ~., data =training, method ="lda")

# Build niave bayes

modnb <- train(Species~., data = training, method ="nb")

#predict lda model

plda <- predict(modlda, testing)

# Predict niave bayes
pnb <- predict(modnb, testing)

table(plda,pnb)


#### Compare results of the 2

equalPredictions = (plda==pnb)

qplot(Petal.Width, Sepal.Width, color = equalPredictions, data = testing)
