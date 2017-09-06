#######################
### Random Forrests ###
#######################


# 1. Bootstrap samples
# 2. At each split, bootstrap variables
# 3. Grow multiple trees and vote

### Pros - Accuracy

### Cons - Speed, interpretability, overfitting

library(ggplot2)
library(caret)
data(iris)

inTrain <- createDataPartition(y = iris$Species, p =0.7, list = F)

training <- iris[inTrain,]

testing <- iris[-inTrain,]

# Fit random forrest model with caret
modFit <- train(Species~., data = training, method ="rf", prox = T)
modFit

# Look at specific tree
getTree(modFit$finalModel, k =2)

# Can use centers to find the centers of the class predictions

# Class centers
irisP <- classCenter(training[,c(3,4)], training$Species, modFit$finalModel$prox)
irisP <- as.data.frame(irisP)
irisP$Species <- rownames(irisP)
p <- qplot(Petal.Width, Petal.Length, col = Species, data =training)
p + geom_point(aes(x=Petal.Width, y=Petal.Length, col=Species), size=5, shape=4, data=irisP)



##### Predicting new values

pred <- predict(modFit, testing)

## can use this to see which values the predictions misses
testing$predRight <- pred ==testing$Species
table(pred, testing$Species)

# see which predictions were missed
qplot(Petal.Width, Petal.Length, colour = predRight, data = testing, main = "newData Predictions")
