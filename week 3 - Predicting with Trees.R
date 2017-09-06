#############################
### Predicting with Trees ###
#############################

library(ggplot2)
library(caret)

data("iris")

names(iris)

table(iris$Species)


inTrain <- createDataPartition(y = iris$Species, p=0.7, list = FALSE)

training <- iris[inTrain,]

testing <- iris[-inTrain,]

dim(training)

dim(testing)

qplot(Petal.Width, Sepal.Width, color = Species, data = training)

# train function - method rpart - regression and classification tree package
modFit <- train(Species ~., method = "rpart", data = training)

# see all the nodes and how they are split
# Probabilites for being in each 
print(modFit$finalModel)


# can plot classification tree

plot(modFit$finalModel, uniform = T, main = "classification tree")
text(modFit$finalModel, use.n = T, all = T, cex =.8)

# can make a much prettier version with rattle

library(rattle)

# Much easier to see whats going on
fancyRpartPlot(modFit$finalModel)

# Predicting spits out classes
predict(modFit, newdata = testing)
