###############################
### Unsupervised Prediction ###
###############################


# iris example ignoring species labels

library(ggplot2)
data(iris)

inTrain <- createDataPartition(y = iris$Species, p =0.7, list = FALSE)

training <- iris[inTrain,]
testing <- iris[-inTrain,]

dim(training)

dim(testing)

## Cluster with kmeans - and plot

kMeans1 <- kmeans(subset(training, select=-c(Species)), centers = 3)

training$clusters <-as.factor(kMeans1$cluster)

qplot(Petal.Width, Petal.Length, color = clusters, data = training)

# we see 3 clusters

table(kMeans1$cluster, training$Species)

#### Build Predictor

modFit <- train(clusters ~., data = subset(training, select=-c(Species)), method = "rpart")

table(predict(modFit, training), training$Species)


### Now apply model to test data set

testClustPred <- predict(modFit, testing)

table(testClustPred, testing$Species)