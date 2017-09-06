##################################
### Predicting with Regression ###
##################################

library(caret)

data(faithful)

set.seed(333)

inTrain <-createDataPartition(y =faithful$waiting, p =0.5, list = FALSE)


trainFaith <- faithful[inTrain,]
testFaith <- faithful[-inTrain,]

head(trainFaith)

plot(trainFaith$waiting, trainFaith$eruptions, pch = 19, col ="blue", 
     xlab ="waiting", ylab ="Duration")


# Fit linear model

lm1 <- lm(eruptions ~ waiting, data = trainFaith)

summary(lm1)

# Add line of fitted values to the plot
lines(trainFaith$waiting, lm1$fitted.values, lwd=3)

# coef extracts coefficient value for each - below, predict eruption duration
# for a waiting time of 80
coef(lm1)[1] + coef(lm1)[2]*80

# Does the same as above
newdata <- data.frame(waiting=80)
predict(lm1,newdata)


# Plot predictions, training and test
par(mfrow=c(1,2))
plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col="blue",
     xlab="Waiting",ylab="Duration")
lines(trainFaith$waiting,predict(lm1),lwd=3)
plot(testFaith$waiting,testFaith$eruptions,pch=19,col="blue",
     xlab="Waiting",ylab="Duration")
lines(testFaith$waiting,predict(lm1,newdata=testFaith),lwd=3)

### Get training/test set errors

# RMSE for training set
sqrt(sum((lm1$fitted.values-trainFaith$eruptions)^2))

# RMSE for test set
sqrt(sum((predict(lm1, newdata=testFaith)-testFaith$eruptions)^2))


### Prediction Intervals

pred1 <- predict(lm1, newdata=testFaith, interval ="prediction")
ord <- order(testFaith$waiting)
plot(testFaith$waiting, testFaith$eruptions, pch=19,col="blue")
matlines(testFaith$waiting[ord], pred1[ord,],type="l",,col=c(1,2,2), lty= c(1,1,1), lwd =3)


# do the same thing with caret

modFit <- train(eruptions ~ waiting, data =trainFaith, method = "lm")

summary(modFit$finalModel)
