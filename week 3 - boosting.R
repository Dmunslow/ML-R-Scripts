################
### Boosting ###
################



# adaboost most famous boosting algo

# Most of the boosting packages for R are contained in caret

library(ISLR)
library(ggplot2)
library(caret)

data(Wage)


Wage <- subset(Wage, select = -c(logwage))

inTrain <- createDataPartition(y=Wage$wage, p= 0.7, list = F)

training <- Wage[inTrain,]
testing <- Wage[-inTrain,]


# Model wage as a function of all variables, gbm uses boosting with trees
# gbm creates a lot of output if you dont use gbm = false
modFit <- train(wage~., method ="gbm", data =training, verbose=FALSE)
print(modFit)

# Plot prediction of wage against actual wage
qplot(predict(modFit,testing),wage,data=testing)