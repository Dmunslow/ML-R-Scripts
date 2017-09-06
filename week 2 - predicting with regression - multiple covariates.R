##################################
### Predicting with Regression ###
###   Multiple Covariates      ###
##################################

library(ISLR)
library(ggplot2)
library(caret)

data(Wage)

Wage <- subset(Wage, Select = -c(logwage))

summary(Wage)

inTrain <- createDataPartition(y = Wage$wage, p =0.7, list =FALSE)

training <- Wage[inTrain,]
testing <- Wage[-inTrain,]

dim(training)

# Feature plot to see how variables are related

featurePlot(x = training[,c("age", "education", "jobclass")], 
            y = training$wage, plot = "pairs")

# We see an outlying set of points
qplot(age, wage, data=training)

# Most of outlying top class in information
qplot(age,wage, color = jobclass, data = training)

# Advanced degree also helps to explain outlying class
qplot(age, wage, color = education, data = training)


# Fit model

# Caret function again
modFit <- train(wage ~ age + jobclass + education, method = "lm", data = training)

finMod <- modFit$finalModel

print(modFit)


plot(finMod, 1, pch = 19, cex=0.5, col ="#00000010")

# color by variables not used in the model
qplot(finMod$fitted, finMod$residuals, color = race, data = training)


# Plot fitted residuals vs index(row)
plot(finMod$residuals, pch=19)


# Predicted versus truth in test set
pred <- predict(modFit, testing)
qplot(wage,pred, color =year, data = testing)

# Use all covariates

modFitAll <- train(wage ~., data =training, method = "lm")
pred <- predict(modFitAll, testing)
qplot(wage, pred, data = testing)