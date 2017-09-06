##########################
### Covariate Creation ###
##########################


# Two levels of covariate creation

# Level 1 - RAW data to covariate

# Level 2 - Transforming tidy covariates

library(kernlab)
data(spam)

spam$capitalAvESq <- spam$capitalAve^2


library(ISLR)
library(caret)
data(Wage)

inTrain <- createDataPartition( y = Wage$wage, p =0.7, list = FALSE)

training <- Wage[inTrain,]
testing <- Wage[-inTrain,]


table(training$jobclass)

# dummyVars is a caret package
# 2 job classes - industrial and Information
dummies <- dummyVars(wage ~ jobclass, data = training)
head(predict(dummies, newdata=training))


#removing zero coviariates

nsv <- nearZeroVar(training, saveMetrics = T)

nsv

#Spline Basis

library(splines)

# Creates a polynomial variable
# 3rd degree polynomial for the age variable
bsBasis <- bs(training$age, df =3)

# Colomns = age, age^2, age^3
bsBasis

# Pass it all the predictors with bsBasis dataframe
lm1 <- lm(wage ~bsBasis, data = training)

plot(training$age, training$wage, pch=19, cex=0.5)

points(training$age,predict(lm1,newdata=training), col="red", pch= 19, cex =0.5)

#Create a new set of variables using the test set, with the bsBasis function
predict(bsBasis, age = testing$age)