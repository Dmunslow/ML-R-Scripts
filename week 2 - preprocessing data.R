################################
### Preprocessing Predictors ###
################################


# often more useful for model based approaces

library(caret)
library(kernlab)

data(spam)

# 75% of data for training, the rest for testing
inTrain <- createDataPartition(y = spam$type, p = 0.75, list = FALSE)

training <- spam[inTrain,]
testing <- spam[-inTrain,]

# we see that capitalAve is very skewed -which makes it very hard to deal
# with in model based approaches
hist(training$capitalAve, main = "", xlab = "ave. capital run length")

mean(training$capitalAve)

sd(training$capitalAve)

# we see that mean is 4 but SD is 25


# Can be useful to standardize the variable by subtracting mean and dividing 
# by the standard deviation

trainCapAve <- training$capitalAve
trainCapAveS <- (trainCapAve - mean(trainCapAve))/sd(trainCapAve)


# When we do this, mean is 0 and SD is 1
mean(trainCapAveS)

sd(trainCapAveS)


# When we run the algorithm on the test set, we need to use the data from the 
# train set- the sd and mean of the TRAIN set, and transform test set using 
# those values

testCapAve <- testing$capitalAve

testCapAveS <- (testCapAve - mean(trainCapAve))/sd(trainCapAve)

# As a result, mean will not be exactly 0, and SD will not be exactly 1

mean(testCapAveS)
sd(testCapAveS)



########## Caret preProcess Function ######################

# Pass the whole data frame except 58th object, which is the outcome
# Tell it to center and scale every variable
preObj <- preProcess(training[,-58],method =c("center","scale"))
trainCapAveS <- predict(preObj, training[,-58])$capitalAve

mean(trainCapAveS)

sd(trainCapAveS)

# we see it gives same result as transformations above

# We can use the same object created above to apply transformation to 
# the test set
testCapAveS <- predict(preObj, testing[,-58])$capitalAve

mean(testCapAveS)
sd(testCapAveS)


################# We can also pass preprocess commands to train function

set.seed(32343)

modelFit <- train(type ~., data = training, preProcess=c("center","scale"),
                  method = "glm")

modelFit



################ Box-Cox transformations

# Centering and Scaling will help remove very strongly biased predictors
# or predictors that have super high variance

### Box-Cox
### Take continuous data and and try to make it look like normal data
### does this using set of preprocess parameters based on MLE

preObj <- preProcess(training[,-58], method =c("BoxCox"))

trainCapAveS <- predict(preObj, training[,-58])$capitalAve

par(mfrow=c(1,2))
hist(trainCapAveS)
qqnorm(trainCapAveS)

## We see that while it has helped to make this data more normal, it still has
## bunched data at 0, this is because it is a continuous transformation and it
## will not help deal with repeated values (in this case of 0)

###################### Imputing Data ####################################

# Prediction algorithms generally are not built to handle missing data
# as a result most of them will fail if its in the data set

library(RANN)
set.seed(13343)

# Make some values NA
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1], size =1, prob =0.05)==1
training$capAve[selectNA] <- NA

# Impute and Standardize
# using k-nearest-neighbors imputation - finds the k nearest data vectors 
# that look most like the data vector with the missing value and average the 
# value of the variable thats missing and impute it
preObj <- preProcess(training[,-58], method = "knnImpute")
capAve <- predict(preObj, training[,-58])$capAve

# Standardize true values
capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth -mean(capAveTruth))/sd(capAveTruth)


## compare the actual values vs imputed values

# Imputed vs Real values - all very close
quantile(capAve - capAveTruth)

# Look at just imputed vs real values
quantile((capAve - capAveTruth)[selectNA])

# Look at ones that were not selected to be NA
quantile((capAve - capAveTruth)[!selectNA])