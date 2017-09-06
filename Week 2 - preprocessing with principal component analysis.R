####################################
### Preprocessing with Principal ###
###     Components Analysis      ###
####################################

library(caret)
library(kernlab)
data(spam)


inTrain <- createDataPartition(y=spam$type,
                               p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

# Leave out 58th column outcome
# Calculate absolute value of correlation of all variables
M <- abs(cor(training[,-58]))
# Remove correlation with variables with themselves
diag(M) <- 0
# Display variables with correlation greater than 0.8
which(M >0.8, arr.ind = T)

# See which variables are correlated - confirm previous findings
names(spam)[c(34,32)]

# very very highly correlated - likely digits of a phonenumber
plot(spam[,34],spam[,32])

# Having both in the model might not be useful, may be better to have
# a weighted combination of the 2
    # Pick the combo that captures the most info possible

# We could rotate the plot

X <- 0.71*training$num415 + 0.71*training$num857
Y <- 0.71*training$num415 - 0.71*training$num857

plot(X,Y)

# we see that the majority of the information is captured by the X variable
# which is adding the two variables together - May want to use the sum of 
# the two variables as a predictor


#################### Singular Value Decomposition ##############################

# Isolate 2 values which were highly correlated
smallSpam <- spam[,c(34,32)]

# Do principal components in R - prcomp
# does the SVD
prComp <- prcomp(smallSpam)

# Plot 1st principal component vs 2nd principal component
plot(prComp$x[,1],prComp$x[,2])

# Look at rotation matrix - how its summing up the variables to get each
# of the principal compnets
prComp$rotation

# PCA on SPAM data

typeColor <- ((spam$type=="spam")*1 +1)

#Transformation log10 made to make the data look more normal - because
# some of the data is very skewed
prComp <- prcomp(log10(spam[,-58] +1))

#Calculate principal components of ALL variables
plot(prComp$x[,1], prComp$x[,2], col = typeColor,xlab ="PC1", ylab="PC2")

##### Can do a similar operation with the caret package

# Tell it the number of Pricipal components to calculate
preProc <- preProcess(log10(spam[,-58]+1),method="pca",pcaComp=2)


# The principal components are essentailly two new variables
# They are essentially a model that you fit to the data
spamPC <- predict(preProc,log10(spam[,-58]+1))

plot(spamPC[,1],spamPC[,2],col=typeColor)

### Pre processing with caret

#Create training predictions
preProc <- preProcess(log10(training[,-58]+1),method="pca",pcaComp=2)
trainPC <- predict(preProc,log10(training[,-58]+1))

# Fit the model that relates the training variable to the principal components
modelFit <- train(x = trainPC, y = training$type,method="glm")

# In the test data set, you have to use the same principal components that
# you used in the training set, for the test set

testPC <- predict(preProc, log10(testing[,-58]+1))
confusionMatrix(testing$type, predict(modelFit,testPC))





