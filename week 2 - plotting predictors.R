###########################
### Plotting Predictors ###
###########################

library(ISLR)
library(ggplot2)
library(caret)

data(Wage)
summary(Wage)

### Get Training and test sets

inTrain <-createDataPartition(y = Wage$wage, p = 0.7, list = FALSE)

training <- Wage[inTrain,]
testing <- Wage[-inTrain,]

dim(training); dim(testing)

## Feature plot using caret package

featurePlot(x = training[,c("age", "education", "jobclass")],
            y = training$wage, plot = "pairs")

# qplot with GGplot

qplot(age, wage, data = training)

# we see that outlying chunk comes mostly from information jobclass
qplot(age, wage, color = jobclass, data = training)

qq <- qplot(age, wage, color = education, data = training)

qq + geom_smooth(method = 'lm', formula = y~x)


###### can be useful to cut data into sections

library(Hmisc)

# cuts the data up base on quantile groups
cutWage <- cut2(training$wage, g = 3)

table(cutWage)
# can now plot data based on these quantiles

p1 <- qplot(cutWage, age, data = training, fill = cutWage, geom = c("boxplot"))

p1

p2 <-qplot(cutWage, age, data = training, fill = cutWage, 
           geom = c("boxplot", "jitter"))

library(gridExtra)
grid.arrange(p1,p2,ncol=2)


### can use the factorized cut variables to look at tables of data

t1 <- table(cutWage, training$jobclass)

# table compares factor variable of cutwages and job class
t1

# gives proportions of each group
prop.table(t1,1)


#### Density plot

qplot(wage, color = education, data = training , geom = "density")

