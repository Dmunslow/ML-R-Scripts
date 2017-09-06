########################################
### In Sample vs Out of Sample Error ###
########################################

library(kernlab)
data(spam)
set.seed(333)

# take 10 messages
# look at number of capital letters
smallSpam <- spam[sample(dim(spam)[1], size = 10),]
spamLabel <- (smallSpam$type =="spam")*1 +1
plot(smallSpam$capitalAve, col = spamLabel)


# We can create a algorithm that classifies based on number of capital letters
# and design it to perfectly capture the spam values --> overfit

# or we create more general rule that will miss some values in test set
# but apply better to data set as a whole
rule2 <- function(x){
  prediction <- rep(NA, length(x))
  prediction[x > 2.8] <- "spam"
  prediction[x <= 2.8] <- "nonspam"
  return(prediction)
}

#apply to test data set
table(rule2(smallSpam$capitalAve), smallSpam$type)

#apply to whole data set
table(rule2(spam$capitalAve), spam$type)

#see how many times our algorithm is right 

sum(rule2(spam$capitalAve)==spam$type)
