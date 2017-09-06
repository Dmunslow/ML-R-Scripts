##############################
### Regularized Regression ###
##############################

library(ElemStatLearn)

data(prostate)

str(prostate)

# Issue for high dimensional data:
## We will not be able to estimate all coefficients due to DF
small = prostate[1:5,]

lm(lpsa ~., data = small)

# Hard thresholding 

# Choose lambda coefficients to be non-zero


#### Regularized regression
