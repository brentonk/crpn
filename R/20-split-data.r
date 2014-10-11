## Split data into training and test sets
##
## Training set:
##   o Used to learn model parameters
##   o When models have tuning parameters, we select those by further
##   subdividing the training set through k-fold cross-validation
##
## Test set:
##   o Used to estimate out-of-sample prediction error without bias
##   o No further tuning/training can be done on the basis of the test set
##   (unless we have another test of out-of-sample data available, and so on
##   and so on...)

source("10-load-data.r")

library("caret")

## `createDataPartition()` (probabilistically) ensures class balance across
## training and test sets
set.seed(1978)
split_work <- createDataPartition(y = work$OutcomeCollapsed,
                                  times = 1,
                                  p = 0.8,  # 80% of data in training
                                  list = TRUE)

work_train <- work[split_work[[1]], , drop = FALSE]
work_test <- work[-split_work[[1]], , drop = FALSE]
