###
### How close are the predicted probabilities if we switch Side A and Side B
### and refit the model to the training data?
###

library("caret")
library("randomForest")
library("stringr")

source("20-split-data.r")
source("26-full-formula.r")
source("40-load-results.r")

## Construct "reversed" data
reverse_sides <- function(dat)
{
    ## Column indices of side-specific variables
    idx_a <- str_detect(names(dat), fixed("SideA_"))
    idx_b <- str_detect(names(dat), fixed("SideB_"))

    ## Switch 'em
    names(dat)[idx_a] <- str_replace(names(dat)[idx_a],
                                     "SideA",
                                     "SideB")
    names(dat)[idx_b] <- str_replace(names(dat)[idx_b],
                                     "SideB",
                                     "SideA")

    dat
}

work_train_rev <- reverse_sides(work_train)
work_test_rev <- reverse_sides(work_test)

## Calculate predicted probabilities for training data
pp_orig <- predict(fit_rf_time,
                   newdata = work_train,
                   type = "prob")
pp_rev <- predict(fit_rf_time,
                  newdata = work_train_rev,
                  type = "prob")
pp_rev <- pp_rev[, c(3, 2, 1)]

## Compare predicted probabilities (median absolute difference)
pp_diff <- abs(pp_orig - pp_rev)
apply(pp_diff, 2, median)

## Same for test data
pp_test_orig <- predict(fit_rf_time,
                        newdata = work_test,
                        type = "prob")
pp_test_rev <- predict(fit_rf_time,
                       newdata = work_test_rev,
                       type = "prob")
pp_test_rev <- pp_test_rev[, c(3, 2, 1)]

pp_test_diff <- abs(pp_test_orig - pp_test_rev)
apply(pp_test_diff, 2, median)
