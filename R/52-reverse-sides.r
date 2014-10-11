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
idx_a <- which(names(work_train) %in% paste0("SideA_", cinc_components))
idx_b <- which(names(work_train) %in% paste0("SideB_", cinc_components))
work_train_rev <- work_train
names(work_train_rev)[idx_a] <- str_replace(names(work_train_rev)[idx_a],
                                            "SideA",
                                            "SideB")
names(work_train_rev)[idx_b] <- str_replace(names(work_train_rev)[idx_b],
                                            "SideB",
                                            "SideA")

## Calculate predicted probabilities
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
