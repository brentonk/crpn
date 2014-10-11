## Run a random forest on the training data

library("caret")
library("randomForest")

if (Sys.info()["user"] %in% c("brenton", "kenkelb")) {
    library("doMC")
    registerDoMC(max(1, parallel:::detectCores() - 1))
}

source("20-split-data.r")
source("26-full-formula.r")
source("27-seeds.r")

## TO DO IN FUTURE: Use weighted kappa

## Fit 3-class models to full training data
set.seed(full_seed)
fit_rf <- train(
    full_formula,
    data = work_train,
    method = "rf",
    metric = "Kappa",
    trControl = trainControl(
        ## Normally we would use "oob" with random forests, but this way the
        ## measurement will be consistent with whatever we do with other
        ## methods
        method = "cv",
        number = 10,
        allowParallel = TRUE
    ),
    tuneLength = 11,        # no. predictors - 1
    ntree = 1000
)

set.seed(full_seed)
fit_rf_time <- train(
    full_time_formula,
    data = work_train,
    method = "rf",
    metric = "Kappa",
    trControl = trainControl(
        method = "cv",
        number = 10,
        allowParallel = TRUE
    ),
    tuneLength = 12,        # no. predictors - 1
    ntree = 1000
)

## Fit 2-class models to data with "victory" outcomes
set.seed(victory_seed)
fit_rf_victory <- train(
    victory_formula,
    data = work_train,
    subset = OutcomeCollapsed != "Stalemate",
    method = "rf",
    metric = "Kappa",
    trControl = trainControl(
        method = "cv",
        number = 10,
        allowParallel = TRUE
    ),
    tuneLength = 11,        # no. predictors - 1
    ntree = 1000
)

set.seed(victory_seed)
fit_rf_victory_time <- train(
    victory_time_formula,
    data = work_train,
    subset = OutcomeCollapsed != "Stalemate",
    method = "rf",
    metric = "Kappa",
    trControl = trainControl(
        method = "cv",
        number = 10,
        allowParallel = TRUE
    ),
    tuneLength = 12,        # no. predictors - 1
    ntree = 1000
)

## Save model results
save(fit_rf, fit_rf_time, fit_rf_victory, fit_rf_victory_time,
     file = "results-rf.RData")
