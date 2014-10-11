## Run support vector machines on the training data

library("caret")
library("kernlab")

if (Sys.info()["user"] %in% c("brenton", "kenkelb")) {
    library("doMC")
    registerDoMC(max(1, parallel:::detectCores() - 1))
}

source("20-split-data.r")
source("26-full-formula.r")
source("27-seeds.r")

## Fit 3-class models to full training data
set.seed(full_seed)
fit_svm <- train(
    full_formula,
    data = work_train,
    method = "svmRadial",
    metric = "Kappa",
    preProc = c("center", "scale"),
    trControl = trainControl(
        method = "cv",
        number = 10,
        allowParallel = FALSE,          # Parallelizing gives me a segfault :(
        classProbs = TRUE
    ),
    tuneLength = 15
)

set.seed(full_seed)
fit_svm_time <- train(
    full_time_formula,
    data = work_train,
    method = "svmRadial",
    metric = "Kappa",
    preProc = c("center", "scale"),
    trControl = trainControl(
        method = "cv",
        number = 10,
        allowParallel = FALSE,
        classProbs = TRUE
    ),
    tuneLength = 15
)

## Fit 2-class models to data with "victory" outcomes
set.seed(victory_seed)
fit_svm_victory <- train(
    victory_formula,
    data = work_train,
    subset = OutcomeCollapsed != "Stalemate",
    method = "svmRadial",
    metric = "Kappa",
    preProc = c("center", "scale"),
    trControl = trainControl(
        method = "cv",
        number = 10,
        allowParallel = FALSE,
        classProbs = TRUE
    ),
    tuneLength = 15
)

set.seed(victory_seed)
fit_svm_victory_time <- train(
    victory_time_formula,
    data = work_train,
    subset = OutcomeCollapsed != "Stalemate",
    method = "svmRadial",
    metric = "Kappa",
    preProc = c("center", "scale"),
    trControl = trainControl(
        method = "cv",
        number = 10,
        allowParallel = FALSE,
        classProbs = TRUE
    ),
    tuneLength = 15
)

save(fit_svm, fit_svm_time, fit_svm_victory, fit_svm_victory_time,
     file = "results-svm.RData")
