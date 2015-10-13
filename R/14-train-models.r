################################################################################
###
### Fit each candidate model to the full training data
###
################################################################################

sessionInfo()

## Local parallelization
library("doMC")
registerDoMC(min(5, parallel::detectCores() - 1))

## Load plyr before dplyr -- forcing this here since some of the ML packages
## call plyr
library("plyr")
library("dplyr")

## Analysis packages
library("caret")
library("foreach")
library("doRNG")
library("iterators")
library("methods")

source("10-fn-train.r")
source("11-defs-train.r")

load("results-imputations-train.rda")

## Destination for log files across separate nodes
if (!dir.exists("logs-models"))
    dir.create("logs-models")


time_start <- proc.time()

set.seed(2233)                          # For exact replicability

trained_models <- foreach (dat = imputations_train, i = icount(), .packages = c("caret", "tidyr", "dplyr")) %dorng% {
    ## Separate log for each imputation
    logfile <- paste0("logs-models/",
                      "imp",
                      sprintf("%.2d", i),
                      ".log")

    ## Blank old log files
    cat("", file = logfile, append = FALSE)

    ## Train each model on the full imputed dataset
    imputation_models <- args_to_train(arg_list = method_args,
                                       common_args = common_args,
                                       tr_control = tr_control,
                                       data_train = dat,
                                       data_test = NULL,
                                       for_probs = FALSE,
                                       allow_no_tune = FALSE,
                                       logfile = logfile)

    imputation_models
}

time_end <- proc.time()
print(time_end - time_start)

save(trained_models,
     file = "results-trained-models.rda")
