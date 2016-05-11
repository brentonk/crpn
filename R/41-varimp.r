################################################################################
###
### Assess variable importance by removing terms one-by-one and rerunning the
### model from start to finish
###
### Only need to do the weight training, not the actual model training, since
### the weight training is what gives us our bias-adjusted estimate of
### out-of-sample log loss for the super learner
###
################################################################################

sessionInfo()

## Load plyr before dplyr -- forcing this here since some of the ML packages
## call plyr
library("plyr")
library("dplyr")
library("tidyr")

## Analysis packages
library("foreach")
library("methods")

source("10-fn-train.r")
source("11-defs-train.r")
source("40-fn-varimp.r")

load("results-imputations-train.rda")


###-----------------------------------------------------------------------------
### Read in the command line argument (job array number) that tells us which of
### the variable combinations and imputations to run for
###
### Assumes a call like
###   Rscript 41-varimp.r i
### where 0 <= i <= (number of variable combos) x (number of imputations) - 1
###-----------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
    i <- as.integer(args[1])
} else {
    stop("No imputation specified at command line")
}

n_imp <- length(imputations_train)
combo <- i %/% n_imp + 1
imputation <- i %% n_imp + 1

cat("Combo:", combo, "\n")
cat("Imputation:", imputation, "\n")

## Set up vector of variable names to drop
variable <- component_combos[combo, "variable"]
to_drop <- component_combos[combo, "to_drop"]

if (to_drop != "both") {
    x_drop <- paste0(variable, "_", to_drop)
} else {
    x_drop <- paste0(variable, "_", c("a", "b"))
}

cat("Dropping:", paste(x_drop, collapse = ", "), "\n")


###-----------------------------------------------------------------------------
### Modify the training arguments:
###   * Drop the specified terms from the formula
###   * Adjust the tuning parameters appropriately for rf models, since the ones
###   set originally are dependent on the number of terms
###-----------------------------------------------------------------------------

method_args_drop <- foreach (arg = method_args) %do% {
    arg$form <- drop_all_terms(arg$form, x_drop)

    if (arg$method == "rf")
        arg$tuneGrid <- data.frame(mtry = seq(2, 10, by = 2))

    arg
}
names(method_args_drop) <- names(method_args)


###-----------------------------------------------------------------------------
### Calculate out-of-fold predictions and corresponding ensemble weights for the
### models with the given term(s) dropped, using the same methods as in
### 15-train-weights.r
###-----------------------------------------------------------------------------

dat <- imputations_train[[imputation]]

time_start <- proc.time()

## We want the seed to be deterministic (for replicability) but to differ across
## runs
set.seed(11 * i)

trained_weights <- train_weights(dat = dat,
                                 n_folds = 10,
                                 arg_list = method_args_drop,
                                 common_args = common_args,
                                 tr_control = tr_control)

## Save corresponding variable combination and imputation to make these easier
## to back out when we collect the results
trained_weights$combo <- combo
trained_weights$imputation <- imputation

time_end <- proc.time()
print(time_end - time_start)

save_dir <- "results-varimp"
save_file <- paste0("combo",
                    sprintf("%.2d", combo),
                    "-imp",
                    sprintf("%.2d", imputation),
                    ".rda")

if (!dir.exists(save_dir))
    dir.create(save_dir)
save(trained_weights,
     file = file.path(save_dir, save_file))
