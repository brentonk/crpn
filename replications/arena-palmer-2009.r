################################################################################
###
### Replication of Arena and Palmer 2009, "Politics or the Economy?", replacing
### CINC ratios with DOE scores
###
### Replicating Table 3
###
################################################################################

library("caret")
library("dplyr")
library("foreach")
library("foreign")
library("Formula")
library("glmx")

raw_arena_palmer_2009 <- read.dta("arena-palmer-2009.dta")
doe_dir_dyad <- read.csv("../R/results-predict-dir-dyad.csv")

## Merge in DOE scores
data_arena_palmer_2009 <- left_join(raw_arena_palmer_2009,
                                    doe_dir_dyad,
                                    by = c(ccode1 = "ccode_a",
                                           ccode2 = "ccode_b",
                                           year = "year"))
stopifnot(with(data_arena_palmer_2009, sum(is.na(VictoryA)) == 0))

## Set up model formulas for mean and variance
f_mean <-
    init ~ gov + d_un + d_inf + d_gro + du_gov + di_gov + dg_gov + cap_1 + ipeace
f_var <-
    ~ gov + d_inf + di_gov + d_un + du_gov + d_gro + dg_gov + cap_1
f_arena_palmer_2009 <- as.Formula(f_mean, f_var)

## Reproduce original model
cr_arena_palmer_2009 <- hetglm(
   f_arena_palmer_2009,
   data = data_arena_palmer_2009
)

summary(cr_arena_palmer_2009)

## Replicate with DOE scores
doe_arena_palmer_2009 <- hetglm(
    update(f_arena_palmer_2009,
           . ~ . - cap_1 + VictoryA + VictoryB | . - cap_1 + VictoryA + VictoryB),
    data = data_arena_palmer_2009
)

summary(doe_arena_palmer_2009)

## Cross-validate each
cv_hetglm <- function(model, folds, repeats) {
    ## Create folds
    fold_id <- createMultiFolds(y = model$y, k = folds, times = repeats)

    ## Calculate CV error for each fold
    cv_loss <- foreach (idx = fold_id, .combine = "c") %do% {
        ## Split sample according to fold IDs
        dat_in <- model$model[idx, , drop = FALSE]
        dat_out <- model$model[-idx, , drop = FALSE]
        y_out <- model$y[-idx]

        ## Refit model to fold
        fit <- update(model, data = dat_in)

        ## Make out-of-fold predictions and calculate log loss
        pp <- predict(fit, newdata = dat_out, type = "response")
        pp <- ifelse(y_out == 1, pp, 1 - pp)
        -1 * mean(log(pp))
    }

    ## Arrange into data frame, like the resample element of train() output
    data.frame(logLoss = cv_loss)
}

set.seed(109)
cr_cv <- cv_hetglm(cr_arena_palmer_2009, 10, 10)

set.seed(901)
doe_cv <- cv_hetglm(doe_arena_palmer_2009, 10, 10)

## Assemble output to be caret-like
cr_arena_palmer_2009 <- list(
    finalModel = cr_arena_palmer_2009,
    resample = cr_cv
)

doe_arena_palmer_2009 <- list(
    finalModel = doe_arena_palmer_2009,
    resample = doe_cv
)

save(data_arena_palmer_2009,
     cr_arena_palmer_2009,
     doe_arena_palmer_2009,
     file = "results-arena-palmer-2009.rda")
