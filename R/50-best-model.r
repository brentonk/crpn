## Choose the best ML alternative to the capability ratio according to the
## cross-validation criterion, namely maximal Kappa score

library("caret")

source("40-load-results.r")

## Identify model with highest maximal kappa from a list of objects of class
## "train"
highestKappa <- function(x)
{
    each_max <- sapply(x, function(z) max(z$results[, "Kappa"]))
    ans <- max(each_max)
    names(ans) <- names(x)[which.max(each_max)]
    ans
}

## Best model for full sample classification
highestKappa(list(rf = fit_rf,
                  rf_time = fit_rf_time,
                  svm = fit_svm,
                  svm_time = fit_svm_time)) # rf_time, Kappa = .35

## Best model for victory subsample classification
highestKappa(list(rf = fit_rf_victory,
                  rf_time = fit_rf_victory_time,
                  svm = fit_svm_victory,
                  svm_time = fit_svm_victory_time)) # rf_time, Kappa = .17
