################################################################################
###
### Function to calculate predicted outcome probabilities for out-of-sample data
### from a fitted ensemble model
###
################################################################################

library("caret")
library("foreach")
library("iterators")

predict_from_ensemble <- function(dat, models, wts)
{
    ## Loop through each of the 10 imputed ensembles
    ans <- foreach (imp_models = models, imp_wts = wts, i = icount()) %do% {
        ## Calculate predicted probabilities from each individual model
        pred <- foreach (model = imp_models) %do% {
            pp <- predict(model, newdata = dat, type = "prob")

            ## Hack to normalize predicted probabilities from avNNet models --
            ## should be fixed in an upcoming version of caret
            ##
            ## See https://github.com/topepo/caret/issues/261
            if (model$method == "avNNet") {
                pp <- pp / rowSums(pp)
            }

            pp
        }

        ## Multiply each set of predicted probabilities by the corresponding
        ## ensemble weight
        wt <- imp_wts$weights$par
        wt <- c(wt, 1 - sum(wt))
        pred <- Map("*", pred, wt)

        ## Sum the weighted probabilities
        pred <- Reduce("+", pred)

        cat("Finished imputation", i, date(), "\n")

        pred
    }

    ## Average across the imputations
    ans <- Reduce("+", ans)
    ans <- ans / length(models)

    ans
}
