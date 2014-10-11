## Interface between `polr()` and the `caret` package
##
## See documentation at <http://topepo.github.io/caret/custom_models.html>

orderedResponse <- list()

## Required packages
orderedResponse$library <- c("MASS")

## Tasks the model performs (classification, regression, or both)
##
## Since `multinom` is listed as only performing classification, not
## regression, I assume the same is true for `polr`
orderedResponse$type <- c("Classification")

## Tuning parameters
##
## None here, so use the same as for `glm`, see
##
##     getModelInfo("glm", FALSE)$glm
## 
orderedResponse$parameters <- data.frame(parameter = "parameter",
                                         class = "character",
                                         label = "parameter")

## Function to make grid of tuning parameters
##
## Again using same as `glm`
orderedResponse$grid <- function(x, y, len = NULL)
    data.frame(parameter = "none")

## Function to fit the model
##
## Based on `caret`'s fit function for `glm`
orderedResponse$fit <- function(x, y, wts, param, lev, last, classProbs, ...)
{
    modelArgs <- list(...)

    ## Set up data
    dat <- if (is.data.frame(x)) x else as.data.frame(x)
    dat$.outcome <- y
    modelArgs <- c(list(formula = .outcome ~ .,
                        data = dat),
                   modelArgs)

    ## Always force Hessian calculation
    modelArgs$Hess <- TRUE

    ## Pass in model weights, if any
    if (!is.null(wts))
        modelArgs$weights <- wts

    ## Fit model
    ans <- do.call("polr", modelArgs)

    ## Strip out the call to avoid unnecessary data repetition
    ans$call <- NULL

    ans
}

## Class predictions from output
orderedResponse$predict <- function(modelFit,
                                    newdata,
                                    preProc = NULL,
                                    submodels = NULL)
    predict(modelFit, newdata = newdata, type = "class")

## Class probabilities from output
orderedResponse$prob <- function(modelFit,
                                 newdata,
                                 preProc = NULL,
                                 submodels = NULL)
    predict(modelFit, newdata = newdata, type = "probs")

## Sort tuning parameters
##
## Again using same as `glm`
orderedResponse$sort <- function(x)
    x

## Name of model
orderedResponse$label <- "Ordered Logistic or Probit Regression"

## Tags (see <http://topepo.github.io/caret/bytag.html>)
orderedResponse$tags <- c("Logistic Regression", "Linear Classifier")

## Names of predictors
##
## Same as `glm`
orderedResponse$predictors <- function(x, ...)
    predictors(terms(x))

## Variable importance metric
##
## Same concept as `glm`: highest z-statistics
orderedResponse$varImp <- function(object, ...)
{
    ## Extract coefficients (without cutpoints)
    cf <- coef(object)
    ncf <- length(cf)

    ## Calculate standard errors of coefficients and z-statistics
    se <- sqrt(diag(vcov(object)))
    se <- se[seq_len(ncf)]
    z <- cf / se

    ## Organize output as in the method for `glm`
    out <- data.frame(Overall = abs(z))
    if (!is.null(names(cf)))
        rownames(out) <- names(cf)

    out
}
