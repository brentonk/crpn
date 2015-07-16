################################################################################
###
### Wrapper to run ordered probit in caret
###
### Can't pass `method = "probit"` as an argument to train(), since that
### clobbers train()'s own method argument
###
################################################################################

library("caret")

## Extract code for running ordered logit in caret
caret_ologit <- getModelInfo("polr")$polr

## Insert slightly modified fitting function to use probit instead
caret_oprobit <- caret_ologit
caret_oprobit$fit <- function (x, y, wts, param, lev, last, classProbs, ...) 
{
    modelArgs <- list(...)
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$.outcome <- y
    modelArgs <- c(list(formula = .outcome ~ ., data = dat), 
        modelArgs)
    modelArgs$Hess <- TRUE
    if (!is.null(wts)) 
        modelArgs$weights <- wts
    modelArgs$method <- "probit"
    ans <- do.call("polr", modelArgs)
    ans$call <- NULL
    ans
}

## Test that it works
if (FALSE) {
    
    dat <- data.frame(x1 = rnorm(100),
                      x2 = rnorm(100),
                      y = ordered(rep(1:4, 25)))

    m1 <- polr(y ~ x1 + x2,
               data = dat,
               Hess = TRUE,
               method = "probit")

    m2 <- train(y ~ x1 + x2,
                data = dat,
                method = caret_oprobit,
                trControl = trainControl(method = "none"))

    all.equal(coef(m1), coef(m2$finalModel))
    all.equal(vcov(m1), vcov(m2$finalModel))

}
