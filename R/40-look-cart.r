################################################################################
###
### Look at the CART models more closely
###
### Specifically, those on the components with time included, because these
### receive disproportionate weight in the final Super Learner
###
################################################################################

library("caret")
library("foreach")
library("rpart")

source("11-defs-train.r")

load("results-trained-models.rda")
load("results-trained-weights.rda")

## Extract the CART models and their weights
cart_models <- foreach (imp = trained_models) %do% {
    imp[["cart_t"]]
}
cart_weights <- foreach (imp = trained_weights) %do% {
    ## Need to do this trick with the names because currently the names get lost
    ## in the weight training process
    ##
    ## TODO: Fix this
    wt <- imp$weights$par
    wt <- c(wt, 1 - sum(wt))
    names(wt) <- names(method_args)
    wt["cart_t"]
}

## Plot the regression tree for each imputation
plot_dir <- "cart-plots"
if (!dir.exists(plot_dir))
    dir.create(plot_dir)

for (i in seq_along(cart_models)) {
    pdf(file = file.path(plot_dir,
                         paste0("imp",
                                sprintf("%.2d", i),
                                ".pdf")),
        width = 8.5,
        height = 11)
    plot(cart_models[[i]]$finalModel,
         main = paste0("Imputation ",
                       i,
                       ", weight = ",
                       format(cart_weights[[i]], digits = 2)))
    text(cart_models[[i]]$finalModel)
    dev.off()
}

## While we're at it, let's look at the variable importance metrics for the
## random forests with time
rf_varImp <- foreach (imp = trained_models) %do% {
    varImp(imp[["rf_t"]])
}

print(rf_varImp)
