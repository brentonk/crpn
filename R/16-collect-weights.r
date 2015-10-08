################################################################################
###
### Collect the ensemble weights from each imputation, which are saved in
### separate files in results-weights/, into a single list
###
################################################################################

load("results-imputations-train.rda")   # Only need it for the length, but this
                                        # will help keep the code compatible if
                                        # we change the number of imputations

n_imp <- length(imputations_train)

## Set up an empty list to store results
res <- list()

## For each imputation, retrieve the corresponding dataset and store it in the
## corresponding list position
for (i in seq_len(n_imp)) {
    ## This part needs to be consistent with the naming scheme used at the end
    ## of 15-train-weights.r
    file_name <- file.path("results-weights",
                           paste0("imp",
                                  sprintf("%.2d", i),
                                  ".rda"))

    wt_object <- load(file_name)
    if (length(wt_object) != 1)
        stop("Loaded data file should contain exactly one object")
    res[[i]] <- get(wt_object)
}

## Rename the stored object
##
## The natural name is "trained_weights," to pair with "trained_models," but of
## course that's what each of the component parts was originally named in
## 15-train-weights.r, hence why we need this renaming hack to avoid collision
## problems.  Could get around it by loading into an environment other than the
## global one, but not really worth all that trouble
trained_weights <- res

save(trained_weights,
     file = "results-trained-weights.rda")
