################################################################################
###
### Collect the results of the variable importance re-runs into a single list
###
### Essentially an adapted version of the same code in 16-collect-weights.r,
### since both times we're dealing with trained weights objects
###
################################################################################

## Retrieve relevant filenames
varimp_dir <- "results-varimp"
varimp_files <- list.files(path = varimp_dir,
                           pattern = "\\.rda$")
varimp_files <- file.path(varimp_dir, varimp_files)

## Sanity check
stopifnot(length(varimp_files) == 180)

## Retrieve and store each set of results
##
## Can't load all at once due to name collisions
results_varimp <- vector("list", length(varimp_files))
for (i in seq_along(varimp_files)) {
    loaded_objects <- load(varimp_files[i])

    if (length(loaded_objects) != 1)
        stop("Loaded data file should contain exactly one object")
    results_varimp[[i]] <- get(loaded_objects)
}

save(results_varimp, file = "results-varimp.rda")
