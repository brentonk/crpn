################################################################################
###
### Calculate predicted probabilities of dispute outcomes for all dyad-years
###
################################################################################

library("plyr")
library("dplyr")
library("foreach")
library("iterators")

source("04-merge-nmc-dyad.r")
source("20-predict-from-ensemble.r")

load("results-dir-dyad-year.rda")
load("results-data-nmc.rda")
load("results-impute-nmc-new.rda")
load("results-trained-models.rda")
load("results-trained-weights.rda")


###-----------------------------------------------------------------------------
### Read in years to make predictions for
###
### This assumes that the script has two arguments: the first giving the start
### year, the second giving the number of years.  e.g.,
###   Rscript 31-predict.r 1850 10
### would make the measurements for the years 1850-1859.
###-----------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 2) {
    start_year <- as.integer(args[1])
    n_years <- as.integer(args[2])
    end_year <- start_year + n_years
} else {
    start_year <- min(dir_dyad_year$year)
    end_year <- max(dir_dyad_year$year)
}

dir_dyad_year <- filter(dir_dyad_year,
                        year >= start_year,
                        year < end_year)
dim(dir_dyad_year)


###-----------------------------------------------------------------------------
### Calculate predictions
###-----------------------------------------------------------------------------

## Start with the complete cases
##
## Only need to run once for these
dat_complete <- merge_NMC_dyad(nmc = data_NMC,
                               dyad = dir_dyad_year %>% filter(complete),
                               capratio = TRUE)
stopifnot(!any(is.na(dat_complete)))

cat("Started complete cases", as.character(Sys.time()), "\n")

system.time(
    pred_complete <- predict_from_ensemble(dat = dat_complete,
                                           models = trained_models,
                                           wts = trained_weights)
)

## For the incomplete cases, loop over imputations
if (!all(dir_dyad_year$complete)) {
    pred_incomplete <- foreach(nmc = imputations_NMC_new, i = icount()) %do% {
        cat("Started incomplete cases imputation", i, as.character(Sys.time()), "\n")

        ## Merge imputed covariates into incomplete dyads
        dat <- merge_NMC_dyad(nmc = nmc,
                              dyad = dir_dyad_year %>% filter(!complete),
                              capratio = TRUE)

        pred <- predict_from_ensemble(dat = dat,
                                      models = trained_models,
                                      wts = trained_weights)

        pred
    }

    ## Average predictions for incomplete cases
    pred_incomplete <- Reduce("+", pred_incomplete)
    pred_incomplete <- pred_incomplete / length(imputations_NMC_new)

    ## Merge with country codes and years
    pred_complete <- cbind(dir_dyad_year %>% filter(complete),
                           pred_complete)
    pred_incomplete <- cbind(dir_dyad_year %>% filter(!complete),
                             pred_incomplete)
    pred_dir_dyad <- rbind(pred_complete, pred_incomplete)
} else {
    cat("No incomplete cases", as.character(Sys.time()), "\n")
    pred_dir_dyad <- cbind(dir_dyad_year, pred_complete)
}

## Remove completion indicator from output
pred_dir_dyad <- dplyr::select(pred_dir_dyad, -complete)
stopifnot(nrow(pred_dir_dyad) == nrow(dir_dyad_year))

## Sort by dyad-year
idx <- with(pred_dir_dyad,
            order(ccode_a, ccode_b, year))
pred_dir_dyad <- pred_dir_dyad[idx, ]
rownames(pred_dir_dyad) <- NULL

## Write results to CSV
##
## This is easier than writing in RData format, since that would require taking
## care not to let the variable names collide
subdir <- "results-predict"
if (!dir.exists(subdir))
    dir.create(subdir)
write.csv(pred_dir_dyad,
          row.names = FALSE,
          file = file.path(subdir, paste0(start_year, ".csv")))
