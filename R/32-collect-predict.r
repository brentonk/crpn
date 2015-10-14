################################################################################
###
### Collect predicted probabilities of dispute outcomes into a single file and
### compute undirected dyad-year predictions
###
################################################################################

library("dplyr")

load("results-dir-dyad-year.rda")


###-----------------------------------------------------------------------------
### Directed dyads
###-----------------------------------------------------------------------------

## Collate files stored in year-by-year CSVs
subdir <- "results-predict"
predict_files <- list.files(subdir)
predict_files <- file.path(subdir, predict_files)
predict_dir_dyad <- lapply(predict_files, read.csv)
predict_dir_dyad <- do.call(rbind, predict_dir_dyad)
stopifnot(nrow(predict_dir_dyad) == nrow(dir_dyad_year))

## Sort by dyad-year
idx <- with(predict_dir_dyad,
            order(ccode_a, ccode_b, year))
predict_dir_dyad <- predict_dir_dyad[idx, ]
rownames(predict_dir_dyad) <- NULL


###-----------------------------------------------------------------------------
### Undirected dyads
###
### Predicted probability is the average of the two directed predictions
###-----------------------------------------------------------------------------

## Start with two datasets: the original directed predictions, and the same ones
## where we've just flipped identities of A and B
predict_dyad_orig <- predict_dir_dyad %>%
    rename(VictoryB_orig = VictoryB,
           Stalemate_orig = Stalemate,
           VictoryA_orig = VictoryA)
predict_dyad_flip <- predict_dir_dyad %>%
    rename(ccode_a = ccode_b,
           ccode_b = ccode_a,
           VictoryB_flip = VictoryA,
           Stalemate_flip = Stalemate,
           VictoryA_flip = VictoryB)

## Calculate predicted probabilities
##
## Keeping all pairings in the table, even though every observation is
## effectively there twice, for ease of merging later on
##
## Splitting this into pieces because of weird dplyr behavior that seems to be
## memory-related -- couldn't totally eliminate the bugs, which don't repeat
## consistently, but got it to work at least once
predict_dyad <-
    left_join(predict_dyad_orig,
              predict_dyad_flip,
              by = c("ccode_a", "ccode_b", "year"))
predict_dyad <-
    mutate(predict_dyad,
           VictoryB = 0.5 * (VictoryB_orig + VictoryB_flip),
           Stalemate = 0.5 * (Stalemate_orig + Stalemate_flip),
           VictoryA = 0.5 * (VictoryA_orig + VictoryA_flip))
predict_dyad <-
    select(predict_dyad,
           ccode_a, ccode_b, year, VictoryB, Stalemate, VictoryA)

## Sanity checks
stopifnot(nrow(predict_dyad) == nrow(predict_dir_dyad))
stopifnot(all.equal(with(predict_dyad,
                         VictoryB + Stalemate + VictoryA),
                    rep(1, nrow(predict_dyad)),
                    check.attributes = FALSE))


###-----------------------------------------------------------------------------
### Write output to CSV
###
### Using CSV instead of RData since these are intended for distribution
###-----------------------------------------------------------------------------

write.csv(predict_dir_dyad,
          row.names = FALSE,
          file = "results-predict-dir-dyad.csv")

write.csv(predict_dyad,
          row.names = FALSE,
          file = "results-predict-dyad.csv")
