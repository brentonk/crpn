################################################################################
###
### Clean and impute the material capabilities data
###
################################################################################

library("Amelia")
library("dplyr")
library("ggplot2")
library("methods")
library("reshape2")
library("stringr")

###-----------------------------------------------------------------------------
### Load and clean
###-----------------------------------------------------------------------------

raw_NMC <- read.csv("../data/NMC_v4_0.csv",
                     na.strings = "-9")

## Remove unnecessary variables
data_NMC <- select(raw_NMC, -version, -stateabb)

## Examine missingness in each variable
data_NMC %>%
  summarise_each(funs(mean(is.na(.)))) %>%
  round(3)
                                        # Biggest offender by far is milex, with
                                        # 14% missing.  milper and pec have
                                        # about 3% missing, while the others are
                                        # all less than 1%.

## Visually examine the distributions of the raw variables
if (interactive()) {
    ggplot(data_NMC %>% select(irst:cinc) %>% melt(),
           aes(x = value)) +
      geom_density() +
      facet_wrap(~ variable, scales = "free")
}
                                        # They are all really right-skewed

## Look at whether there are 0s present in each variable -- if so, logging is
## self-defeating since it just creates missing data
data_NMC %>%
  select(irst:cinc) %>%
  summarise_each(funs(any(. == 0, na.rm = TRUE)))
                                        # Only tpop and cinc are lacking zeroes,
                                        # so a logarithmic transformation won't
                                        # work

## To try and normalize, calculate an inverse hyperbolic sine transformation of
## each variable
##
## The exception is cinc, because its values are all very small (between 0 and 1
## by definition) and it has no zeroes -- so cinc gets logged
##
## On the asinh() transform, see Burbidge et al, "Alternative Transformations to
## Handle Extreme Values of the Dependent Variable," JASA 1988
data_NMC <- data_NMC %>%
  mutate_each(funs(asinh),
              irst:upop) %>%
  mutate(cinc = log(cinc))

## Examine the distributions of the transformed variables
if (interactive()) {
    ggplot(data_NMC %>% select(irst:cinc) %>% melt(),
           aes(x = value)) +
      geom_density() +
      facet_wrap(~ variable, scales = "free")
}
                                        # Much better!


###-----------------------------------------------------------------------------
### Multiply impute
###-----------------------------------------------------------------------------

## asinh()-transformed variables are bounded below by 0
bds <- cbind(which(names(data_NMC) %in% c("irst",
                                          "milex",
                                          "milper",
                                          "pec",
                                          "tpop",
                                          "upop")),
             0,
             .Machine$double.xmax)

## Run Amelia
##
## Using a ridge prior because of instability (lots of singularity errors)
## induced by high number of tscs variables, and high correlation among some of
## them
set.seed(999)                           # For exact replicability
impute_NMC <- amelia(x = data_NMC,
                     m = 10,
                     ts = "year",
                     cs = "ccode",
                     polytime = 3,
                     intercs = TRUE,
                     bounds = bds,
                     max.resample = 1000,
                     p2s = 2,
                     empri = 0.001 * nrow(data_NMC))

save(impute_NMC,
     file = "results-impute-nmc.rda")

## Save the imputed datasets separately, so that we don't have to load up the
## whole 100+ MB file whenever we just want the data
imputations_NMC <- impute_NMC$imputations
save(imputations_NMC,
     file = "results-imputations-nmc.rda")

## Also save the cleaned pre-imputation data
save(data_NMC,
     file = "results-data-nmc.rda")
