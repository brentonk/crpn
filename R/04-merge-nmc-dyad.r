################################################################################
###
### Function to merge NMC data into dyadic data and, optionally, calculate the
### logged capability ratio
###
################################################################################

library("dplyr")
library("stringr")

merge_NMC_dyad <- function(nmc, dyad, capratio = TRUE)
{
    ## Make "Side A" and "Side B" copies of the NMC data
    nmc_a <- nmc
    nmc_b <- nmc

    ## Make variable names in the NMC data country-specific, except year
    names(nmc_a) <- ifelse(names(nmc_a) != "year",
                           paste0(names(nmc_a), "_a"),
                           names(nmc_a))
    names(nmc_b) <- str_replace(names(nmc_a), "_a$", "_b")

    ## Merge in each NMC dataset in sequence
    dyad <- dyad %>%
        left_join(nmc_a, by = c("ccode_a", "year")) %>%
        left_join(nmc_b, by = c("ccode_b", "year"))

    ## Calculate logged capability ratio, if requested
    ##
    ## Takes exponents of cinc_a and cinc_b since these are already logged
    if (capratio) {
        dyad <- mutate(dyad,
                       capratio = exp(cinc_a) / (exp(cinc_a) + exp(cinc_b)),
                       capratio = log(capratio))
    }

    dyad
}
