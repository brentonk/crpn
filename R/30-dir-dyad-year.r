################################################################################
###
### Make dataset of all directed dyad-years
###
################################################################################

library("dplyr")
library("foreach")

load("results-data-nmc.rda")

## Identify complete cases
##
## Later on, this will prevent us from multiplying some computations tenfold,
## since we're averaging over imputed datasets for incomplete cases
data_NMC$complete <- rowSums(is.na(data_NMC)) == 0

## Extract set of state-years
state_year <- select(data_NMC,
                     ccode, year, complete)

## Construct set of directed dyad-years
dir_dyad_year <- with(data_NMC, {
    ## Set of years covered
    yrs <- sort(unique(year))

    foreach (yr = yrs, .combine = rbind) %do% {
        ## Countries active in the given year
        countries <- ccode[year == yr]

        ## Calculate all ordered combinations of countries
        dir_dyads <- expand.grid(ccode_a = countries,
                                 ccode_b = countries)

        ## Exclude self-pairings
        dir_dyads <- filter(dir_dyads,
                            ccode_a != ccode_b)

        ## Identify complete cases
        dir_dyads$year <- yr
        dir_dyads <- dir_dyads %>%
            left_join(state_year %>% rename(ccode_a = ccode,
                                            complete_a = complete),
                      by = c("ccode_a", "year")) %>%
            left_join(state_year %>% rename(ccode_b = ccode,
                                            complete_b = complete),
                      by = c("ccode_b", "year")) %>%
            mutate(complete = complete_a & complete_b) %>%
            select(-complete_a, -complete_b)
    }
})

save(dir_dyad_year,
     file = "results-dir-dyad-year.rda")
