################################################################################
###
### Clean and impute the material capabilities data
###
################################################################################

library("Amelia")
library("dplyr")
library("foreach")
library("ggplot2")
library("methods")
library("reshape2")
library("stringr")
library("tidyr")
library("xtable")

component_names <- c("irst",
                     "milex",
                     "milper",
                     "pec",
                     "tpop",
                     "upop")

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

###-----------------------------------------------------------------------------
### Transform component variables
###
### Using inverse hyperbolic sines for everything except CINC, which gets
### logged.  See Burbidge et al, "Alternative Transformations to Handle Extreme
### Values of the Dependent Variable," JASA 1988
### ----------------------------------------------------------------------------

## Function to calculate the Kolmogorov-Smirnov test statistic for a vector of
## numeric values, compared to a normal distribution with the same mean and
## variance (among observed values)
##
## Lower values <-> More normally distributed
ks_stat <- function(x) {
    x <- x[!is.na(x)]
    ans <- ecdf(x)(x) - pnorm(x, mean = mean(x), sd = sd(x))
    max(abs(ans))
}

## For each CINC component, search across potential scales to find the one that
## makes the asinh transformation most normal
theta_candidates <- 2^seq(-10, 10)
theta <- foreach (component = component_names, .combine = "c") %do% {
    x <- data_NMC[[component]]
    loss <- foreach (s = theta_candidates) %do% {
        ks_stat(asinh(s * x))
    }
    theta_candidates[which.min(loss)]
}
names(theta) <- component_names

## Transform each component variable using the chosen scale
for (component in component_names) {
    x <- data_NMC[[component]]
    s <- theta[component]
    data_NMC[[component]] <- asinh(s * x)
}

## Since the CINC score is strictly positive, log-transform it
data_NMC$cinc <- log(data_NMC$cinc)

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
bds <- cbind(which(names(data_NMC) %in% component_names),
             0,
             .Machine$double.xmax)

## We want to place an observation-level prior with mean zero for any
## missing value satisfying the following criteria:
##   * There are no non-zero observations that come before it
##   * The first observed value that comes after it is zero
##
## To that end, this function takes a data series and gives us the latest year
## we can assume to be zero by these rules (-Inf if no zeroes in the series)
latest_all_zero <- function(x, year)
{
    ## Strip out missing years
    year <- year[!is.na(x)]
    x <- x[!is.na(x)]

    ## We want the latest year at which the cumulative sum of the data series is
    ## zero
    ##
    ## Suppressing warnings because max() of a length-zero vector throws one
    suppressWarnings(max(year[cumsum(x) == 0]))
}

## Calculate the relevant "last all-zero year" for each series of each CINC
## component variable
laz_NMC <- data_NMC %>%
  group_by(ccode) %>%
  summarise_each(funs(latest_all_zero(., year)),
                 irst:upop)
laz_NMC <- left_join(data_NMC %>% select(ccode, year),
                     laz_NMC,
                     by = "ccode")

## Construct matrix of observation-level priors to pass to Amelia
prs <- foreach (v = component_names, .combine = "rbind") %do% {
    ## Identify observations that are both missing and before the relevant year
    rows <- which(is.na(data_NMC[, v]) & laz_NMC[, "year"] < laz_NMC[, v])

    ## Identify corresponding column of the data being passed to amelia()
    col <- which(names(data_NMC) == v)

    ## Prior has zero mean
    mu <- 0

    ## Set standard deviation of prior to the s.d. of the variable in the
    ## overall data
    sigma <- sd(data_NMC[, v], na.rm = TRUE)

    if (length(rows) > 0) {
        cbind(rows, col, mu, sigma)
    } else {
        matrix(nrow = 0, ncol = 4)
    }
}

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
                     priors = prs,
                     max.resample = 1000,
                     p2s = 2,
                     empri = 0.001 * nrow(data_NMC))

## Draw a separate set of imputations to use when applying the model to all
## possible dyad-years
##
## Not sure if it's bad to use the same imputations for both training and
## predictions, but this way should be safe for sure
##
## We should be able to run amelia() again on the last set of output, but for
## some reason this results in lots of singularity errors.  Seems like something
## goes wrong with the ridge prior when run on an object of class "amelia", and
## I can't debug it.  So we'll just copy the code above instead.
set.seed(777)                           # For exact replicability
impute_NMC_new <- amelia(x = data_NMC,
                         m = 10,
                         ts = "year",
                         cs = "ccode",
                         polytime = 3,
                         intercs = TRUE,
                         bounds = bds,
                         priors = prs,
                         max.resample = 1000,
                         p2s = 2,
                         empri = 0.001 * nrow(data_NMC))


###-----------------------------------------------------------------------------
### Calculate proportions of yearly totals
###-----------------------------------------------------------------------------

## Use original (untransformed) data to compute the yearly totals
##
## We could do this within each imputed dataset instead.  Then the resulting
## datasets would each be logically consistent -- i.e., all proportions would
## sum to 1.  But then every observation on the proportions would vary across
## imputed datasets, increasing the variation due to imputation.
untransform_NMC <- raw_NMC %>%
    select_(.dots = c("ccode", "year", component_names))

## Calculate total amount of each component each year, removing missing values
totals_NMC <- untransform_NMC %>%
    group_by(year) %>%
    summarise_each_(funs(sum(., na.rm = TRUE)),
                    vars = component_names)
names(totals_NMC) <- ifelse(names(totals_NMC) %in% component_names,
                            paste0("total_", names(totals_NMC)),
                            names(totals_NMC))

## Apply the scaling factors used in the transformation process (so that the
## proportions sum to roughly 1 within each year)
for (component in component_names) {
    total <- paste0("total_", component)
    totals_NMC[[total]] <- theta[component] * totals_NMC[[total]]
}

## Function to compute proportions for a given (transformed) set of data and
## merge them into it
##
## Writing a function since it needs to be done separately for each imputation
## -- can't compute the proportions in advance due to missingness in raw data
add_props <- function(x)
{
    ## Untransform the given data
    untransform_x <- x %>%
        select_(.dots = c("ccode", "year", component_names)) %>%
        mutate_each_(funs(sinh), vars = component_names)

    ## Calculate proportions
    prop_mutations <- paste0(component_names,
                             "/total_",
                             component_names)
    names(prop_mutations) <- paste0("prop_", component_names)
    props_x <-
        left_join(untransform_x,
                  totals_NMC,
                  by = "year") %>%
        mutate_(.dots = prop_mutations) %>%
        select_(.dots = c("ccode", "year", names(prop_mutations)))

    ## Merge proportions back into original data and return
    left_join(x,
              props_x,
              by = c("ccode", "year"))
}

## Add proportions to original data
data_NMC <- add_props(data_NMC)

## Add proportions to imputed datasets
imputations_NMC <- lapply(impute_NMC$imputations, add_props)
imputations_NMC_new <- lapply(impute_NMC$imputations, add_props)

## Save data and imputations
save(raw_NMC,
     data_NMC,
     totals_NMC,
     file = "results-data-nmc.rda")

save(impute_NMC,
     imputations_NMC,
     file = "results-impute-nmc.rda")

save(impute_NMC_new,
     imputations_NMC_new,
     file = "results-impute-nmc-new.rda")


###-----------------------------------------------------------------------------
### Make table of summary statistics of raw components
###-----------------------------------------------------------------------------

component_pretty_names <- c("Iron and Steel Production",
                            "Military Expenditures",
                            "Military Personnel",
                            "Primary Energy Consumption",
                            "Total Population",
                            "Urban Population")

## Compute summary statistics for each component variable
table_NMC <- raw_NMC %>%
    select_(.dots = component_names) %>%
    gather(component, value, everything()) %>%
    group_by(component) %>%
    summarise(zero = sum(value == 0, na.rm = TRUE) / length(value),
              miss = mean(is.na(value)))

## Add the scale used in the transformation of each variable
table_NMC <- left_join(table_NMC,
                       data.frame(component = component_names,
                                  theta = theta))

## Prettify numbers and names
table_NMC <- table_NMC %>%
    mutate(component = factor(component,
                              levels = component_names,
                              labels = component_pretty_names),
           theta = round(log2(theta)),
           theta = paste0("$2^{", theta, "}$"),
           zero = sprintf("%.3f", zero),
           miss = sprintf("%.3f", miss)) %>%
    rename(Component = component,
           "Pr(Zero)" = zero,
           "Pr(Missing)" = miss,
           "$\\theta$" = theta)

## Convert to LaTeX
xtable_NMC <- xtable(table_NMC,
                     align = c("l", "l", "r", "r", "r"))

print(xtable_NMC,
      file = file.path("..", "latex", "tab-summary.tex"),
      floating = FALSE,
      include.rownames = FALSE,
      sanitize.text.function = identity)


###-----------------------------------------------------------------------------
### Sample data for slides
###
### TODO: Fix scaling when applying sinh()
###-----------------------------------------------------------------------------

data_NMC %>%
    filter(ccode %in% c(2, 200, 365),
           year %in% 1816:1817) %>%
    select(ccode, year, milex, milper, prop_milex, prop_milper) %>%
    mutate_each(funs(sinh), milex, milper) %>%
    mutate(ccode = factor(ccode,
                          levels = c(2, 200, 365),
                          labels = c("USA", "UK", "Russia")),
           ccode = as.character(ccode),
           milex = prettyNum(milex, big.mark = ",")) %>%
    mutate_each(funs(sprintf("%.3f", .)), prop_milex, prop_milper) %>%
    select("Country" = ccode,
           "Year" = year,
           "Funds" = milex,
           "Troops" = milper,
           "%Funds" = prop_milex,
           "%Troops" = prop_milper)
