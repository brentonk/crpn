################################################################################
###
### Replication of Salehyan 2008, "Externalities of Civil Strife," replacing
### CINC ratios with DOE scores
###
### Replicating Table 1, Model 2
###
################################################################################

library("caret")
library("dplyr")
library("foreign")

source("glm-and-cv.r")

raw_salehyan_2008a <- read.dta("salehyan-2008a.dta")
doe_dir_dyad <- read.csv("../R/results-predict-dir-dyad.csv")

## Convert dependent variable to factor and eliminate pre-1955 cases
data_salehyan_2008a <- raw_salehyan_2008a %>%
    mutate(mzinit_lead = factor(
               mzinit_lead,
               levels = 0:1,
               labels = c("No", "Yes")
           )) %>%
    filter(year >= 1955)

## Merge in DOE scores
data_salehyan_2008a <- left_join(data_salehyan_2008a,
                                 doe_dir_dyad,
                                 by = c(ccode1 = "ccode_a",
                                        ccode2 = "ccode_b",
                                        year = "year"))
stopifnot(with(data_salehyan_2008a, sum(is.na(VictoryA)) == 0))

## Replicate original model and cross-validate
set.seed(1908)
f_salehyan_2008a <-
    mzinit_lead ~ logref1*capshare + logref2*capshare + uppcivcon1 +
        uppcivcon2 + dem1*dem2 + trans1*trans2 + contig + colcont +
        s_wt_glo + depend1 + depend2 + igos + lpcyrs + lpcyrs1 + lpcyrs2 + lpcyrs3
cr_salehyan_2008a <- glm_and_cv(
    form = f_salehyan_2008a,
    data = data_salehyan_2008a,
    number = 10,
    repeats = 100,
    probit = TRUE
)
printCoefmat(cr_salehyan_2008a$summary)

## Replicate, replacing capability ratios with DOE scores (so we now have four
## interactions instead of two)
set.seed(8091)
f_salehyan_2008a_doe <-
    mzinit_lead ~ logref1*(VictoryA + VictoryB) + logref2*(VictoryA + VictoryB) +
        uppcivcon1 + uppcivcon2 + dem1*dem2 + trans1*trans2 + contig + colcont +
        s_wt_glo + depend1 + depend2 + igos + lpcyrs + lpcyrs1 + lpcyrs2 + lpcyrs3
doe_salehyan_2008a <- glm_and_cv(
    form = f_salehyan_2008a_doe,
    data = data_salehyan_2008a,
    number = 10,
    repeats = 100,
    probit = TRUE
)
printCoefmat(doe_salehyan_2008a$summary)

save(cr_salehyan_2008a,
     doe_salehyan_2008a,
     file = "results-salehyan-2008a.rda")
