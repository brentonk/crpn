################################################################################
###
### Replication of Salehyan 2008, "Externalities of Civil Strife," replacing
### CINC ratios with DOE scores
###
### Replicating Table 1, Model 1
###
################################################################################

library("caret")
library("dplyr")
library("foreign")

source("glm-and-cv.r")

raw_salehyan_2008_ajps <- read.dta("salehyan-2008-ajps.dta")
doe_dir_dyad <- read.csv("../R/results-predict-dir-dyad.csv")

## Convert dependent variable to factor and eliminate pre-1955 cases
data_salehyan_2008_ajps <- raw_salehyan_2008_ajps %>%
    mutate(mzinit_lead = factor(
               mzinit_lead,
               levels = 0:1,
               labels = c("No", "Yes")
           )) %>%
    filter(year >= 1955)

## Merge in DOE scores
data_salehyan_2008_ajps <- left_join(data_salehyan_2008_ajps,
                                 doe_dir_dyad,
                                 by = c(ccode1 = "ccode_a",
                                        ccode2 = "ccode_b",
                                        year = "year"))
stopifnot(with(data_salehyan_2008_ajps, !any(is.na(VictoryA))))

## Replicating Model 1 -- since Model 2 interacts the main substantive variables
## and the capability ratio, this makes for a cleaner separation between
## hypotheses
f_salehyan_2008_ajps <-
    mzinit_lead ~ logref1 + logref2 + uppcivcon1 +
        uppcivcon2 + dem1*dem2 + trans1*trans2 + contig + colcont + capshare +
        s_wt_glo + depend1 + depend2 + igos + lpcyrs + lpcyrs1 + lpcyrs2 + lpcyrs3

## Null hypothesis: no effect of refugee flows (in or out)
hyp_main <- c("logref1 = 0", "logref2 = 0")

## Replicate original model and cross-validate
set.seed(1908)
cr_salehyan_2008_ajps <- glm_and_cv(
    form = f_salehyan_2008_ajps,
    data = data_salehyan_2008_ajps,
    se_cluster = data_salehyan_2008_ajps$dyad,
    hyp_main = hyp_main,
    hyp_power = "capshare = 0",
    number = 10,
    repeats = 100,
    probit = TRUE
)
print(cr_salehyan_2008_ajps$summary)

## Replicate, replacing capability ratio with DOE scores
doe_salehyan_2008_ajps <- glm_and_cv(
    form = update(f_salehyan_2008_ajps,
                  . ~ . - capshare + VictoryA + VictoryB),
    data = data_salehyan_2008_ajps,
    se_cluster = data_salehyan_2008_ajps$dyad,
    hyp_main = hyp_main,
    hyp_power = c("VictoryA = 0", "VictoryB = 0"),
    number = 10,
    repeats = 100,
    probit = TRUE
)
print(doe_salehyan_2008_ajps$summary)

save(cr_salehyan_2008_ajps,
     doe_salehyan_2008_ajps,
     file = "results-salehyan-2008-ajps.rda")
