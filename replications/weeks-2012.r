################################################################################
###
### Replication of Weeks 2012, "Strongmen and Straw Men," replacing CINC ratios
### with DOE scores
###
### Replicating Table 1, Model 2
###
################################################################################

library("caret")
library("dplyr")
library("foreign")

source("glm-and-cv.r")

raw_weeks_2012 <- read.dta("weeks-2012.dta")
doe_dir_dyad <- read.csv("../R/results-predict-dir-dyad.csv")

## Convert response to factor and drop unused cases (those with a missing value
## for country 1's democracy score)
data_weeks_2012 <- raw_weeks_2012 %>%
    mutate(mzinit = factor(
               mzinit,
               levels = 0:1,
               labels = c("No", "Yes")
           )) %>%
    filter(!is.na(democracy_1))

## Merge in DOE scores
data_weeks_2012 <- left_join(data_weeks_2012,
                             doe_dir_dyad,
                             by = c(ccode1 = "ccode_a",
                                    ccode2 = "ccode_b",
                                    year = "year"))
stopifnot(with(data_weeks_2012, sum(is.na(VictoryA)) == 0))

## Table 1, Model 2
f_weeks_2012 <-
    mzinit ~ machinejlw_1 + juntajlw_1 + bossjlw_1 + strongmanjlw_1 +
        allotherauts_1 + newregime_1 + democracy_2 + cap_1 + cap_2 + initshare +
        dependlow + majmaj + minmaj + majmin + contigdum + logdist + s_wt_glo +
        s_lead_1 + s_lead_2 + pcyrsmzinit + pcyrsmzinits1 + pcyrsmzinits2 +
        pcyrsmzinits3

## Null hypothesis: no variation in effects across types of authoritarians
hyp_main <- c("juntajlw_1 = machinejlw_1",
              "bossjlw_1 = machinejlw_1",
              "strongmanjlw_1 = machinejlw_1",
              "allotherauts_1 = machinejlw_1")

## Replicate original model and cross-validate
set.seed(2312)
cr_weeks_2012 <- glm_and_cv(
    form = f_weeks_2012,
    data = data_weeks_2012,
    se_cluster = data_weeks_2012$dirdyadid,
    hyp_main = hyp_main,
    hyp_power = c("initshare = 0", "cap_1 = 0", "cap_2 = 0"),
    number = 10,
    repeats = 10
)
print(cr_weeks_2012$summary)

## Replicate, replacing all functions of the CINC score (the two raw scores and
## the ratio) with DOE scores
doe_weeks_2012 <- glm_and_cv(
    form = update(f_weeks_2012,
                  . ~ . - cap_1 - cap_2 - initshare + VictoryA + VictoryB),
    data = data_weeks_2012,
    se_cluster = data_weeks_2012$dirdyadid,
    hyp_main = hyp_main,
    hyp_power = c("VictoryA = 0", "VictoryB = 0"),
    number = 10,
    repeats = 10
)
print(doe_weeks_2012$summary)

save(cr_weeks_2012,
     doe_weeks_2012,
     file = "results-weeks-2012.rda")
