################################################################################
###
### Replication of Fang, Johnson, Leeds 2014, "To Concede or Resist?", replacing
### CINC ratio with DOE scores
###
################################################################################

library("caret")
library("dplyr")
library("foreign")
library("sampleSelection")

raw_fang_2014 <- read.dta("fang-2013.dta")
doe_dir_dyad <- read.csv("../R/results-predict-dir-dyad.csv")

## Convert outcomes to factors
data_fang_2014 <- raw_fang_2014 %>%
    mutate(dispute = factor(
               dispute,
               levels = 0:1,
               labels = c("0", "1")
           ),
           targ_resist = factor(
               targ_resist,
               levels = 0:1,
               labels = c("0", "1")
           ))

## Merge in DOE scores
data_fang_2014 <- left_join(data_fang_2014,
                            doe_dir_dyad,
                            by = c(target = "ccode_b",
                                   challenger = "ccode_a",
                                   year = "year"))

## Fit original model
f_selection <- dispute ~ pchaloff + pchalneu + c_win + ln_distance + jdem +
                   s_un_glo + peaceyrs + peaceyrs2 + peaceyrs3
f_outcome <- targ_resist ~ ally_cost + pchaloff + pchalneu + c_win
cr_fang_2014 <- selection(
    selection = f_selection,
    outcome = f_outcome,
    data = data_fang_2014
)

## Fit new model, replacing capability ratio (c_win) with DOE scores
f_selection_doe <- update(f_selection,
                          . ~ . - c_win + VictoryA + VictoryB)
f_outcome_doe <- update(f_outcome,
                        . ~ . - c_win + VictoryA + VictoryB)
doe_fang_2014 <- selection(
    selection = f_selection_doe,
    outcome = f_outcome_doe,
    data = data_fang_2014
)

## TODO: Cross-validate
