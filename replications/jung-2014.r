################################################################################
###
### Replication of Jung 2014, "Foreign Targets and Diversionary Conflict,"
### replacing CINC ratios with DOE scores
###
### Replicating Table 1, Model 2
###
################################################################################

library("caret")
library("dplyr")
library("foreign")

source("glm-and-cv.r")

raw_jung_2014 <- read.dta("jung-2014.dta")
doe_dir_dyad <- read.csv("../R/results-predict-dir-dyad.csv")

## Convert outcome to factor
data_jung_2014 <- raw_jung_2014 %>%
    mutate(initiate = factor(
               initiate,
               levels = 0:1,
               labels = c("No", "Yes")
           ))

## 1- and 5-year lags of DOE variables
data_jung_2014 <- left_join(data_jung_2014,
                            doe_dir_dyad %>%
                            mutate(year = year + 1) %>%
                            rename(VictoryA_1 = VictoryA,
                                   Stalemate_1 = Stalemate,
                                   VictoryB_1 = VictoryB),
                            by = c(ccode1 = "ccode_a",
                                   ccode2 = "ccode_b",
                                   year = "year"))
data_jung_2014 <- left_join(data_jung_2014,
                            doe_dir_dyad %>%
                            mutate(year = year + 5) %>%
                            rename(VictoryA_5 = VictoryA,
                                   Stalemate_5 = Stalemate,
                                   VictoryB_5 = VictoryB),
                            by = c(ccode1 = "ccode_a",
                                   ccode2 = "ccode_b",
                                   year = "year"))

## Create DOE analogue of the "power_cinc" measure: the 25% of observations with
## the power shift most favorable to Country B
##
## Shift in B's direction:
##   (p_B_1 - p_B_5) - (p_A_1 - p_A_5)
data_jung_2014 <- data_jung_2014 %>%
    mutate(shift_DOE = (VictoryB_1 - VictoryB_5) - (VictoryA_1 - VictoryA_5),
           powtg_DOE = as.numeric(shift_DOE > quantile(shift_DOE, 0.75, na.rm = TRUE)))

## Check that we don't have new missingness
stopifnot(with(data_jung_2014, !any(is.na(powtg_DOE) & !is.na(powtg_cinc))))

## Replicating Table 1, Model 2
f_jung_2014 <-
    initiate ~ dom * (powtg_cinc + identg + tertg + hegtg) +
        caprat_1 + cowally_1 + depend1_1 + depend2_1 + jointdemo_1 +
        jointminor_1 + border + lndistance + mid1peace + mid1sp1 +
        mid1sp2 + mid1sp3

## Null hypothesis: no effect of target rising power or its interaction with
## domestic unrest
##
## Setting separately for each model since power is part of it

## Original model
set.seed(5306)
cr_jung_2014 <- glm_and_cv(
    form = f_jung_2014,
    data = data_jung_2014,
    se_cluster = data_jung_2014$dyadid,
    hyp_main = c("powtg_cinc = 0", "dom:powtg_cinc = 0"),
    hyp_power = c("caprat_1 = 0"),
    number = 10,
    repeats = 10
)
print(cr_jung_2014$summary)

## Replicate, replacing CINC ratio with DOE scores
doe_jung_2014 <- glm_and_cv(
    form = update(f_jung_2014,
                  . ~ . - dom * powtg_cinc + dom * powtg_DOE - caprat_1 +
                      VictoryA_1 + VictoryB_1),
    data = data_jung_2014,
    se_cluster = data_jung_2014$dyadid,
    hyp_main = c("powtg_DOE = 0", "dom:powtg_DOE = 0"),
    hyp_power = c("VictoryA_1 = 0", "VictoryB_1 = 0"),
    number = 10,
    repeats = 10
)
print(doe_jung_2014$summary)

save(cr_jung_2014,
     doe_jung_2014,
     file = "results-jung-2014.rda")
