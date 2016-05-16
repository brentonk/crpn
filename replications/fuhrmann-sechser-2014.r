################################################################################
###
### Replication of Fuhrmann & Sechser 2014, "Signalling Alliance Commitments,"
### replacing CINC ratio with DOE scores
###
### Replicating Table 2, Model 1
###
################################################################################

library("caret")
library("dplyr")
library("foreign")
library("MASS")

source("glm-and-cv.r")

data_fuhrmann_sechser_2014 <- read.dta("fuhrmann-sechser-2014.dta")
doe_dir_dyad <- read.csv("../R/results-predict-dir-dyad.csv")

## only keep the politically-relevant ones
data_fuhrmann_sechser_2014 <- filter(data_fuhrmann_sechser_2014,
                                     politically_relevant == 1)

## convert DV to factor
data_fuhrmann_sechser_2014$military_conflict <-
    factor(data_fuhrmann_sechser_2014$military_conflict,
           levels = 0:1,
           labels = c("No", "Yes"))

## merge in DOE
data_fuhrmann_sechser_2014 <- left_join(data_fuhrmann_sechser_2014,
                                        doe_dir_dyad,
                                        by = c(state_a = "ccode_a",
                                               state_b = "ccode_b",
                                               year = "year"))

## every row has a DOE
stopifnot(with(data_fuhrmann_sechser_2014, !any(is.na(VictoryA))))

## replication formula
f_fuhrmann_sechser_2014 <-
    military_conflict ~ defense_pact_nuclear_cow + nuclear_deployment +
        defense_pact_nonnuclear_cow + us_troops + challenger_nuclear_weapons +
        target_nuclear_weapons + contiguity + alliance_with_challenger_cow +
        foreign_policy_similarity + power_ratio + challenger_polity +
        target_polity + polity_interaction + time_conflict + time_conflict2 +
        time_conflict3

## Null hypothesis: no effect of defense pact with nuclear power
hyp_main <- "defense_pact_nuclear_cow = 0"

## run the reported model with cross validation
set.seed(630976)
cr_fuhrmann_sechser_2014 <- glm_and_cv(
    form = f_fuhrmann_sechser_2014,
    data = data_fuhrmann_sechser_2014,
    se_cluster = data_fuhrmann_sechser_2014$dyad_id,
    hyp_main = hyp_main,
    hyp_power = "power_ratio = 0",
    number = 10,
    repeats = 100,
    probit = TRUE
)
print(cr_fuhrmann_sechser_2014$summary)

## run it again with DOE
doe_fuhrmann_sechser_2014 <- glm_and_cv(
    form = update(f_fuhrmann_sechser_2014,
                  . ~ . - power_ratio + VictoryA + VictoryB),
    data = data_fuhrmann_sechser_2014,
    se_cluster = data_fuhrmann_sechser_2014$dyad_id,
    hyp_main = hyp_main,
    hyp_power = c("VictoryA = 0", "VictoryB = 0"),
    number = 10,
    repeats = 100,
    probit = TRUE
)
print(doe_fuhrmann_sechser_2014$summary)

## save it
save(cr_fuhrmann_sechser_2014,
     doe_fuhrmann_sechser_2014,
     file = "results-fuhrmann-sechser-2014.rda")
