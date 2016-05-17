################################################################################
###
### Replication of Bennett 2006, "Toward a Continuous Specification...,"
### replacing CINC ratio with DOE scores
###
### Replicating Table 1, Column 1
###
################################################################################

library("caret")
library("dplyr")
library("foreign")
library("MASS")

source("glm-and-cv.r")

data_bennett_2006 <- read.dta("bennett-2006.dta")
doe_dir_dyad <- read.csv("../R/results-predict-dir-dyad.csv")

## drop observations with ongoing war
data_bennett_2006 <- data_bennett_2006[data_bennett_2006$cwongo != 1,]

## construct some variables, as per Bennett's .do file
data_bennett_2006$demint1orig <- data_bennett_2006$demint1
data_bennett_2006$demint2orig <- data_bennett_2006$demint2
data_bennett_2006$demi1simorig <- data_bennett_2006$demi1sim
data_bennett_2006$di1di2orig <- data_bennett_2006$di1di2
data_bennett_2006$d1d2isqrorig <- data_bennett_2006$d1d2isqr

data_bennett_2006$demint1 <- data_bennett_2006$polity21
data_bennett_2006$demint2 <- data_bennett_2006$polity22
data_bennett_2006$demi1sim <- data_bennett_2006$demi1simpol
data_bennett_2006$di1di2 <- data_bennett_2006$di1di2pol
data_bennett_2006$d1d2isqr <- data_bennett_2006$d1d2isqrpol

## make a factor DV
data_bennett_2006$cwinit <- factor(data_bennett_2006$cwinit,
                                   levels = 0:1,
                                   labels = c("No", "Yes"))

## merge in the DOE scores and calculate DOE equivalent of "parity"
data_bennett_2006 <- left_join(data_bennett_2006,
                               doe_dir_dyad,
                               by = c(ccode1 = "ccode_a",
                                      ccode2 = "ccode_b",
                                      year = "year")) %>%
    mutate(ParityDOE = abs(VictoryA - VictoryB))

## check the merge
stopifnot(with(data_bennett_2006, !any(!is.na(parity) & is.na(VictoryA))))

## replicate Bennett's table 1
f_bennett_2006 <-
    cwinit ~ demint1 + demint2 + demi1sim + di1di2 + d1d2isqr + cap_1 + cap_2 +
        lndist + anypact + priordisputeproportion + parity + cwpceyrs + cwpceyr1 +
        cwpceyr2 + cwpceyr3

## Null hypothesis: no effect of any of the regime type variables
hyp_main <- c("demint1 = 0", "demint2 = 0", "demi1sim = 0", "di1di2 = 0", "d1d2isqr = 0")

## Reproduce and cross-validate original model
set.seed(92379)
cr_bennett_2006 <- glm_and_cv(
    form = f_bennett_2006,
    data = data_bennett_2006,
    se_cluster = data_bennett_2006$dyadid,
    hyp_main = hyp_main,
    hyp_power = c("cap_1 = 0", "cap_2 = 0", "parity = 0"),
    number = 10,
    repeats = 10
)
print(cr_bennett_2006$summary)

## run it with DOE
doe_bennett_2006 <- glm_and_cv(
    form = update(f_bennett_2006,
                  . ~ . - parity - cap_1 - cap_2 + VictoryA + VictoryB + ParityDOE),
    data = data_bennett_2006,
    se_cluster = data_bennett_2006$dyadid,
    hyp_main = hyp_main,
    hyp_power = c("VictoryA = 0", "VictoryB = 0", "ParityDOE = 0"),
    number = 10,
    repeats = 10
)
print(doe_bennett_2006$summary)

save(cr_bennett_2006,
     doe_bennett_2006,
     file = "results-bennett-2006.rda")
