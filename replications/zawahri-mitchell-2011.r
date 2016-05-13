################################################################################
###
### Replication of Zawahri and Mitchell 2011, "Fragmented Governance of
### International Rivers," replacing CINC scores with DOE scores
###
### Replicating Table 2, Model 1
###
################################################################################

library("caret")
library("dplyr")
library("foreign")

source("glm-and-cv.r")

raw_zawahri_mitchell_2011 <- read.dta("zawahri-mitchell-2011.dta")
doe_dyad <- read.csv("../R/results-predict-dyad.csv")

## Convert outcome to factor and drop multilateral basins
data_zawahri_mitchell_2011 <- raw_zawahri_mitchell_2011 %>%
    mutate(bilattreaty = factor(
               bilattreaty,
               levels = 0:1,
               labels = c("No", "Yes")
           )) %>%
    filter(bilatbasin == 1)

## Merge in DOE scores
##
## `upstreamstate` has three codings: A, B, and AB; we have upstreampower =
## cinc1 if upstreamstate = A or AB, and upstreampower = cinc2 otherwise
data_zawahri_mitchell_2011 <- data_zawahri_mitchell_2011 %>%
    left_join(doe_dyad,
              by = c(ccode1 = "ccode_a",
                     ccode2 = "ccode_b",
                     year = "year")) %>%
    mutate(VictoryUp = ifelse(upstreamstate == "B", VictoryB, VictoryA),
           VictoryDown = ifelse(upstreamstate == "B", VictoryA, VictoryB))
stopifnot(with(data_zawahri_mitchell_2011, sum(is.na(VictoryUp)) == 0))

## Remove cases where CINC is missing but DOE is not (appears to be post-2001
## cases)
data_zawahri_mitchell_2011 <- data_zawahri_mitchell_2011 %>%
    filter(!is.na(downstreampower),
           !is.na(upstreampower))

## Replicating Model 1
f_zawahri_mitchell_2011 <-
    bilattreaty ~ percentarealow + waterdependlow + avgpreciplow +
        lowpolity + samelegal + upstreampower + downstreampower

## Null hypothesis: no effect of river dependence
hyp_main <- c("percentarealow = 0",
              "waterdependlow = 0",
              "avgpreciplow = 0")

## Reproduce original model
set.seed(2611)
cr_zawahri_mitchell_2011 <- glm_and_cv(
    form = f_zawahri_mitchell_2011,
    data = data_zawahri_mitchell_2011,
    se_cluster = 1:nrow(data_zawahri_mitchell_2011),  # robust, not clustered
    hyp_main = hyp_main,
    hyp_power = c("upstreampower = 0", "downstreampower = 0"),
    number = 10,
    repeats = 100
)
print(cr_zawahri_mitchell_2011$summary)

## Replace raw CINC scores with corresponding DOE scores and run again
doe_zawahri_mitchell_2011 <- glm_and_cv(
    form = update(f_zawahri_mitchell_2011,
                  . ~ . - downstreampower - upstreampower + VictoryUp + VictoryDown),
    data = data_zawahri_mitchell_2011,
    se_cluster = 1:nrow(data_zawahri_mitchell_2011),  # robust, not clustered
    hyp_main = hyp_main,
    hyp_power = c("VictoryUp = 0", "VictoryDown = 0"),
    number = 10,
    repeats = 100
)
print(doe_zawahri_mitchell_2011$summary)

## Check that reponse is the same across runs
stopifnot(identical(as.vector(cr_zawahri_mitchell_2011$y),
                    as.vector(doe_zawahri_mitchell_2011$y)))

save(cr_zawahri_mitchell_2011,
     doe_zawahri_mitchell_2011,
     file = "results-zawahri-mitchell.rda")
