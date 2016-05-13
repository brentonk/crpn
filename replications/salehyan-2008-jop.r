################################################################################
###
### Replication of Salehyan 2008, "No Shelter Here," 
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

data_salehyan_2008_jop <- read.dta("salehyan-2008-jop.dta")
doe_dyad <- read.csv("../R/results-predict-dyad.csv")

## add the correct dependent variable
data_salehyan_2008_jop$DV <- ifelse(data_salehyan_2008_jop$cwhostd >= 4, 1, 0)

## switch DV to factor for caret
data_salehyan_2008_jop$DV <- factor(data_salehyan_2008_jop$DV,
                                    levels = 0:1,
                                    labels = c("No", "Yes"))

## merge in the DOE scores and compute pairwise max and min (since A and B are
## exchangeable in undirected data)
data_salehyan_2008_jop <- left_join(data_salehyan_2008_jop,
                                    doe_dyad,
                                    by = c(ccode1 = "ccode_a",
                                           ccode2 = "ccode_b",
                                           year = "year")) %>%
    mutate(VictoryMax = pmax(VictoryA, VictoryB),
           VictoryMin = pmin(VictoryA, VictoryB))

## DOE scores in every row:
stopifnot(with(data_salehyan_2008_jop, !any(is.na(VictoryA))))

## Remove cases where CINC is missing but DOE is not
##
## In particular, there are two cases where cinc1 == cinc2, both observed, but
## lcaprat is NA
data_salehyan_2008_jop <- data_salehyan_2008_jop %>%
    filter(!is.na(lcaprat))

## Replicating Model 1
f_salehyan_2008_jop <- 
    DV ~ extbase + intriv + lcaprat + defpact + politylo + peaceyears + 
        s1 + s2 + s3

## Null hypothesis: no effect of external base
hyp_main <- "extbase = 0"

## run the reported model with cross validation
set.seed(44444)
cr_salehyan_2008_jop <- glm_and_cv(
    form = f_salehyan_2008_jop,
    data = data_salehyan_2008_jop,
    se_cluster = data_salehyan_2008_jop$dyad,
    hyp_main = hyp_main,
    hyp_power = "lcaprat = 0",
    number = 10,
    repeats = 100
)
print(cr_salehyan_2008_jop$summary)

doe_salehyan_2008_jop <- glm_and_cv(
    form = update(f_salehyan_2008_jop,
                  . ~ . - lcaprat + log(VictoryMax) + log(VictoryMin)),
    data = data_salehyan_2008_jop,
    se_cluster = data_salehyan_2008_jop$dyad,
    hyp_main = hyp_main,
    hyp_power = c("log(VictoryMax) = 0", "log(VictoryMin) = 0"),
    number = 10,
    repeats = 100
)
print(doe_salehyan_2008_jop$summary)

## Confirm that response is the same across runs
stopifnot(identical(as.vector(cr_salehyan_2008_jop$y),
                    as.vector(doe_salehyan_2008_jop$y)))

save(cr_salehyan_2008_jop,
     doe_salehyan_2008_jop,
     file = "results-salehyan-2008-jop.rda")
