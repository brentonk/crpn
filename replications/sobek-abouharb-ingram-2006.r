################################################################################
###
### Replication of Sobek, Abouharb, and Ingram, "Human Rights Peace," 
### replacing CINC ratio with DOE scores
###
### Replicating Table 1, Row 1
###
################################################################################

library("caret")
library("dplyr")
library("foreign")
library("MASS")

source("glm-and-cv.r")

data_sai_2006 <- read.dta("sobek-abouharb-ingram-2006.dta")
doe_dyad <- read.csv("../R/results-predict-dyad.csv")

## rename the splines that start with an underscore
names(data_sai_2006)[names(data_sai_2006) == "_spline1mid"] <- "spline1mid"
names(data_sai_2006)[names(data_sai_2006) == "_spline2mid"] <- "spline2mid"
names(data_sai_2006)[names(data_sai_2006) == "_spline3mid"] <- "spline3mid"

## switch DV to a factor
data_sai_2006$cwmid <- factor(data_sai_2006$cwmid,
                              levels = 0:1,
                              labels = c("No", "Yes"))

## replication formula
f_sai_2006 <-
  cwmid ~ movelow + demlow + demhigh + caprat + disimciv + s_un_glo + contig + 
  pol_rel + ally + peaceyearsmid + spline1mid + spline2mid + spline3mid

## run the reported model with cross validation
set.seed(90210)
cr_sai_2006 <- glm_and_cv(
  form = f_sai_2006,
  data = data_sai_2006,
  number = 10,
  repeats = 10
)
printCoefmat(cr_sai_2006$summary)

## merge in the DOE scores and compute pairwise max and min
data_sai_2006 <- left_join(data_sai_2006,
                                doe_dyad,
                                by = c(ccode1 = "ccode_a",
                                       ccode2 = "ccode_b",
                                       year = "year")) %>%
    mutate(VictoryMax = pmax(VictoryA, VictoryB),
           VictoryMin = pmin(VictoryA, VictoryB))

### VictoryA is in all rows:
length(which(is.na(data_sai_2006$VictoryA)))

## run it with DOE
set.seed(8032)
doeForm <- update(f_sai_2006,
                  . ~ . - caprat + VictoryMax + VictoryMin)
doe_sai_2006 <- glm_and_cv(
  form = doeForm,
  data = data_sai_2006,
  number = 10,
  repeats = 10
)
printCoefmat(doe_sai_2006$summary)

## save the results
save(cr_sai_2006,
     doe_sai_2006,
     file = "results-sobek-abouharb-ingram-2006.rda")
