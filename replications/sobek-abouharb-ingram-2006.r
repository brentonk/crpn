################################################################################
###
### Replication of Sobek, Abouharb, and Ingram, "Human Rights Peace," 
### replacing CINC ratio with DOE scores
###
### Replicating Table 2, Index Model
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

## merge in the DOE scores and compute pairwise max and min
data_sai_2006 <- left_join(data_sai_2006,
                           doe_dyad,
                           by = c(ccode1 = "ccode_a",
                                  ccode2 = "ccode_b",
                                  year = "year")) %>%
    mutate(VictoryMax = pmax(VictoryA, VictoryB),
           VictoryMin = pmin(VictoryA, VictoryB))

### VictoryA is in all rows:
stopifnot(with(data_sai_2006, !any(is.na(VictoryA))))

## replication formula
f_sai_2006 <-
    cwmid ~ demlow + demhigh + caprat + ally + disimciv + s_un_glo + pol_rel +
        contig + physintlow + empinxlow + peaceyearsmid + spline1mid +
        spline2mid + spline3mid

## Null hypothesis: no effect of phyical integrity or empowerment rights
hyp_main <- c("physintlow = 0", "empinxlow = 0")

## run the reported model with cross validation
set.seed(435390)
cr_sai_2006 <- glm_and_cv(
    form = f_sai_2006,
    data = data_sai_2006,
    se_cluster = data_sai_2006$dyad,
    hyp_main = hyp_main,
    hyp_power = "caprat = 0",
    number = 10,
    repeats = 10
)
print(cr_sai_2006$summary)

## run it with DOE
doe_sai_2006 <- glm_and_cv(
    form = update(f_sai_2006,
                  . ~ . - caprat + VictoryMax + VictoryMin),
    data = data_sai_2006,
    se_cluster = data_sai_2006$dyad,
    hyp_main = hyp_main,
    hyp_power = c("VictoryMax = 0", "VictoryMin = 0"),
    number = 10,
    repeats = 10
)
print(doe_sai_2006$summary)

## save the results
save(cr_sai_2006,
     doe_sai_2006,
     file = "results-sobek-abouharb-ingram-2006.rda")
