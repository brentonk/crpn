################################################################################
###
### Replication of Uzonyi, Souva and Golder 2012, "Domestic Institutions and 
### Credible Signals," replacing CINC scores with DOE scores
###
### Replicating Table 3, Model 3 (Weeks and ACC in same model)
###
################################################################################

library("caret")
library("dplyr")
library("foreign")

source("glm-and-cv.r")

data_usg_2012 <- read.dta("uzonyi-souva-golder-2012.dta")
doe_dir_dyad <- read.csv("../R/results-predict-dir-dyad.csv")

## drop droppables
keeper <- data_usg_2012$year > 1945 & 
  data_usg_2012$year <= 2000 & 
  data_usg_2012$originator == 1
data_usg_2012 <- data_usg_2012[keeper,]
rm(keeper)

## convert DV to factor
data_usg_2012$cowrecipx <- factor(data_usg_2012$cowrecipx,
                                  levels = 0:1,
                                  labels = c("No", "Yes"))

## run the reported model with cross validation
set.seed(90210)
f_usg_2012 <- 
  cowrecipx ~ acc1nomvx + personal1w + single1w + military1w + hybrid1w +
  other1w + dynastic1w + nondynastic1w + interregnaw + interregnademw + majmaj +
  minmaj + majmin + powerratio + allies + s_wt_glo + s_ld_1 + s_ld_2 + 
  contig400 + terr1 + regime1 + policy1 + other1 
cr_usg_2012 <- glm_and_cv(
  form = f_usg_2012,
  data = data_usg_2012,
  number = 10,
  repeats = 100
)
printCoefmat(cr_usg_2012$summary)

## merge in the DOE data
data_usg_2012 <- left_join(data_usg_2012,
                                doe_dir_dyad,
                                by = c(ccode1 = "ccode_a",
                                       ccode2 = "ccode_b",
                                       year = "year"))

## DOE in every row
length(which(is.na(data_usg_2012$VictoryA)))

## run it with DOE
set.seed(8032)
doeForm <- update(f_usg_2012,
                  . ~ . - powerratio + VictoryA + VictoryB)
doe_usg_2012 <- glm_and_cv(
  form = doeForm,
  data = data_usg_2012,
  number = 10,
  repeats = 100
)
printCoefmat(doe_usg_2012$summary)

save(cr_usg_2012,
     doe_usg_2012,
     file = "results-uzonyi-souva-golder-2012.rda")
