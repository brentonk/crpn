library("broom")
library("caret")
library("dplyr")
library("magrittr")
library("tidyr")
library("xtable")

load("results-reed-et-al.rda")


## cross validate
myControl <- trainControl(classProbs = TRUE,
                          verboseIter = TRUE,
                          allowParallel = FALSE,
                          summaryFunction = mnLogLoss,
                          trim = TRUE,
                          method = "repeatedcv",
                          number = 10,
                          repeats = 100,
                          savePredictions = FALSE,
                          returnData = FALSE)

dat_mod1 <- model.frame(mod1)
dat_mod1$cwmid <- factor(dat_mod1$cwmid,
                         levels = 0:1,
                         labels = c("No", "Yes"))

dat_d.mod1 <- model.frame(d.mod1)
dat_d.mod1$cwmid <- factor(dat_d.mod1$cwmid,
                           levels = 0:1,
                           labels = c("No", "Yes"))

set.seed(337875672)
cv.null <- train(cwmid ~ 1,
                 data = dat_mod1,
                 method = "glm",
                 family = binomial(link = "probit"),
                 metric = "logLoss",
                 trControl = myControl)

cv.mod1 <- train(cwmid ~ .,
                 data = dat_mod1,
                 method = "glm",
                 family = binomial(link = "probit"),
                 metric = "logLoss",
                 trControl = myControl)

cv.d.mod1 <- train(cwmid ~ .,
                   data = dat_d.mod1,
                   method = "glm",
                   family = binomial(link = "probit"),
                   metric = "logLoss",
                   trControl = myControl)

### start making a table
tid <- tidy(mod1) %>%
  select(-statistic, -p.value) %>%
  filter(!(term %in% c("spline1", "spline2", "spline3"))) %>%
  rename("est" = estimate, "se" = std.error)
d.tid <- tidy(d.mod1) %>%
  select(-statistic, -p.value) %>%
  filter(!(term %in% c("spline1", "spline2", "spline3"))) %>%
  rename("d.est" = estimate, "d.se" = std.error) %>%
  mutate(term = ifelse(term == "doeAbs1_lag", "myAbs_lag", term),
         term = ifelse(term == "doeSqu1_lag", "mySqu_lag", term))

myTab <- left_join(tid, d.tid, by = "term")
myTab$term <- factor(myTab$term,
                     levels = c("demlow",
                                "logdist",
                                "myAbs_lag",
                                "mySqu_lag",
                                "(Intercept)"),
                     labels = c("Democracy",
                                "ln(Distance)",
                                "$|q - p|_{t - 1}$",
                                "$(q - p)_{t - 1}^2$",
                                "Intercept"))

myTab %<>%
  arrange(term) %>%
  mutate_each(funs(round(., 3)), -term)

### finishing up the table
mod1.prl <- (cv.null$results$logLoss - cv.mod1$results$logLoss) /
  cv.null$results$logLoss
d.mod1.prl <- (cv.null$results$logLoss - cv.d.mod1$results$logLoss) /
  cv.null$results$logLoss

myTab %<>%
  rename("Coefficient" = est,
         "S.E." = se,
         "Coefficient" = d.est,
         "S.E." = d.se,
         "Variable" = term)

myXTab <- xtable(myTab, align = c("l", "l", "r", "r", "r", "r"), digits = 3)

toTop <- paste0("\\toprule ",
               "& \\multicolumn{2}{c}{Reed et al. (2008)} & \\multicolumn{2}{c}{DOE Replication} ",
               "\\\\ \n",
               "\\cmidrule(l{.2em}r{.2em}){2-3} \\cmidrule(l{.2em}r{.2em}){4-5} %")

toBottom <- paste0("\\midrule \n",
                   "$N$ & \\multicolumn{2}{c}{",
                   prettyNum(nobs(mod1), big.mark = ","),
                   "} & \\multicolumn{2}{c}{",
                   prettyNum(nobs(d.mod1), big.mark = ","), "} \\\\ \n",
                   "AIC & \\multicolumn{2}{c}{", round(mod1$aic, 3), "}",
                   " & \\multicolumn{2}{c}{", round(d.mod1$aic, 3), "} \\\\ \n",
                   "PRL & \\multicolumn{2}{c}{", round(mod1.prl, 3), "}",
                   " & \\multicolumn{2}{c}{", round(d.mod1.prl, 3), "} \\\\ \n")

print(myXTab,
      include.rownames = FALSE,
      booktabs = TRUE,
      floating = FALSE,
      sanitize.text.function = identity,
      math.style.negative = TRUE,
      add.to.row = list(
        pos = list(-1, 5),
        command = c(toTop, toBottom)
      ),
      file = "../latex/tab-rcnw.tex")
