
library(car)
library(sandwich)
library(foreach)
library(MASS)
library(dplyr)
library(tidyr)
library(foreign)
library(magrittr)
library(sqldf)
library(caret)
library(broom)
library(xtable)
library(pscl)
library(tikzDevice)

dat <- as.tbl(read.dta("reed-et-al-2008.dta"))

### many cases with NAs for all variables.  year and the DV
### are the best indicators of this
dat %<>% filter(!is.na(year) & !is.na(cwmid))

### rename splines
names(dat)[names(dat) == "_spline1"] <- "spline1"
names(dat)[names(dat) == "_spline2"] <- "spline2"
names(dat)[names(dat) == "_spline3"] <- "spline3"

### run original models to check replication quality.
f_1 <- cwmid ~ demlow + logdist + zqpb_1 + zqpb2_1
f_2 <- update(f_1,
              . ~ . + alliance + twomaj +
                peaceyrs + spline1 + spline2 + spline3)

### their model 1
mod1 <- glm(formula = f_1,
            data = dat,
            model = FALSE,
            family = binomial(link = "probit"))

### their model 2
mod2 <- glm(formula = f_2,
            data = dat,
            model = FALSE,
            family = binomial(link = "probit"))

### need to construct the variables
dat %<>%
  mutate(A_ccode = sprintf("%003d", ccode1),
         B_ccode = sprintf("%003d", ccode2),
         ud = paste(A_ccode, B_ccode, sep = "_"),
         udyear = paste(A_ccode, B_ccode, year, sep = "_")) %>%
  select(-A_ccode, -B_ccode)

points <- as.tbl(read.dta("idealpoint4600.dta")) %>%
  mutate(A_ccode = sprintf("%003d", ccode1),
         B_ccode = sprintf("%003d", ccode2),
         udyear = paste(A_ccode, B_ccode, year, sep = "_"),
         qhat = ifelse(meana > meanb, meana - meanb, meanb - meana)) %>%
  select(-meana, -lowa, -higha, -meanb, -lowb, -highb)

foo <- as.tbl(read.csv("../data/NMC_v4_0.csv")) %>%
  select(ccode, year, cinc) %>%
  filter(year %in% sort(unique(dat$year)))

nmc <- as.tbl(sqldf("select A.ccode A_ccode, B.ccode B_ccode, A.cinc A_cinc,
                     B.cinc B_cinc, year
                     from foo A join foo B using (year)
                     where A.ccode < B.ccode
                     order by A.ccode, year")) %>%
  mutate(A_ccode = sprintf("%003d", A_ccode),
         B_ccode = sprintf("%003d", B_ccode),
         udyear = paste(A_ccode, B_ccode, year, sep = "_"),
         phat = ifelse(A_cinc > B_cinc,
                       A_cinc - B_cinc, B_cinc - A_cinc)) %>%
  select(udyear, phat)
rm(foo)

points <- left_join(points, nmc, by = "udyear") %>%
  select(udyear, qhat, phat)
rm(nmc)

doe <- as.tbl(read.csv("../R/results-predict-dyad.csv")) %>%
  mutate(A_ccode = sprintf("%003d", ccode_a),
         B_ccode = sprintf("%003d", ccode_b),
         udyear = paste(A_ccode, B_ccode, year, sep = "_")) %>%
  filter(udyear %in% points$udyear) %>%
  select(-ccode_a, -ccode_b, -year, -A_ccode, -B_ccode, udyear, VictoryB,
         Stalemate, VictoryB)

points <- left_join(points, doe, by = "udyear")
rm(doe)

points %<>%
  mutate(Qstd = qhat / max(qhat),
         Pstd = phat / max(phat),
         doePhat1 = abs(VictoryA - VictoryB),
         doePhat2 = abs(VictoryA - VictoryB) / abs(VictoryA + VictoryB),
         doePstd1 = doePhat1 / max(doePhat1),
         doePstd2 = doePhat2 / max(doePhat2),
         myAbs = abs(Qstd - Pstd),
         mySqu = (Qstd - Pstd)^2)

dat <- left_join(dat, points, by = "udyear") %>%
  arrange(udyear)

dat %<>%
  group_by(ud) %>%
  mutate(myAbs_lag = lag(myAbs, order_by = year),
         mySqu_lag = lag(mySqu, order_by = year)) %>%
  ungroup()

### check correlations
cor(dat$myAbs_lag, dat$zqpb_1, use = "complete")
cor(dat$mySqu_lag, dat$zqpb2_1, use = "complete")

### make doe versions, run models
dat %<>%
  mutate(doeAbs1 = abs(Qstd - doePhat1),
         doeSqu1 = (Qstd - doePhat1)^2,
         doeAbs2 = abs(Qstd - doePhat2),
         doeSqu2 = (Qstd - doePhat2)^2) %>%
  group_by(ud) %>%
  mutate(doeAbs1_lag = lag(doeAbs1, order_by = year),
         doeSqu1_lag = lag(doeSqu1, order_by = year),
         doeAbs2_lag = lag(doeAbs2, order_by = year),
         doeSqu2_lag = lag(doeSqu2, order_by = year)) %>%
    ungroup()

## re-run
f_1 <- update(f_1,
              . ~ . - zqpb_1 - zqpb2_1 + myAbs_lag + mySqu_lag)
f_2 <- update(f_2,
              . ~ . - zqpb_1 - zqpb2_1 + myAbs_lag + mySqu_lag)

mod1 <- glm(formula = f_1,
            data = dat,
            model = FALSE,
            family = binomial(link = "probit"))

mod2 <- glm(formula = f_2,
            data = dat,
            model = FALSE,
            family = binomial(link = "probit"))

## Subset to only those observations used in the original paper *and* for which
## DOE scores are non-missing
##
## Entails dropping cases involving the Maldives (ccode 781) in 1971 -- there
## don't seem to be ideal points for the Maldives in 1970 yet these enter the
## original analysis
dat_sub <- dat[-na.action(mod1), ] %>%
    filter(!is.na(VictoryA))

mod1 <- glm(formula = f_1,
            data = dat_sub,
            model = TRUE,
            family = binomial(link = "probit"))

mod2 <- glm(formula = f_2,
            data = dat_sub,
            model = FALSE,
            family = binomial(link = "probit"))
stopifnot(nobs(mod2) == nobs(mod1))

df_1 <- update(f_1,
               . ~ . - myAbs_lag - mySqu_lag + doeAbs1_lag + doeSqu1_lag)
df_2 <- update(f_2,
               . ~ . - myAbs_lag - mySqu_lag + doeAbs1_lag + doeSqu1_lag)

d.mod1 <- glm(formula = df_1,
               data = dat_sub,
               model = TRUE,
               family = binomial(link = "probit"))
stopifnot(nobs(d.mod1) == nobs(mod1))

d.mod2 <- glm(formula = df_2,
               data = dat_sub,
               model = FALSE,
               family = binomial(link = "probit"))
stopifnot(nobs(d.mod2) == nobs(mod1))

## In-sample fit
c("mod1" = AIC(mod1),
  "d.mod1" = AIC(d.mod1),
  "mod2" = AIC(mod2),
  "d.mod2" = AIC(d.mod2))

### in case you want Vuong
print(vuong(d.mod1, mod1))
print(vuong(d.mod2, mod2))

### summarizing the two versions of the two variables
toSum <- select(dat_sub,
                myAbs_lag, doeAbs1_lag, mySqu_lag, doeSqu1_lag) %>%
  rename("RCNW_abs" = myAbs_lag,
         "DOE_abs" = doeAbs1_lag,
         "RCNW_squ" = mySqu_lag,
         "DOE_squ" = doeSqu1_lag) %>%
  gather(key, value) %>%
  separate(key, c("measure", "variable")) %>%
  mutate(measure = ifelse(measure == "RCNW", "Reed et al. (2008)",
                          "DOE Replication"),
         measure = factor(measure, levels = c("Reed et al. (2008)",
                                              "DOE Replication")),
         variable = ifelse(variable == "abs", "Absolute Difference",
                           "Squared Difference"))


### plotting the marginal effects, per RCNW Figure 4
dat2plot <- expand.grid(p = seq(0, 1, length.out = 101),
                        q = c(0.1, 0.5, 0.9))
dat2plot$demlow <- min(dat$demlow, na.rm = TRUE)
dat2plot$logdist <- min(dat$logdist, na.rm = TRUE)
dat2plot %<>%
    mutate(myAbs_lag = abs(p - q),
           mySqu_lag = (p-q)^2,
           doeAbs1_lag = myAbs_lag,
           doeSqu1_lag = mySqu_lag)

dat2plot$rcnw.pp <- predict(mod1, newdata = dat2plot, type = "response")
dat2plot$doe.pp <- predict(d.mod1, newdata = dat2plot, type = "response")

set.seed(42)
X_rcnw <- model.matrix(formula(delete.response(terms(formula(mod1)))),
                       data = dat2plot)
B_rcnw <- mvrnorm(1000,
                  mu = coef(mod1),
                  Sigma = vcovHC(mod1))
XB_rcnw <- X_rcnw %*% t(B_rcnw)
pp_rcnw <- pnorm(drop(XB_rcnw))
dat2plot$rcnw.low <- apply(pp_rcnw, 1, quantile, 0.025)
dat2plot$rcnw.high <- apply(pp_rcnw, 1, quantile, 0.975)

X_d1 <- model.matrix(formula(delete.response(terms(formula(d.mod1)))),
                     data = dat2plot)
B_d1 <- mvrnorm(1000,
                mu = coef(d.mod1),
                Sigma = vcovHC(d.mod1))
XB_d1 <- X_d1 %*% t(B_d1)
pp_d1 <- pnorm(drop(XB_d1))
dat2plot$doe.low <- apply(pp_d1, 1, quantile, 0.025)
dat2plot$doe.high <- apply(pp_d1, 1, quantile, 0.975)

dat2plot <- as.tbl(dat2plot) %>%
  select(-demlow, -logdist, -myAbs_lag, -mySqu_lag, -doeAbs1_lag, -doeSqu1_lag)

plot_data <- dat2plot %>%
    gather(model, prediction, rcnw.pp:doe.high) %>%
    separate(model, into = c("model", "quantity")) %>%
    spread(quantity, prediction) %>%
  mutate(model = ifelse(model == "doe", "DOE Replication",
                        "Reed et al. (2008)"),
         model = factor(model, levels = c("Reed et al. (2008)",
                                          "DOE Replication")),
         q = paste0("$q = ", q, "$"))

tikz(file = "../latex/fig-rcnw-gull.tex",
     width = 5.4,
     height = 4.5)
ggplot(plot_data,
       aes(x = p, y = pp)) +
    geom_ribbon(aes(ymin = low, ymax = high),
                fill = "gray80") +
    geom_line() +
    facet_grid(q ~ model) +
    theme_bw() +
    scale_x_continuous("Distribution of Power") +
    scale_y_continuous("Predicted Probability of Conflict") +
    theme_grey(base_size = 10) +
    theme(panel.grid.minor = element_blank(),
          axis.text = element_text(size = rel(0.8)),
          axis.title.y = element_text(margin = margin(0, 10, 0, 0)))
dev.off()


## Save fitted models to pass on to the cross-validation script
save(mod1,
     d.mod1,
     file = "results-reed-et-al.rda")
