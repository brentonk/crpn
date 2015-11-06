################################################################################
###
### Compare DOE scores with capability ratios
###
################################################################################

library("MASS")
library("dplyr")
library("ggplot2")
library("ggtern")
library("RColorBrewer")
library("tidyr")
library("tikzDevice")

Sys.unsetenv("TEXINPUTS")

load("results-data-nmc.rda")
load("results-imputations-train.rda")

pred_dir_dyad <- read.csv("results-predict-dir-dyad.csv")
pred_dyad <- read.csv("results-predict-dyad.csv")


###-----------------------------------------------------------------------------
### Basic correlations
###-----------------------------------------------------------------------------

## Calculate capability ratios for all dyads
make_capratio <- function(pred) {
    pred %>%
        left_join(data_NMC %>% select(ccode, year, cinc_a = cinc),
                  by = c(ccode_a = "ccode", year = "year")) %>%
        left_join(data_NMC %>% select(ccode, year, cinc_b = cinc),
                  by = c(ccode_b = "ccode", year = "year")) %>%
        mutate(capratio = exp(cinc_a) / (exp(cinc_a) + exp(cinc_b)))
}

pred_dir_dyad <- make_capratio(pred_dir_dyad)
pred_dyad <- make_capratio(pred_dyad)

## Calculate correlations
with(pred_dir_dyad, cor(VictoryA, capratio, use = "complete"))
with(pred_dir_dyad, cor(VictoryB, capratio, use = "complete"))
with(pred_dyad, cor(VictoryA, capratio, use = "complete"))
with(pred_dyad, cor(VictoryB, capratio, use = "complete"))

## Calculate canonical correlations
with(pred_dir_dyad[!is.na(pred_dir_dyad$capratio), ],
     cancor(cbind(VictoryA, VictoryB), capratio))
with(pred_dyad[!is.na(pred_dyad$capratio), ],
     cancor(cbind(VictoryA, VictoryB), capratio))


###-----------------------------------------------------------------------------
### In-sample predicted probabilities
###-----------------------------------------------------------------------------

## Re-running the capability ratio model to avoid having to load the huge
## ensemble into memory
##
## No need to combine across imputations since there's no missingness in the
## capability ratio
polr_capratio <- polr(Outcome ~ capratio,
                      data = imputations_train[[1]])

## Calculate predicted probabilities from capability ratio model
pp_capratio <- predict(polr_capratio, type = "prob") %>%
    data.frame() %>%
    select(VictoryA, Stalemate, VictoryB) %>%
    mutate(method = "capratio")

## Merge in DOE scores
pp_doe <- imputations_train[[1]] %>%
    left_join(pred_dir_dyad, by = c("ccode_a", "ccode_b", "year")) %>%
    select(VictoryA, Stalemate, VictoryB) %>%
    mutate(method = "doe")

## Make data to pass to ggplot
pp_data <- rbind(pp_capratio, pp_doe)
pp_data$Outcome <- factor(imputations_train[[1]]$Outcome,
                          levels = c("VictoryA", "Stalemate", "VictoryB"),
                          labels = c("A Wins", "Stalemate", "B Wins"))
pp_data$method <- factor(pp_data$method,
                         levels = c("capratio", "doe"),
                         labels = c("Ordered Logit on Capability Ratio",
                                    "Super Learner"))

tikz(file = file.path("..", "latex", "fig-in-sample.tex"),
     width = 5,
     height = 4)
ggtern(pp_data,
       aes(x = VictoryA,
           y = Stalemate,
           z = VictoryB)) +
    geom_point(aes(shape = Outcome,
                   colour = Outcome),
               alpha = 0.3) +
    scale_colour_brewer("Observed Outcome",
                        palette = "Set1") +
    scale_shape("Observed Outcome") +
    scale_T_continuous("$\\emptyset$") +
    scale_L_continuous("$A$") +
    scale_R_continuous("$B$") +
    facet_wrap(~ method) +
    guides(colour = guide_legend(override.aes = list(alpha = 1))) +
    theme_grey(base_size = 10) +
    theme(axis.tern.ticks = element_blank(),
          axis.tern.text = element_text(size = rel(0.8)),
          legend.position = "bottom")
dev.off()

## Version for slides
tikz(file = file.path("..", "slides", "fig-in-sample.tex"),
     width = 4.25,
     height = 3.25)
ggtern(pp_data,
       aes(x = VictoryA,
           y = Stalemate,
           z = VictoryB)) +
    geom_point(aes(shape = Outcome,
                   colour = Outcome),
               alpha = 0.3) +
    scale_colour_brewer("Observed Outcome",
                        palette = "Set1") +
    scale_shape("Observed Outcome") +
    scale_T_continuous("$\\emptyset$") +
    scale_L_continuous("$A$") +
    scale_R_continuous("$B$") +
    facet_wrap(~ method) +
    guides(colour = guide_legend(override.aes = list(alpha = 1))) +
    theme_grey(base_size = 8) +
    theme(axis.tern.ticks = element_blank(),
          axis.tern.text = element_text(size = rel(0.8)),
          plot.background = element_rect(fill = "transparent", colour = NA),
          legend.background = element_rect(fill = "transparent", colour = NA),
          legend.position = "bottom")
dev.off()

###-----------------------------------------------------------------------------
### Dyadic predictions over time
###-----------------------------------------------------------------------------

## Extract the USA-Russia dyadic data
##
## Using the undirected scores since we don't want to presume an initiator
pp_usa_russia_doe <- pred_dyad %>%
    filter(ccode_a == 2,
           ccode_b == 365) %>%
    mutate(capratio = log(capratio))

## Calculate predicted probabilities under the capability ratio model
##
## Need to do it twice to average, same way the undirected DOE scores are
## calculated
pp_usa_russia_cr_a <- predict(polr_capratio,
                              newdata = pp_usa_russia_doe,
                              type = "prob")
pp_usa_russia_cr_b <- predict(polr_capratio,
                              newdata = pp_usa_russia_doe %>%
                                  mutate(capratio = log(1 - exp(capratio))),
                              type = "prob")
pp_usa_russia_cr <- 0.5 * pp_usa_russia_cr_a + 0.5 * pp_usa_russia_cr_b[, 3:1]
stopifnot(all.equal(range(rowSums(pp_usa_russia_cr)), c(1, 1)))

## Format data for ggplot
dat_usa_russia_doe <- pp_usa_russia_doe %>%
    select(year, VictoryUS = VictoryA, Stalemate, VictoryRussia = VictoryB) %>%
    mutate(method = "doe")
dat_usa_russia_cr <- pp_usa_russia_cr %>%
    data.frame() %>%
    select(VictoryUS = VictoryA, Stalemate, VictoryRussia = VictoryB) %>%
    mutate(year = pp_usa_russia_doe$year, method = "capratio")

dat_usa_russia <- rbind(dat_usa_russia_cr, dat_usa_russia_doe) %>%
    gather(quantity, probability, VictoryUS:VictoryRussia) %>%
    mutate(method = factor(method,
                           levels = c("capratio", "doe"),
                           labels = c("Ordered Logit on Capability Ratio",
                                      "Super Learner")),
           quantity = factor(quantity,
                             levels = c("VictoryUS", "Stalemate", "VictoryRussia"),
                             labels = c("USA Wins", "Stalemate", "Russia Wins")))

tikz(file = file.path("..", "latex", "fig-vs.tex"),
     width = 5,
     height = 3.5,
     packages = c(getOption("tikzLatexPackages"),
                  "\\usepackage{amsmath}"))
ggplot(dat_usa_russia, aes(x = year, y = probability)) +
    geom_area(aes(fill = quantity), alpha = 0.8) +
    facet_wrap(~ method) +
    scale_x_continuous("Year") +
    scale_y_continuous("Probability") +
    scale_fill_manual("",
                      values = rev(brewer.pal(3, "Blues"))) +
    theme_grey(base_size = 10) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "bottom",
          legend.box = "horizontal")
dev.off()
