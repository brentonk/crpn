################################################################################
###
### Compare OINC scores with capability ratios
###
################################################################################

library("dplyr")
library("ggplot2")
library("RColorBrewer")
library("tidyr")
library("tikzDevice")

Sys.unsetenv("TEXINPUTS")

load("results-data-nmc.rda")

pred_dir_dyad <- read.csv("results-predict-dir-dyad.csv")
pred_dyad <- read.csv("results-predict-dyad.csv")

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

## Plot each quantity for all pairings of:
##   * USA (2)
##   * UK (200)
##   * Russia (365)
##   * China (710)
##   * Japan (740)
## Want to use undirected dyads for this since the capability ratio isn't
## initiator-specific
countries <- c(USA = 2, UK = 200, Russia = 365, China = 710, Japan = 740)
plot_data <-
    expand.grid(ccode_a = countries,
                ccode_b = countries,
                year = 1860:2007) %>%
    left_join(pred_dyad,
              by = c("ccode_a", "ccode_b", "year")) %>%
    select(-cinc_a, -cinc_b) %>%
    mutate_each(funs(factor(.,
                            levels = countries,
                            labels = names(countries))),
                ccode_a,
                ccode_b)

tikz(file = file.path("..", "latex", "fig-vs.tex"),
     width = 5,
     height = 4.5,
     packages = c(getOption("tikzLatexPackages"),
                  "\\usepackage{amsmath}"))
plot_data %>%
    gather(quantity, probability, VictoryA:VictoryB) %>%
    mutate(ccode_a = factor(ccode_a,
                            labels = paste("1:", levels(ccode_a))),
           ccode_b = factor(ccode_b,
                            labels = paste("2:", levels(ccode_b)))) %>%
    ggplot(aes(x = year, y = probability)) +
    geom_area(aes(fill = quantity),
              alpha = 0.8) +
    geom_line(aes(y = capratio, colour = factor(1))) +
                                        # Created a dummy aesthetic mapping to
                                        # get the capability ratio to show up in
                                        # the legend
    facet_grid(ccode_b ~ ccode_a) +
    scale_x_continuous("Year",
                       breaks = c(1900, 2000)) +
    scale_y_continuous("Probability") +
    scale_fill_manual(values = rev(brewer.pal(3, "Blues")),
                      labels = c("Pr(1 Wins)", "Pr(Stalemate)", "Pr(2 Wins)")) +
    scale_colour_manual(values = "black",
                        labels = "$\\text{CINC}_1/(\\text{CINC}_1 + \\text{CINC}_2)$") +
    guides(fill = guide_legend(reverse = TRUE, title = NULL, order = 1),
           colour = guide_legend(title = NULL, order = 2)) +
    theme_grey(base_size = 10) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "bottom",
          legend.box = "horizontal")
dev.off()

## Make simpler version for slides: just USA vs Russia
tikz(file = file.path("..", "slides", "fig-usa-russia.tex"),
     width = 4.25,
     height = 3.25,
     packages = c(getOption("tikzLatexPackages"),
                  "\\usepackage{amsmath}"))
plot_data %>%
    filter(ccode_a == "USA", ccode_b == "Russia") %>%
    gather(quantity, probability, VictoryA:VictoryB) %>%
    ggplot(aes(x = year, y = probability)) +
    geom_area(aes(fill = quantity),
              alpha = 0.8) +
    geom_line(aes(y = capratio, colour = factor(1))) +
    scale_x_continuous("Year") +
    scale_y_continuous("Probability") +
    scale_fill_manual("DOE",
                      values = rev(brewer.pal(3, "Blues")),
                      labels = c("Pr(US Wins)", "Pr(Stalemate)", "Pr(Russia Wins)")) +
    scale_colour_manual("CINC",
                        values = "black",
                        labels = "Capability Ratio") +
    ggtitle("Hypothetical US--Russian Dispute") +
    theme_grey(base_size = 8) +
    theme(plot.background = element_rect(fill = "transparent", colour = NA),
          legend.background = element_rect(fill = "transparent", colour = NA))
dev.off()
