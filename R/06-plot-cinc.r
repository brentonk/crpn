################################################################################
###
### Plot dispute outcomes as a function of CINC scores to show what a
### restrictive decision rule the capability ratio is
###
################################################################################

library("caret")
library("dplyr")
library("ggplot2")
library("np")
library("RColorBrewer")
library("tikzDevice")

Sys.unsetenv("TEXINPUTS")

load("results-imputations-train.rda")

## Only use the training data, since this is being presented before we show the
## trained results
##
## All we need is each state's CINC score and the outcome
##
## Converting to log10 for ease of plot axis interpretability
cinc_data <- imputations_train[[1]] %>%
    filter(Outcome != "Stalemate") %>%
    select(Outcome, cinc_a, cinc_b) %>%
    mutate(Outcome = as.character(Outcome),
           Outcome = gsub("Victory", "", Outcome)) %>%
    mutate_each(funs(./log(10)), cinc_a, cinc_b)

## Want same limits in all plots, since they'll be overlaid
cmin <- with(cinc_data, min(c(cinc_a, cinc_b)))
cmax <- with(cinc_data, max(c(cinc_a, cinc_b)))
eps <- 0.1
lims <- c(cmin - eps, cmax + eps)

## Define other common plot elements
tikz_width <- 4.25
tikz_height <- 3.25
tikz_size <- 8
tikz_theme <-
    theme(plot.background = element_rect(fill = "transparent", colour = NA),
          legend.background = element_rect(fill = "transparent", colour = NA))
tikz_package = c(getOption("tikzLatexPackages"),
                 "\\usepackage{amsmath}",
                 "\\usepackage{amssymb}")

plot_base <-
    ggplot(cinc_data,
           aes(x = cinc_a, y = cinc_b)) +
    scale_shape_manual("\\parbox{0.9in}{Outcome}",
                       values = c(16, 21),
                       labels = c("A Wins", "B Wins")) +
    scale_x_continuous("$\\log_{10}(\\mbox{CINC}_A)$",
                       limits = lims) +
    scale_y_continuous("$\\log_{10}(\\mbox{CINC}_B)$",
                       limits = lims) +
    theme_grey(base_size = tikz_size) +
    tikz_theme

## Plot raw data, with and without decision boundary
plot_cinc <- plot_base +
    geom_point(aes(shape = Outcome),
               size = 2.5,
               alpha = 0.7,
               fill = "white")

tikz(file = file.path("..", "slides", "fig-cinc-pts.tex"),
     width = tikz_width,
     height = tikz_height,
     packages = tikz_package)
print(
    plot_cinc
)
dev.off()

tikz(file = file.path("..", "slides", "fig-cinc-pts-bd.tex"),
     width = tikz_width,
     height = tikz_height,
     packages = tikz_package)
print(
    plot_cinc +
        geom_abline(intercept = 0)
)
dev.off()

## Also plot with indifference curves
tikz(file = file.path("..", "slides", "fig-cinc-pts-indiff.tex"),
     width = tikz_width,
     height = tikz_height,
     packages = tikz_package)
print(
    plot_cinc +
        geom_abline(intercept = seq(-4, 4, by = 4/3),
                    size = 2,
                    alpha = 0.2)
)
dev.off()


## Use a kernel regression to get a smooth nonparametric estimate of
## Pr(A Wins | CINC_A, CINC_B), to compare to the indifference curves under the
## capability ratio
fit_np <- npregbw(Outcome ~ cinc_a + cinc_b,
                  data = cinc_data %>%
                      mutate(Outcome = as.numeric(Outcome == "A")))
fit_np <- npreg(fit_np)

## Predicted probabilities across a grid of potential CINC scores
pp_grid <- expand.grid(cinc_a = seq(cmin, cmax, length.out = 25),
                       cinc_b = seq(cmin, cmax, length.out = 25))
pp <- predict(fit_np, newdata = pp_grid)
pp_grid <- cbind(pp_grid, prob = pp)

## Make basic heat map of the kernel regression predictions, then plot with and
## without original data, contour lines, and indifference curves
heat_cinc <- plot_base +
    geom_tile(data = pp_grid, aes(fill = prob)) +
    scale_fill_gradient("\\parbox{0.9in}{Probability}",
                        low = "white",
                        high = brewer.pal(5, "Blues")[4],
                        limits = c(0, 1))

tikz(file = file.path("..", "slides", "fig-cinc-heat.tex"),
     width = tikz_width,
     height = tikz_height,
     packages = tikz_package)
print(
    heat_cinc
)
dev.off()

tikz(file = file.path("..", "slides", "fig-cinc-heat-pts.tex"),
     width = tikz_width,
     height = tikz_height,
     packages = tikz_package)
print(
    heat_cinc +
        geom_point(aes(shape = Outcome),
                   size = 2.5,
                   alpha = 0.7,
                   fill = "white")
)
dev.off()

tikz(file = file.path("..", "slides", "fig-cinc-heat-contour.tex"),
     width = tikz_width,
     height = tikz_height,
     packages = tikz_package)
print(
    contour_cinc <- heat_cinc +
        geom_contour(data = pp_grid,
                     aes(z = prob),
                     colour = "black")
)
dev.off()

tikz(file = file.path("..", "slides", "fig-cinc-heat-contour-indiff.tex"),
     width = tikz_width,
     height = tikz_height,
     packages = tikz_package)
print(
    contour_cinc +
        geom_abline(intercept = seq(-4, 4, by = 4/3),
                    size = 2,
                    alpha = 0.2)
)
dev.off()
