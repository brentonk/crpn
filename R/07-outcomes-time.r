################################################################################
###
### Plot distribution of outcomes over time
###
################################################################################

library("dplyr")
library("ggplot2")
library("RcppRoll")
library("tidyr")
library("tikzDevice")

Sys.unsetenv("TEXINPUTS")

load("results-imputations-train.rda")

## Just use first imputation (no missingness in outcome or year)
dat_train <- imputations_train[[1]]
outcomes <- levels(dat_train$Outcome)

## Form grid of all year-outcome combinations
plot_data <- as_data_frame(
    expand.grid(year = 1816:2007,
                Outcome = outcomes)
)

## Count number of each kind of dispute by year
dispute_counts <- dat_train %>%
    group_by(year, Outcome) %>%
    summarise(count = n())

## Merge counts into data and insert zeroes where necessary
plot_data <- plot_data %>%
    left_join(dispute_counts,
              by = c("year", "Outcome")) %>%
    mutate(count = ifelse(is.na(count), 0, count))

## Count total disputes by year
plot_data <- plot_data %>%
    group_by(year) %>%
    mutate(total = sum(count))

## Calculate rolling averages of each proportion
n_roll <- 10
plot_data <- plot_data %>%
    group_by(Outcome) %>%
    mutate(count_roll = order_by(year, roll_sumr(count, n = n_roll)),
           total_roll = order_by(year, roll_sumr(total, n = n_roll)),
           prop_roll = count_roll / total_roll) %>%
    filter(year >= 1816 + n_roll - 1) %>%
    ungroup()

tikz(file = file.path("..", "latex", "fig-outcomes-time.tex"),
     width = 5.25,
     height = 2)
print(
    ggplot(plot_data %>%
           mutate(Outcome = factor(
                      Outcome,
                      levels = c("VictoryA", "Stalemate", "VictoryB"),
                      labels = c("A Wins", "Stalemate", "B Wins")
                  )),
           aes(x = year, y = prop_roll)) +
    geom_line(size = 0.75) +
    facet_wrap(~ Outcome) +
    xlab("") +
    ylab("") +
    theme_grey(base_size = 10)
)
dev.off()
