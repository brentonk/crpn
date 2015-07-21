################################################################################
###
### Collect and summarize replication results
###
################################################################################

library("dplyr")
library("foreach")
library("ggplot2")
library("iterators")
library("tikzDevice")
library("xtable")
library("yaml")

Sys.unsetenv("TEXINPUTS")

source("fn-collect.r")

## Load up each replication file and compute relevant summary statistics and
## model comparison tests
replication_files <- list.files(pattern = "^results.*rda$")
replication_table <- foreach (x = replication_files, .combine = "rbind") %do% {
    fits <- load(x)
    stopifnot(grepl("^cr", fits[1]) && grepl("^doe", fits[2]))
    fit_cr <- get(fits[1])
    fit_doe <- get(fits[2])

    ## Summary statistics:
    ##   * Each model's AIC
    ##   * Vuong test statistic (where positive values indicate that the DOE model
    ##     is preferred)
    ##   * Each model's proportional reduction in out-of-sample log loss
    fit_name <- sub("^results\\-", "", x)
    fit_name <- sub("\\.rda$", "", fit_name)
    data.frame(name = fit_name,
               n = length(fit_cr$y),
               aic_cr = aic(fit_cr),
               aic_doe = aic(fit_doe),
               vuong = vuong(fit_doe, fit_cr),
               prl_cr = prl(fit_cr),
               prl_doe = prl(fit_doe),
               stringsAsFactors = FALSE)
}

## Show some basic summaries
replication_table %>%
    summarise(better_aic = sum(aic_doe < aic_cr),
              better_vuong = sum(vuong >= 1.96),
              worse_vuong = sum(vuong <= -1.96),
              better_prl = sum(prl_doe > prl_cr))

## Retrieve data frame of replication characteristics
replication_info <- yaml.load_file(
    "replication-info.yml",
    handlers = list(
        seq = function(x) paste(x, collapse = ", "),
        map = function(x) {
            x <- data.frame(x, stringsAsFactors = FALSE)
            if (is.null(x$notes))
                x$notes <- NA_character_
            x
        },
        main = function(x) do.call("rbind", x)
    ))

## Merge the tables
replication_table <- left_join(replication_table,
                               replication_info,
                               by = "name")

## Make table for the paper and prettify
paper_table <- replication_table %>%
    select(citekey,
           n,
           repeats,
           aic_cr,
           aic_doe,
           vuong,
           prl_cr,
           prl_doe) %>%
    arrange(desc(n)) %>%
    mutate(citekey = paste0("\\citet{", citekey, "}"),
           n = prettyNum(n, big.mark = ",", trim = TRUE),
           n = paste0(n, ifelse(repeats == 10,
                                "$^{\\dag}$",
                                "")),
           aic_cr = sprintf("%.0f", aic_cr),
           aic_doe = sprintf("%.0f", aic_doe),
           vuong = sprintf("%.2f", vuong),
           vuong = gsub("-", "$-$", vuong, fixed = TRUE),
           prl_cr = sprintf("%.3f", prl_cr),
           prl_doe = sprintf("%.3f", prl_doe)) %>%
    select(-repeats) %>%
    rename("Replication" = citekey,
           "$N$" = n,
           "CINC" = aic_cr,
           "DOE" = aic_doe,
           "Vuong" = vuong,
           "CINC " = prl_cr,
           "DOE " = prl_doe)

paper_xtable <- paper_table %>%
    xtable(align = c("l", "l", rep("r", ncol(paper_table) - 1)))

print(
    paper_xtable,
    floating = FALSE,
    hline.after = c(0, nrow(paper_xtable)),
    include.rownames = FALSE,
    sanitize.text.function = identity,
    add.to.row = list(
        pos = list(-1),
        command = "\\hline\n&& \\multicolumn{2}{c}{AIC} && \\multicolumn{2}{c}{P.R.L.} \\\\\n"
    ),
    file = file.path("..", "latex", "tab-replications.tex")
)


###-----------------------------------------------------------------------------
### Plots for slides
###-----------------------------------------------------------------------------

## Define common plot elements
tikz_width <- 4.25
tikz_height <- 3.25
tikz_size <- 8
tikz_theme <-
    theme(plot.background = element_rect(fill = "transparent", colour = NA),
          legend.background = element_rect(fill = "transparent", colour = NA))
tikz_package = c(getOption("tikzLatexPackages"),
                 "\\usepackage{amsmath}",
                 "\\usepackage{amssymb}")

## Subset data and prettify names
plot_data <- replication_table %>%
    mutate(author_year = paste(author, year),
           author_year = ifelse(duplicated(author_year) |
                                    duplicated(author_year, fromLast = TRUE),
                                paste0(author_year, " (", journal, ")"),
                                author_year),
           aic_diff = aic_doe - aic_cr,
           prl_diff = prl_doe - prl_cr) %>%
    select(author_year,
           n,
           aic_diff,
           vuong,
           prl_diff)

## Plot differences in AIC
tikz(file = file.path("..", "slides", "fig-replication-aic.tex"),
     width = tikz_width,
     height = tikz_height,
     packages = tikz_package)
plot_data %>%
    arrange(aic_diff) %>%
    mutate(author_year = factor(author_year,
                                levels = rev(author_year))) %>%
    ggplot(aes(x = author_year, y = -aic_diff)) +
    geom_point(aes(size = log10(n))) +
    geom_hline(yintercept = 0, linetype = 2) +
    scale_size("$N$",
               range = c(1, 4),
               breaks = 3:6,
               labels = paste0("$10^", 3:6, "$")) +
    scale_x_discrete("") +
    scale_y_continuous("AIC ($\\text{CINC} - \\text{DOE}$)") +
    coord_flip() +
    theme_grey(base_size = tikz_size) +
    tikz_theme
dev.off()

## Plot Vuong test statistic
tikz(file = file.path("..", "slides", "fig-replication-vuong.tex"),
     width = tikz_width,
     height = tikz_height,
     packages = tikz_package)
plot_data %>%
    arrange(desc(vuong)) %>%
    mutate(author_year = factor(author_year,
                                levels = rev(author_year))) %>%
    ggplot(aes(x = author_year, y = vuong)) +
    geom_point(aes(size = log10(n))) +
    geom_hline(yintercept = c(-1.96, 1.96), linetype = 2) +
    scale_size("$N$",
               range = c(1, 4),
               breaks = 3:6,
               labels = paste0("$10^", 3:6, "$")) +
    scale_x_discrete("") +
    scale_y_continuous("Vuong test statistic") +
    coord_flip() +
    theme_grey(base_size = tikz_size) +
    tikz_theme
dev.off()

## Plot difference in CV loss
tikz(file = file.path("..", "slides", "fig-replication-prl.tex"),
     width = tikz_width,
     height = tikz_height,
     packages = tikz_package)
plot_data %>%
    arrange(desc(prl_diff)) %>%
    mutate(author_year = factor(author_year,
                                levels = rev(author_year))) %>%
    ggplot(aes(x = author_year, y = prl_diff)) +
    geom_point(aes(size = log10(n))) +
    geom_hline(yintercept = 0, linetype = 2) +
    scale_size("$N$",
               range = c(1, 4),
               breaks = 3:6,
               labels = paste0("$10^", 3:6, "$")) +
    scale_x_discrete("") +
    scale_y_continuous("Proportional Reduction in CV Loss ($\\text{DOE} - \\text{CINC}$)") +
    coord_flip() +
    theme_grey(base_size = tikz_size) +
    tikz_theme
dev.off()
