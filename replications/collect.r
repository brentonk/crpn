################################################################################
###
### Collect and summarize replication results
###
################################################################################

library("dplyr")
library("foreach")
library("iterators")
library("tidyr")
library("xtable")

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
               pmain_cr = p_value(fit_cr, "m"),
               pmain_doe = p_value(fit_doe, "m"),
               ppower_cr = p_value(fit_cr, "p"),
               ppower_doe = p_value(fit_doe, "p"),
               stringsAsFactors = FALSE)
}

## When does DOE model fit better?
replication_table %>%
    mutate(doe_wins_aic = aic_doe < aic_cr,
           doe_wins_vuong = vuong > 0,
           doe_wins_prl = prl_doe > prl_cr) %>%
    select(name, starts_with("doe_wins"))

## When does using DOE change our inference about the main hypothesis or about
## power?
replication_table %>%
    mutate(change_main = vuong > 0 & pmain_cr <= 0.05 & pmain_doe > 0.05,
           change_power = vuong > 0 & ppower_cr > 0.05 & ppower_doe <= 0.05) %>%
    select(name, starts_with("change_"))

## Retrieve replication characteristics and convert to data frame
replication_info <- yaml_to_df("replication-info.yml")

## Merge the tables
replication_table <- left_join(replication_table,
                               replication_info,
                               by = "name")

## Table for the body of the paper
body_table <- replication_table %>%
    select(citekey,
           n,
           vuong,
           starts_with("pmain"),
           starts_with("ppower")) %>%
    arrange(desc(n)) %>%
    mutate(n = prettyNum(n, big.mark = ",", trim = TRUE),
           citekey = paste0("\\citet{",
                            citekey,
                            "}"),
           vuong = sprintf("%.2f", vuong)) %>%
    mutate_each(funs(ifelse(. > 0.05, "", "$\\checkmark$")),
                starts_with("p")) %>%
    mutate_each(funs(gsub("^-", "$-$", .)),
                -citekey) %>%
    rename("Replication" = citekey,
           "$N$" = n,
           "Vuong" = vuong,
           "$p_{\\text{CINC}}$" = pmain_cr,
           "$p_{\\text{DOE}}$" = pmain_doe,
           "$p_{\\text{CINC}}${}" = ppower_cr,
           "$p_{\\text{DOE}}${}" = ppower_doe)

body_xtable <- body_table %>%
    xtable(align = c("l", "l", "r", "r", "c", "c", "c", "c"))

print(
    body_xtable,
    booktabs = TRUE,
    floating = FALSE,
    include.rownames = FALSE,
    sanitize.text.function = identity,
    add.to.row = list(
        pos = list(-1),
        command = paste("\\toprule",
                        "&&& \\multicolumn{2}{c}{Main Hyp.} & \\multicolumn{2}{c}{Power Hyp.} \\\\",
                        "\\cmidrule(lr){4-5} \\cmidrule(lr){6-7}",
                        "",
                        sep = "\n")
    ),
    hline.after = c(0, nrow(body_xtable)),
    file = file.path("..", "latex", "tab-replications.tex")
)

## More complete table for the appendix
appendix_table <- replication_table %>%
    select(citekey,
           starts_with("aic"),
           starts_with("prl"),
           starts_with("pmain"),
           starts_with("ppower")) %>%
    gather(variable, value, -citekey) %>%
    separate(variable, c("statistic", "model"), "_") %>%
    spread(statistic, value) %>%
    select(citekey, model, aic, prl, pmain, ppower) %>%
    mutate(citekey = paste0("\\citet{",
                            citekey,
                            "}"),
           model = factor(model,
                          levels = c("cr", "doe"),
                          labels = c("CINC", "DOE")),
           aic = sprintf("%.0f", aic),
           prl = sprintf("%.3f", prl),
           pmain = sprintf("%.2e", pmain),
           ppower = sprintf("%.2e", ppower)) %>%
    rename("Replication" = citekey,
           "Model" = model,
           "AIC" = aic,
           "CV P.R.L." = prl,
           "$p_{\\text{main}}$" = pmain,
           "$p_{\\text{power}}$" = ppower)

appendix_xtable <- appendix_table %>%
    xtable(align = c("l", "l", rep("r", ncol(appendix_table) - 1)))

print(
    appendix_xtable,
    booktabs = TRUE,
    floating = FALSE,
    include.rownames = FALSE,
    sanitize.text.function = identity,
    file = file.path("..", "latex", "tab-replications-appendix.tex")
)
