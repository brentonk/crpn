################################################################################
###
### Collect and summarize replication results
###
################################################################################

library("dplyr")
library("foreach")
library("iterators")
library("xtable")
library("yaml")

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
