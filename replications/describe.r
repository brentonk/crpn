################################################################################
###
### Make description list of basic information about each replication for the
### appendix of the paper
###
################################################################################

library("foreach")
library("iterators")
library("yaml")

source("fn-collect.r")

## Retrieve data frame of replication characteristics
replication_info <- yaml_to_df("replication-info.yml")

## Make description list for the appendix
paper_desc <- foreach(x = iter(replication_info, by = "row"), .combine = "c") %do% {
    c(paste0("  \\item \\citet{", x$citekey, "}"),
      "  \\begin{description}",
      paste("    \\item[Model Replicated]", x$which),
      paste("    \\item[Unit of Analysis]", x$units),
      paste("    \\item[Dependent Variable]", x$dv),
      paste("    \\item[Estimator]", x$method),
      paste("    \\item[CINC Terms]", x$terms_cinc),
      paste("    \\item[DOE Terms]", x$terms_doe),
      paste("    \\item[Main Null Hypothesis]", x$null_hypothesis),
      if (!is.na(x$notes)) paste("    \\item[Notes]", x$notes),
      "  \\end{description}",
      "")
}

paper_desc <- c("\\begin{itemize}",
                paper_desc,
                "\\end{itemize}")

writeLines(paper_desc,
           con = file.path("..", "latex", "list-replications.tex"))
