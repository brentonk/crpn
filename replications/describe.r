################################################################################
###
### Make description list of basic information about each replication for the
### appendix of the paper
###
################################################################################

library("foreach")
library("iterators")
library("yaml")

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

## Make description list for the appendix
paper_desc <- foreach(x = iter(replication_info, by = "row"), .combine = "c") %do% {
    c(paste0("  \\item \\citet{", x$citekey, "}"),
      "  \\begin{description}",
      paste("    \\item[Model Replicated]", x$which),
      paste("    \\item[Unit of Analysis]", x$units),
      paste("    \\item[Estimator]", x$method),
      paste("    \\item[CINC Terms]", x$terms_cinc),
      paste("    \\item[DOE Terms]", x$terms_doe),
      if (!is.na(x$notes)) paste("    \\item[Notes]", x$notes),
      "  \\end{description}",
      "")
}

paper_desc <- c("\\begin{itemize}",
                paper_desc,
                "\\end{itemize}")

writeLines(paper_desc,
           con = file.path("..", "latex", "list-replications.tex"))
