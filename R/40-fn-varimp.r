################################################################################
###
### Functions for assessing variable importance by dropping terms from the model
### and re-running
###
################################################################################

library("dplyr")


## Drop all terms involving the character strings in the vector x from the given
## formula
drop_all_terms <- function(formula., x)
{
    ## Identify terms to be dropped (including interactions and the like)
    terms. <- terms(formula.)
    term_labels <- attr(terms., "term.labels")
    ind_bad <- integer(0)
    for (i in seq_along(x))
        ind_bad <- c(ind_bad, grep(x[i], term_labels))

    ## Drop terms and reconstruct formula
    if (length(ind_bad) > 0) {
        terms. <- drop.terms(terms.,
                             dropx = ind_bad,
                             keep.response = TRUE)
        formula. <- formula(terms.)
    }

    formula.
}

## Matrix of variable combinations
##
## Storing this here and not in the main script so we can refer back to it when
## making a table to summarize the results
component_combos <- expand.grid(
    variable = c("irst",
                 "milex",
                 "milper",
                 "pec",
                 "tpop",
                 "upop"),
    to_drop = c("a", "b", "both"),
    stringsAsFactors = FALSE
) %>% arrange(variable)
