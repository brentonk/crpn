## Full model formula for use in ML methods

## Names of components
cinc_components <- c("irst", "milex", "milper", "pec", "tpop", "upop")

## Construct formula
formula_text <- paste(rep(c("SideA", "SideB"),
                          each = length(cinc_components)),
                      rep(cinc_components, 2),
                      sep = "_",
                      collapse = " + ")
formula_text <- paste("OutcomeCollapsed", "~", formula_text)

full_formula <- as.formula(formula_text)

## Formula for victory data
victory_formula <- update(full_formula, factor(VictoryA) ~ .)

## Formulas with time on the RHS
full_time_formula <- update(full_formula, . ~ . + StYear)
victory_time_formula <- update(victory_formula, . ~ . + StYear)
