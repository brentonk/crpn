################################################################################
###
### Assess variable importance
###
################################################################################

library("caret")
library("dplyr")
library("foreach")
library("iterators")
library("randomForest")

load("results-trained-models.rda")

rf_summary <- foreach (imp = trained_models, .combine = "rbind") %do% {
    forest <- imp[["rf_t"]]$finalModel

    ans <- foreach (k = icount(forest$ntree), .combine = "rbind") %do% {
        tree <- getTree(forest, k = k, labelVar = TRUE)

        ## Retrieve the nodes that split on "year"
        year_nodes <- filter(tree,
                             `split var` == "year")

        ## Retrieve the children of the nodes that split on "year"
        year_node_children <- with(year_nodes, c(`left daughter`, `right daughter`))
        year_node_children <- tree[year_node_children, , drop = FALSE]

        ## Drop nodes that are terminal or themselves split on year
        good_children <- filter(year_node_children,
                                status == 1,
                                `split var` != "year")

        ## Retrieve the nodes that split on a component of a's or b's
        a_nodes <- filter(tree,
                          grepl("_a$", `split var`))
        b_nodes <- filter(tree,
                          grepl("_b$", `split var`))

        ## Retrieve the children of the nodes that split on a component of a's
        ## or b's
        a_node_children <- with(a_nodes, c(`left daughter`, `right daughter`))
        a_node_children <- tree[a_node_children, , drop = FALSE]
        b_node_children <- with(b_nodes, c(`left daughter`, `right daughter`))
        b_node_children <- tree[b_node_children, , drop = FALSE]

        ## Drop nodes that are terminal, split on a component of the same
        ## country, or split on year
        a_node_children <- filter(a_node_children,
                                  status == 1,
                                  grepl("_b$", `split var`))
        b_node_children <- filter(b_node_children,
                                  status == 1,
                                  grepl("_a$", `split var`))

        ## Return (1) whether year is the first split and (2) whether any year
        ## splits lead to a node that splits on a non-year variable
        c(year_first = tree[1, "split var"] == "year",
          any_good_children = nrow(good_children) > 0,
          any_country_interaction = nrow(a_node_children) > 0 |
              nrow(b_node_children) > 0)
    }

    ## Calculate averages within this imputation
    colMeans(ans)
}

## Calculate overall averages
rf_averages <- colMeans(rf_summary)

print(rf_averages)
