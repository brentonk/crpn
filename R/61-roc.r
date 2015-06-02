#### scripts for making ROC curve

if (!require("rgl"))
    NULL
library("akima")
library("tikzDevice")

Sys.unsetenv("TEXINPUTS")

source("51-probs-preds.r")

classify <- function(vector,
                     alpha = NULL,
                     beta = NULL
){

  ### this function takes in a vector of probabilities (one for each obs.) and
  ### spits out a prediction for those observations.  if alpha and beta are
  ### given, it does this with varying sensitivity.  note that the alpha/beta
  ### option only works for triples, with the second category as "baseline"

  if(is.null(alpha)){

    which(vector == max(vector))

  }else{

    ifelse(vector[1] >= alpha,
           1,
           ifelse(vector[3] - vector[2] >= beta,
                  3,
                  2
           )
    )

  }

}

make_3DROC <- function(pr1,
                       pr2 = NULL,
                       y,
                       granularity = 50,
                       plot. = TRUE
                       ){

  if(!is.null(pr2)){

    alphas <- seq(from = 0, to = 1, length = granularity)
    betas  <- seq(from = -1, to = 1, length = granularity)
    gr <- expand.grid(alphas, betas)

    bin1 <- matrix(NA,
                   nrow = nrow(gr),
                   ncol = 3
    )
    bin2 <- bin1

    for(k in 1:nrow(bin1)){

      predict1 <- apply(pr1, 1, classify, alpha = gr$Var1[k], beta = gr$Var2[k])
      predict2 <- apply(pr2, 1, classify, alpha = gr$Var1[k], beta = gr$Var2[k])

      triple1 <- rep(0, 3)
      triple2 <- rep(0, 3)

      for(i in 1:length(triple1)){

        triple1[i] <- length(which(predict1 == i & y == i)) / length(which(y == i))
        triple2[i] <- length(which(predict2 == i & y == i)) / length(which(y == i))

      }

      bin1[k,] <- triple1
      bin2[k,] <- triple2

      if(k/10 == floor(k/10)){cat(k, "out of", nrow(bin1), "done.\n")}

    }

    bin1 <- as.data.frame(bin1)
    bin1 <- bin1[!duplicated(bin1),]
    bin1 <- bin1[order(bin1$V1),]

    bin2 <- as.data.frame(bin2)
    bin2 <- bin2[!duplicated(bin2),]
    bin2 <- bin2[order(bin2$V1),]

    bin1$mod <- 1
    bin2$mod <- 2

    bin <- rbind(bin1, bin2)

    if (plot.)
        plot3d(bin[,1], bin[,2], bin[,3], col = bin[,4], size = 3)

    return(bin)

  }else{

    alphas <- seq(from = 0, to = 1, length = granularity)
    betas  <- seq(from = -1, to = 1, length = granularity)
    gr <- expand.grid(alphas, betas)

    bin1 <- matrix(NA,
                   nrow = nrow(gr),
                   ncol = 3
    )

    for(k in 1:nrow(bin1)){

      predict1 <- apply(pr1, 1, classify, alpha = gr$Var1[k], beta = gr$Var2[k])

      triple1 <- rep(0, 3)

      for(i in 1:length(triple1)){

        triple1[i] <- length(which(predict1 == i & y == i)) / length(which(y == i))

      }

      bin1[k,] <- triple1

      if(k/10 == floor(k/10)){cat(k, "out of", nrow(bin1), "done.\n")}

    }

    bin1 <- as.data.frame(bin1)
    bin1 <- bin1[!duplicated(bin1),]
    bin1 <- bin1[order(bin1$V1),]

    if (plot.)
        plot3d(bin1[,1], bin1[,2], bin1[,3], col = "red", size = 3)

    return(bin1)

  }

}

## Make ggplot-friendly data from a list of probability data frames
makeContourData <- function(...,
                            y,
                            granularity = 50)
{
    dots <- list(...)

    ## Sanity check, then convert factor outcome to integer
    lev_check <- sapply(dots,
                        function(x) all(colnames(x) == levels(y)))
    if (!all(lev_check))
        stop("Factor levels don't match probability tables")
    y <- as.integer(y)

    ## Compute 3D ROC surfaces
    roc_surfaces <- lapply(dots,
                           make_3DROC,
                           y = y,
                           granularity = granularity,
                           plot. = FALSE)

    ## Linear interpolation of each surface, so as to have regularly spaced
    ## output to plug into geom_contour()
    xo <- yo <- seq(0, 1, length.out = granularity)
    for (i in seq_along(roc_surfaces)) {
        surf <- roc_surfaces[[i]]
        surf <- interp(surf[, 3],  # VictoryA
                       surf[, 1],  # VictoryB
                       surf[, 2],  # Stalemate
                       xo = xo,
                       yo = yo,
                       linear = TRUE,
                       duplicate = "user",
                       dupfun = max) # In case of duplicates, we want the best
                                     # possible classification of C given
                                     # classification of A and B, hence using
                                     # the max rule
                                     #
                                     # TODO LATER: Double-check this, or
                                     # rewrite so as not to have to reorder
                                     # the levels
        surf <- data.frame(expand.grid(VictoryA = surf$x,
                                       VictoryB = surf$y),
                           Stalemate = as.numeric(surf$z))
        surf$Method <- factor(names(dots)[i],
                              levels = names(dots))
        rownames(surf) <- NULL

        roc_surfaces[[i]] <- surf
    }

    ans <- do.call(rbind, roc_surfaces)
    rownames(ans) <- NULL

    ## Estimate volume under each curve
    est_vol <- sapply(roc_surfaces,
                      function(x) {
                          x$Stalemate[is.na(x$Stalemate)] <- 0
                          mean(x$Stalemate)
                      })
    vol <- data.frame(VictoryA = NA,
                      VictoryB = NA,
                      Method = factor(names(dots)),
                      label = paste(
                          "Volume under ROC:",
                          sprintf("%.2f", est_vol)
                      ))

    ans <- na.omit(ans)

    list(main = ans, vol = vol)
}

plot_roc <- makeContourData(
    "Capability Ratio Alone" = probs_logit,
    "Random Forest Model\non CINC Components + Year" = probs_rf_time,
    y = work_test$OutcomeCollapsed,
    granularity = 50
)

## Set coordinates for Volume under ROC to appear
plot_roc$vol$VictoryA <- 0.65           # x
plot_roc$vol$VictoryB <- 0.9           # y

pdf(file = file.path("..", "poster", "fig", "roc.pdf"),
    width = 7.25,
    height = 3.5)
print(
    ggplot(plot_roc$main, aes(x = VictoryA, y = VictoryB))
    + geom_tile(aes(fill = Stalemate))
    + geom_text(data = plot_roc$vol,
                aes(label = label),
                size = 3)
    + scale_x_continuous("Correct Identification Rate, A Wins")
    + scale_y_continuous("Correct Identification Rate, B Wins")
    + scale_fill_gradient(name = "Correct Identification\nRate, Stalemate",
                          low = "#b0dad4",
                          high = "#3a8278",
                          limits = c(0, 1))
    + facet_wrap(~ Method, ncol = 2)
    + theme_bw()
)
dev.off()

tikz(file = file.path("..", "slides", "roc.tex"),
     width = 4.75,
     height = 2.75)
print(
    ggplot(plot_roc$main, aes(x = VictoryA, y = VictoryB))
    + geom_tile(aes(fill = Stalemate))
    + geom_text(data = plot_roc$vol,
                aes(label = label),
                size = 2)
    + scale_x_continuous("Correct Identification Rate, A Wins")
    + scale_y_continuous("Correct Identification Rate, B Wins")
    + scale_fill_gradient(name = "Correct Identification\nRate, Stalemate",
                          low = "#b0dad4",
                          high = "#3a8278",
                          limits = c(0, 1))
    + facet_wrap(~ Method, ncol = 2)
    + ggtitle("Out-of-Sample ROC Curves")
    + theme_bw(base_size = 7)
    + theme(plot.background = element_rect(fill = "transparent", colour = NA),
            legend.background = element_rect(fill = "transparent", colour = NA))
)
dev.off()
