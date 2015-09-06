#' OOB error visualization
#' 
#'\code{ppf_oob_error} Plot the cummulative oob error as a function of number of trees 
#' @usage ppf_oob_error(ppf, nsplit)
#' @param ppf a PPforest object
#' @param nsplit number of points where cumulative oob error rate is computed
#' @return a plot with the cumulative oob error rate
#' @export
#' @examples
#' pprf.leukemia <- PPforest(y = leukemia[, 1], x = leukemia[, -1], size.tr = 2/3, m = 500, size.p = .5, 
#' PPmethod = 'PDA', strata = TRUE)
#' ppf_oob_error(ppf = pprf.leukemia, nsplit = 50)
ppf_oob_error <- function(ppf, nsplit) {
    ntree <- NULL
    value <- NULL
    variable <- NULL
    error.cum <- function(ppf, m) {
        l.train <- 1:nrow(ppf$train)
        index2 <- lapply(attributes(ppf$boot.samp)$indices[1:m], function(x) x + 1)
        
        oob.obs <- plyr::ldply(index2, function(x) (!l.train %in% x))
        pred.mtree <- ppf$vote.mat[1:m, ]
        
        oob.pred <- sapply(X = 1:nrow(ppf$train), FUN = function(i) {
            t1 <- table(pred.mtree[oob.obs[, i] == TRUE, i])
            names(t1)[which.max(t1)]
        })
        
        
        oob.mat <- sapply(X = 1:nrow(ppf$train), FUN = function(i) {
            table(pred.mtree[oob.obs[, i] == TRUE, i])
        })
        
        aux <- unlist(lapply(oob.pred, is.null))
        oob.all <- 1 - sum(diag(table(unlist(oob.pred[!aux]), ppf$train[!aux, 1])))/length(ppf$train[!aux, 1])
        tab.err <- table(unlist(oob.pred[!aux]), ppf$train[!aux, 1])
        oob.class <- 1 - diag(tab.err)/apply(tab.err, 2, sum)
        c(oob.all, oob.class)
    }
    
    oob.err.sp <- plyr::mdply(data.frame(m = round(seq(2, ppf$n.tree, nsplit))), error.cum, ppf = ppf)
    
    names(oob.err.sp)[1:2] <- c("ntree", "all")
    
    oob.pl <- reshape2::melt(oob.err.sp, id.vars = "ntree")
    
    if (max(oob.pl$value) < 0.5) {
        p1 <- ggplot2::ggplot(data = oob.pl, ggplot2::aes(x = ntree, y = value, color = variable)) + ggplot2::geom_point() + 
            ggplot2::geom_line() + ggplot2::scale_y_continuous(name = "OOB error rate", limits = c(0, max(oob.pl$value))) + ggplot2::scale_x_continuous(name = "Number of trees")
        p1 + ggplot2::scale_colour_discrete(name = "Class")
        
    } else {
        p1 <- ggplot2::ggplot(data = oob.pl, ggplot2::aes(x = ntree, y = value, color = variable)) + ggplot2::geom_point() + 
            ggplot2::geom_line() + ggplot2::scale_y_continuous(name = "OOB error rate", limits = c(0, 1)) + ggplot2::scale_x_continuous(name = "Number of trees")
        p1 + ggplot2::scale_colour_discrete(name = "Class")
    }
} 
