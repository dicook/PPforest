#' Proximity matrix visualization
#'
#' @param ppfo a PPforest object
#' @param type is MDS or heat
#' @param k number of dimensions of the MDS layout 
#' @return proximity matrix plot, if type is MDS then a MDS plot using proximity matrix information is shown and if type is heat a heat map of the proximity matrix is shown
#' @export
#' @examples
#' ppfr.iris <- PPforest(data = iris[,5:1], size.tr=2/3, m = 500, size.p = .9, 
#' PPmethod = 'LDA', strata = TRUE)
#' pproxy_plot(ppfr.iris, type='heat')
#' pproxy_plot(ppfr.iris, type='MDS', k=2)
#' #Leukemia data set
#' pprf.leukemia <- PPforest(data = leukemia, size.tr = .9, m = 500, size.p = .8, 
#' PPmethod = 'LDA', strata = TRUE)
#' pproxy_plot(pprf.leukemia, type='heat')
#' pproxy_plot(pprf.leukemia, type='MDS', k=2)
pproxy_plot <- function(ppfo, type = "heat", k) {
    
    if (type == "heat") {
        value <- NULL
        Var1 <- NULL
        Var2 <- NULL
        
        id <- diag(dim(ppfo$train)[1])
        id[lower.tri(id, diag = TRUE)] <- ppfo[[9]]$proxi
        id[upper.tri(id)] <- t(id)[upper.tri(id)]
        m.prox <- reshape2::melt(id)
        
        ggplot2::ggplot(m.prox, ggplot2::aes(Var1, Var2, fill = value)) + ggplot2::scale_y_reverse() + ggplot2::xlab("") + 
            ggplot2::ylab("") + ggplot2::geom_tile(ggplot2::aes(fill = value)) + ggplot2::scale_fill_gradient(high = "#132B43", 
            low = "#56B1F7", name = "Proximity")
    } else {
        
        value <- NULL
        Var1 <- NULL
        Var2 <- NULL
        MDS1 <- NULL
        MDS2 <- NULL
        fac <- NULL
        
        id <- diag(dim(ppfo$train)[1])
        id[lower.tri(id, diag = TRUE)] <- 1 - ppfo[[9]]$proxi
        rf.mds <- stats::cmdscale(as.dist(id), eig = TRUE, k = k)
        colnames(rf.mds$points) <- paste("MDS", 1:k, sep = "")
        nlevs <- nlevels(ppfo$train[, 1])
        
        if (k == 2) {
            df <- data.frame(fac = ppfo$train[, 1], rf.mds$points)
            ggplot2::ggplot(data = df) + ggplot2::geom_point(ggplot2::aes(x = MDS1, y = MDS2, color = fac)) + ggplot2::theme(aspect.ratio = 1) + 
                ggplot2::scale_colour_discrete(name = "Class")
            
        } else {
            df <- data.frame(fac = ppfo$train[, 1], rf.mds$points)
            GGally::ggpairs(data = df, columns = 2:ncol(df), colour = "fac")
        }
        
    }
} 
