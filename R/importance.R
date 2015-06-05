#' Global importance variable visualization for a PPForest object
#'
#' @param data is a data frame with the complete data set. Class factor in the first column
#' @param ppforest is a PPforest object
#' @param global is TRUE if we want to see the global importance of the forest
#' @param weight is TRUE if we want to see a weighted mesure of the forest importance based on out of bag trees errors
#' @return  A dotplot with a global measure of importance  variables in the PPforest.
#' @export
#' @importFrom magrittr %>%
#' @examples
#' ppforest <- PPforest( data = iris[,5:1], size.tr = 2/3, testap = TRUE,  
#' m = 500, size.p = .9, PPmethod = 'LDA', strata = TRUE, std = TRUE)
#' importance(iris[,5:1], ppforest, global = TRUE, weight = FALSE) 
importance <- function(data, ppforest, global=TRUE, weight=TRUE) {
  value <- NULL
  variable <- NULL
  node <- NULL
  mat.proj <- abs(plyr::ldply(ppforest[[8]][[2]], function(x) data.frame(node =  1:dim(x$projbest.node)[1],x$projbest.node)))
  colnames(mat.proj)[-1] <- colnames(data[, -1])

  index.part <- plyr::ldply(ppforest[[8]][[2]], function(x) data.frame(index = x$Tree.Struct[,5][x$Tree.Struct[,5]!=0]))
  n.vars <- ncol(mat.proj[,-1])
  index.mat <- matrix(rep(index.part[,1],n.vars),ncol=n.vars,byrow=F)
  
  oob.error.tree <- rep(ppforest[[6]], each = length(unique(mat.proj$node)))
  imp.weight <- mat.proj[,-1]*index.mat*(1 - oob.error.tree)
  mmat.vi <- reshape2::melt(mat.proj, id.vars = "node")
  mat.vi.w <- data.frame(node=mat.proj$node, imp.weight)
  colnames(mat.vi.w)[-1] <- colnames(data[, -1])
  mmat.vi.w <- reshape2::melt(mat.vi.w, id.vars = "node")
  
  mmat.vi.0 <- subset(mmat.vi, value != 0)
  mmat.vi.w.0 <- subset(mmat.vi.w, value != 0)
  
  import.vi.0 <- mmat.vi.0 %>%
    dplyr::group_by(node,variable) %>%
    dplyr::summarise(mean = mean(value)) %>%
    dplyr::arrange( dplyr::desc(mean)) 
  import.vi.0$variable <- with(import.vi.0, reorder(variable, mean) )
  
 
  import.vi.g.0 <- mmat.vi.0 %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise(mean = mean(value)) %>%
    dplyr::arrange( dplyr::desc(mean)) 
  import.vi.g.0$variable <- with(import.vi.g.0, reorder(variable, mean) )
  
  import.vi.w.0 <- mmat.vi.w.0 %>%
    dplyr::group_by(node,variable) %>%
    dplyr::summarise(mean = mean(value)) %>%
    dplyr::arrange( dplyr::desc(mean)) 
  import.vi.w.0$variable <- with(import.vi.w.0, reorder(variable, mean) )
  
  import.vi.wg.0 <- mmat.vi.w.0 %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise(mean = mean(value)) %>%
    dplyr::arrange( dplyr::desc(mean)) 
  import.vi.wg.0$variable <- with(import.vi.wg.0, reorder(variable, mean) )
   
  if(global == TRUE & weight == TRUE){
  print(ggplot2::ggplot( import.vi.wg.0, ggplot2::aes(x = mean, y = variable)) + 
          ggplot2::geom_point())
   }
  if(global == TRUE & weight==FALSE){
    print(ggplot2::ggplot( import.vi.g.0, ggplot2::aes(x = mean, y = variable)) + 
            ggplot2::geom_point()) 
    
    }
  if(global == FALSE & weight == FALSE){
    print(ggplot2::ggplot( import.vi.0, ggplot2::aes(x = mean, y = variable)) +
            ggplot2::geom_point() +
            ggplot2::facet_wrap( ~ node) )
  }
  if(global == FALSE & weight == TRUE){
     print(ggplot2::ggplot( import.vi.w.0, ggplot2::aes(x=mean,y=variable)) + 
              ggplot2::geom_point() +
              ggplot2::facet_wrap( ~ node))
  }
 
}

