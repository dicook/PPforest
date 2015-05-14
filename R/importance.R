#' Data frame with the 1D projection of each split unweighted and weighted by 1- oob error rate for each tree and plots of importance variable for each split.

#' @param data is a data frame with the complete data set. Class factor in the first column
#' @param ppforest is an object from PPforest 
#' @return a data frame with 1D projection of each split (Alpha.Keep) and a boxplot with the importance of variable for each split.
#' @export
#' @examples

#' tr.index <- train_fn(iris[, 5], 2/3)
#' te.index <- as.vector(1:length(iris[, 5]))[!(1:length(iris[, 5]) %in% (sort(tr.index$id)))]
#' train <- iris[sort(tr.index$id), 5:1 ]
#' test <- iris[-tr.index$id, 5:1 ]
#' ppforest <- PPforest( train = train, testap = TRUE, test = test, m = 500, size.p = .9, PPmethod = 'LDA', strata = TRUE)
#' importance(train,ppforest) 

importance <- function(data,ppforest){
  mat.vi <- abs(plyr::ldply(ppforest[[8]][[2]], function(x) data.frame(dim= 1:dim(x$projbest.node)[1],x$projbest.node)))
  colnames(mat.vi)[-1] <- colnames(data[,-1])

  oob.error.tree <- rep(ppforest[[6]], each = length(unique(mat.vi$dim)))
  imp.weight <- mat.vi[,-1]*(1-oob.error.tree)
  aux <- reshape2::melt(mat.vi,id.vars = "dim")
  aux2<- data.frame(dim=mat.vi$dim, imp.weight)
  colnames(aux2)[-1] <- colnames(data[, -1])
  aux22 <- reshape2::melt(aux2,id.vars="dim")
  
  aux.0 <- subset(aux,value!=0)
  aux22.0 <- subset(aux22,value!=0)
  print(ggplot2::qplot(data=aux.0,x=variable,y=value,geom="boxplot",facets=~dim) + 
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)))
  print(ggplot2::qplot(data=aux22.0,x=variable,y=value,geom="boxplot",facets=~dim) + 
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)))
  return(list(mat.vi,aux2))
  
}

