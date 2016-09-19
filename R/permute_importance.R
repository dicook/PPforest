#' Obtain the importance variable measure based in permutation
#' 
#' @param ppf is a PPforest object
#' @param cl.tree when it is TRUE compute the importance variable base on permuted oob observations for each variable when it is FALSE compute an importance for all the forest with permuted values for each variable instead
#' @return importance variable plot  
#' @export
#' @importFrom magrittr %>%
#' @examples
#' pprf.crab<- PPforest(data = crab, class = "Type",
#'  size.tr = 1, m = 100, size.p = .4, PPmethod = 'LDA', strata = TRUE)
#' permute_importance(ppf = pprf.crab, cl.tree = FALSE, pl = TRUE) 
permute_importance <- function(ppf, cl.tree = TRUE, pl = TRUE){
  imp <-NULL
  nm <- NULL
  oob.id <- apply(ppf$oob.obs,1, which)
  permute <- plyr:: llply(oob.id , function(x) sample(x,length(x)))
if(cl.tree){
  pred.oob.per <- list()
  corr.oob.per <- matrix(0,ncol=ncol(ppf$train)-1,nrow=length(permute)) 
for(i in 1:(ncol(ppf$train)-1)) {

  for (j in 1:length(permute)) {
    aux <- ppf$train[,-which(colnames(ppf$train)==ppf$class.var)]
    d <- aux[ oob.id[[j]], ]
    d[, i] <- aux[ permute[[j]], i]
    
    
    pred.oob.per[[j]] <- PPtreeViz::PP.classify(test.data = d, Tree.result =  ppf[[8]][[2]][[j]], Rule = 1)[[2]]
    corr.oob.per[j,i] <-  sum(diag(table(pred.oob.per[[j]], ppf$train[ oob.id[[j]],ppf$class.var])))
  }
}
  corr.oob <- (1-ppf$oob.error.tree) * unlist(lapply(oob.id, length))
  
 imp.pl<- data.frame( nm = colnames(ppf$train)[colnames(ppf$train)!=ppf$class.var], imp=apply(corr.oob.per, 2, function(x) mean((corr.oob-x)))) %>% dplyr::arrange(imp)
 #imp.pl$imp/sd(imp.pl$imp)
 imp.pl$nm <- factor(imp.pl$nm, levels= imp.pl[order( imp.pl$imp), "nm"])
 
 p <-  ggplot2::ggplot(data = imp.pl, ggplot2::aes(imp,nm) ) + ggplot2::geom_point() + ggplot2::xlab("Importance") +
   ggplot2::ylab("")
  
}else{
  
  
  #For each variable a permutation is done without consider oob or not, the new data are predicted with the forest
  #and the training error is computed.
  
  
  corr.per <- numeric()
  for (j in 1:(ncol(ppf$train)-1)) {
   
    d <- ppf$train[,colnames(ppf$train)[colnames(ppf$train)!=ppf$class.var]]
    rnd <- sample(1:nrow(ppf$train), nrow(ppf$train) )
    d[, j] <- d[rnd, j]
    corr.per[j] <- sum(predict( object = ppf, class= "Type", test.data = d)[[2]] == ppf$train[,ppf$class.var])
  }
  
  
 
  
  corr.oob <- (1-ppf$oob.error.forest) * nrow(ppf$train)
  
  imp.pl<- data.frame( nm=colnames(ppf$train)[colnames(ppf$train)!=ppf$class.var], imp=corr.oob-corr.per) %>% dplyr::arrange(imp)
  imp.pl$nm <- factor(imp.pl$nm, levels= imp.pl[order( imp.pl$imp), "nm"])
  
  p <-  ggplot2::ggplot(data = imp.pl, ggplot2::aes(imp,nm) ) +ggplot2::geom_point() + ggplot2::xlab("Importance") +
    ggplot2::ylab("")
 
  
}
  if(pl==TRUE){
    plotly::ggplotly(p)
  }else{
    imp.pl
  }
}
  
  
 
  
  

 

