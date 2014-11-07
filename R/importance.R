#' Data frame with the 1D projection of each split and plot of importance variable.
#' @param boot object from bootstrap_pp (list of PP.Tree objets for bootstrap samples)
#' @param data is a data frame with the complete data set. Class factor in the first column
#' @return a data frame with 1D projection of each split (Alpha.Keep) and a boxplot with the importance of variable for each split.
#' @export
#' @examples
#' data1<-iris[,5:1]
#' training<-train_fn(iris[,5],.9)
#' output<-bootstrap_pp(data1,scale=TRUE,training,ntree=50,index="LDA")     
#' varimp_pp(data,output)
varimp_pp <- function(data,boot){
  mat.vi <- abs(plyr:: ldply(boot[[1]], function(x) x[[2]]$Alpha.Keep))
  colnames(mat.vi)[-1] <- colnames(data)[-1]
  mat.vi$sam <- rep(1: sum((mat.vi$tr)==1),dim(mat.vi)[1]/sum((mat.vi$tr)==1))
  aux <- reshape:: melt(mat.vi,id.vars=c("sam","tr"))
  aux$sam <- factor(aux$sam)
print(ggplot2:: 
         qplot(data=aux,x=variable,y=value,geom="boxplot",facets=~sam)
        +theme(axis.text.x = element_text(angle = 90, hjust = 1))
      )
       return(mat.vi)
}

