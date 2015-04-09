#' List of PP.Tree objects for bootstrap samples.
#'
#' @param data.b are the bootstrap samples. Class factor in the first column
#' @param scale is to standardize the data set, by default it is TRUE.
#' @param index to run the PPtree_plit function, options LDA or PDA, by default it is LDA.
#' @return list with the PP.Tree output for all the bootstraps samples  
#'   training set.
#' @export
#' @examples
#' training<-train_fn2(iris[,5],.9)
#' data1<-iris[training$id,5:1]
#' iris.b <- bootstrap(data1, 5) 
#' output1 <- trees_pp(iris.b,size.p=.9,index="LDA") 
#' output2 <- trees_pp(iris.b,size.p=.9,index="PDA",lambda=.14) 
#' 
trees_pp <- function(data.b,size.p=.9,index='LDA',lambda,...){
  names(data.b)[1] <-"class"
  if(index=="LDA") {
  trees <- data.b %>%
                  dplyr::do(tr = LDAtree_split(as.formula('class~.'), data = . ,size.p=size.p,...))
               #dplyr::do(tr = PPtree_split2(index, fr=as.formula('class~.'), data = . ,size.p=size.p,...))
  
  }else
  {trees <- data.b %>%
     dplyr::do(tr = PDAtree_split(as.formula('class~.'), data = . ,size.p=size.p,lambda,...))
  }
  trees
}

#dplyr::do(tr = PPtree_split2(index, as.formula('class~.'), data = . ,size.p=size.p,...))



 





