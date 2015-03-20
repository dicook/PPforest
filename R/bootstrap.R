#' grouped data frame.
#'
#' @param df is a data frame with the complete data set and class in the first column. 
#' @param m number of bootstrap replicate
#' @param strata identify if the bootrap samples are stratify by class
#' @return grouped data frame object with m bootstrap samples 
#' @export
#' @examples
#'iris.b <- bootstrap(iris[,5:1], 30) 
#'lapply(attributes(iris.b)$indices,function(x) x+1)

bootstrap <- function(df, m,strata=TRUE) {
  n <- nrow(df)
  class.id <- data.frame(id=1:n,class=df[,1])
  if(strata==TRUE){
    samp.g <- replicate(m, class.id %>% 
                          group_by(class) %>%
                          do(samp=sort(sample(.$id, replace=TRUE)) ) %>%
                          ungroup() %>%
                          select(samp), 
                        simplify = FALSE)
    attr(df, "indices") <- lapply(samp.g, function(x) as.numeric(sort(unlist(x)))-1)
  }
  else{
    attr(df, "indices") <-  replicate(m, sort(sample(n, replace = TRUE)-1), 
                                      simplify = FALSE)
  }
  attr(df, "drop") <- TRUE
  attr(df, "group_sizes") <- rep(n, m)
  attr(df, "biggest_group_size") <- n
  attr(df, "labels") <- data.frame(replicate = 1:m)
  attr(df, "vars") <- list(quote(replicate)) 
  class(df) <- c("grouped_df", "tbl_df", "tbl", "data.frame")
  
  df
}