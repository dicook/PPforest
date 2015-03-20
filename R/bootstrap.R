#' grouped data frame.
#'
#' @param df is a data frame with the complete data set. 
#' @param m number of bootstrap replicate
#' @param strata identify if the bootrap samples are stratify by class
#' @return grouped data frame object 
#' @export
#' @examples
#'iris.b <- bootstrap(iris[,5:1], 5) 
#'attributes(iris.b)$indices
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
  attr(df, "indices") <- lapply(samp.g, function(x) as.numeric(unlist(x))-1)
  }
  else{
    attr(df, "indices") <-  replicate(m, sample(n, replace = TRUE)-1, 
                                      simplify = FALSE)
  }
  attr(df, "drop") <- TRUE
  attr(df, "group_sizes") <- rep(n, m)
  attr(df, "biggest_group_size") <- n
  attr(df, "labels") <- data.frame(replicate = 1:m)
  attr(df, "vars") <- list(quote(replicate)) # Change
  class(df) <- c("grouped_df", "tbl_df", "tbl", "data.frame")
  
  df
}