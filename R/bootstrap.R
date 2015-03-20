#' grouped data frame.
#'
#' @param df is a data frame with the complete data set. 
#' @param m number of bootstrap replicate
#' @return grouped data frame object 
#' @export
#' @examples
#'iris.b <- bootstrap(iris[,5:1], 50) 
#'attributes(iris.b)$indices
bootstrap <- function(df, m) {
  n <- nrow(df)
  
  attr(df, "indices") <- replicate(m, sample(n, replace = TRUE)- 1, 
                                   simplify = FALSE)
  attr(df, "drop") <- TRUE
  attr(df, "group_sizes") <- rep(n, m)
  attr(df, "biggest_group_size") <- n
  attr(df, "labels") <- data.frame(replicate = 1:m)
  attr(df, "vars") <- list(quote(replicate)) # Change
  class(df) <- c("grouped_df", "tbl_df", "tbl", "data.frame")
  
  df
}