#' Draws bootstrap samples with strata option.
#'
#' @param df is a data frame with the complete data set with class variable in the first column.
#' @param m is the number of bootstrap replicates we want to sample.
#' @param strata is TRUE if the bootstrap samples are stratified by class variable.
#' @return data frame object with m bootstrap samples
#' @export
#' @importFrom magrittr %>%
#' @examples
#'iris.b <- ppf_bootstrap(iris[, 5:1], 500, strata = FALSE) 
#'index <- lapply(attributes(iris.b)$indices, function(x) x + 1)
ppf_bootstrap <- function(df, m = 500, strata = TRUE) {
    . <- NULL
    samp <- NULL
    n <- nrow(df)
    class.id <- data.frame(id = 1:n, class = df[, 1])
    if (strata == TRUE) {
        samp.g <- replicate(m, class.id %>% dplyr::group_by(class) %>% dplyr::do(samp = sort(sample(.$id, replace = TRUE))) %>% 
            dplyr::ungroup() %>% dplyr::select(samp), simplify = FALSE)
        
        attr(df, "indices") <- lapply(samp.g, function(x) as.numeric(sort(unlist(x))) - 1)
    } else {
        attr(df, "indices") <- replicate(m, sort(sample(n, replace = TRUE) - 1), simplify = FALSE)
    }
    
    attr(df, "drop") <- TRUE
    attr(df, "group_sizes") <- rep(n, m)
    attr(df, "biggest_group_size") <- n
    attr(df, "labels") <- data.frame(replicate = 1:m)
    attr(df, "vars") <- list(quote(replicate))
    class(df) <- c("grouped_df", "tbl_df", "tbl", "data.frame")
    
    df
} 
