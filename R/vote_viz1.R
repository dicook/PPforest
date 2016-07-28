#' Vote matrix visualization
#'  
#' @param data Data frame with the complete data set.
#' @param class A character with the name of the class variable. 
#' @param ppf is a PPforest object
#' @return A side-by-side jittered dotplot with the probability for each class to be classify in the corresponding class column.
#' Color by true observed class for each observation.
#' @importFrom magrittr %>%
#' @examples
#' #leukemia data set with all the observations used as training
#' pprf.leukemia <- PPforest(data = leukemia, class = "Type",
#'  size.tr = 1, m = 70, size.p = .4, PPmethod = 'PDA', strata = TRUE)
#' ppf_importance2(leukemia,"Type",pprf.leukemia, tree.id = 1)

vote_viz1 <- function(data, class, ppf){
  auxx <-
    data.frame(
      ids = 1:nrow(data), Type = ppf$train[,1], ppf$votes, pred = ppf$prediction.oob
    )
  
  juas <- auxx %>% tidyr::gather(Class,Probability,-one_of("pred","ids",class))
  
  p <-
    ggplot(data = juas, aes(Class, Probability, color = Type) )+ geom_jitter(height =
                                                                                          0,size = I(3), alpha = .5) +
    theme(legend.position = "bottom",aspect.ratio=1) +
    ggtitle("Side by side plot") + scale_colour_brewer(type = "qual",palette =
                                                         "Dark2")
  
  
  
  ggplotly(p,tooltip = c("x","y","key")) 
  
  
}