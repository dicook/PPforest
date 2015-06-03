#' Projection Pursuit Random Forest
#'
#' \code{PPforest} implements a random forest using projection pursuit trees algorithm (based on \code{PPtreeViz} package.
#' @usage PPforest(train, testap = TRUE, test, m, PPmethod, size.p, strata = TRUE, lambda=.1)
#' @param train is a data frame with the training data with class variable in the first column
#' @param testap If set to \code{TRUE}(default) indicate a test data will be provided to predict
#' @param test is a data frame with we want to to predict, if \code{testap} is \code{TRUE} we have to include \code{test} data
#' @param m is the number of bootstrap replicates, this corresponds with the number of trees to grow. To ensure that each observation is predicted a few times we have to select this nunber no too small.
#' @param PPmethod is the projection pursuit index to optimize in each classification tree. The options are \code{LDA}  and \code{PDA}, linear discriminant and penalized linear discriminant. By default it is \code{LDA}.
#' @param size.p proportion of variables randomly sampled in each split.
#' @param strata if set \code{TRUE} then the bootrap samples are stratifyed by class
#' @param lambda a parameter for PDA index
#' @return An object of class \code{PPforest} with components
#'   \item{prediction.training}{predicted values for training data set}
#'   \item{training.error}{error of the training data set}
#'   \item{prediction.test}{predicted values for the test data set}
#'   \item{error.test}{error of the test data set}
#'   \item{oob.error.forest}{out of bag error in the forest}
#'   \item{oob.error.tree}{out of bag error for each tree}
#'   \item{boot.samp}{bootrap samples}
#'   \item{output.trees}{output from a trees_pp}
#'   \item{proximity}{Proximity matrix} 
#'   \item{vote.matrix}{Marix with votes for each class on each observation}
#'   \item{n.tree}{number of trees grown in \code{PPforest}}
#'   \item{n.var}{number of predictor variables selected to use for spliting at each node}
#'   \item{type}{classification}
#'   \item{confusion}{confusion matrix of the prediction (based on OOb data)}
#'   \item{call}{the original call to \code{PPforest}}
#' @export
#' @examples
#' tr.index <- train_fn(iris[, 5], 2/3)
#' train <- iris[sort(tr.index$id), 5:1 ]
#' test <- iris[-tr.index$id, 5:1 ]
#' ppfr.iris <- PPforest(train = train, testap = TRUE, test = test, m = 500, size.p = .9, 
#' PPmethod = 'LDA', strata = TRUE)
#' 
PPforest <- function(train, testap = TRUE, test, m, PPmethod, size.p, strata = TRUE, lambda=.1) {
  colnames(train)[1] <- "class"
  type="Classification"
  nam <- colnames(train[,-1])
  var.num <- 1:length(nam)
  var.sel <- floor(length(var.num) * size.p)
  
  if (strata == TRUE) {
    data.b <- bootstrap(train, m, strata)
    output <- trees_pp(data.b, size.p, PPmethod, lambda = .1)
    
  } else {
    data.b <- bootstrap(train, m, strata = FALSE)
    output <- trees_pp(data.b, size.p, PPmethod, lambda = .1)
    
  }
  
  pred.tr <- forest_ppred(train[, -1], output)
  pos <- expand.grid(a = 1:dim(train)[1], b = 1:dim(train)[1])
  cond <- pos[,1] >= pos[, 2]
  tri.low <- pos[cond, ]
  
  same.node <- data.frame(tri.low, dif = apply(t(pred.tr[[2]]), 2, function(x) x[ tri.low[, 1]] == x[ tri.low[, 2]]))
  proximity <- data.frame(same.node[, c(1:2)], proxi = apply(same.node[, -c(1:2)], 1, function(x) sum(x == 1))/dim((pred.tr[[2]]))[1])
  
  l.train <- 1:nrow(train)
  index <- lapply(attributes(data.b)$indices, function(x) x + 1)

  oob.obs <- plyr::ldply(index, function(x) (!l.train%in%x))
  
  oob.pred <- sapply(X = 1:nrow(train), FUN=function(i) {
    t1 <- table(pred.tr[[2]][ oob.obs[, i] == TRUE, i]) 
    names(t1)[which.max(t1)]
  })
  
  oob.mat <- sapply(X = 1:nrow(train), FUN = function(i) {
    table(pred.tr[[2]][ oob.obs[, i] == TRUE, i] )  
  })
  
  vote.matrix <- matrix(0, ncol = length(unique(train[, 1])), nrow = nrow(train))
  colnames(vote.matrix) <- unique(train[, 1])
  
  
  for (i in 1:nrow(train)){ 
    cond <- colnames(vote.matrix) %in% names(oob.mat[[i]])
    vote.matrix[i, cond] <- oob.mat[[i]]
  }
  
  vote.matrix.prop <- vote.matrix/apply(vote.matrix,1,sum)
  
  oob.error <- 1-sum(diag(table(oob.pred, train[, 1])))/length(train[, 1])
  
  oob.err.tree <- sapply(X = 1:m, FUN=function(i) {
    dd <- diag(table( pred.tr[[2]][i,oob.obs[i, ] == TRUE] , train[oob.obs[i, ] == TRUE, 1]))
    1-sum(dd)/sum(oob.obs[i, ] == TRUE)
  })

  error.tr <- 1 - sum(train[, 1] == pred.tr[[3]])/length(pred.tr[[3]])
  
  if (testap==TRUE) {
    pred.test <- forest_ppred(test[, -1], output)
    error.test <- 1 - sum(test[, 1] == pred.test[[3]])/length(pred.test[[3]])
  } else {
    pred.test <- NULL
    error.test <- NULL
  }
  
  tab.tr <- table(Observed=train[,1],Predicted=oob.pred)


  class.error <- 1-diag(tab.tr)/((addmargins(tab.tr,2))[,"Sum"])
  confusion <- cbind(tab.tr,class.error=round(class.error,2))
  
  
 results <- list(prediction.training = pred.tr[[3]], training.error = error.tr, prediction.test = pred.test[[3]],
              error.test = error.test, oob.error.forest = oob.error, oob.error.tree = oob.err.tree, boot.samp = data.b, 
              output.trees = output, proximity = proximity, vote.matrix = vote.matrix.prop, n.tree = m , n.var=var.sel, 
              type="Classification", confusion=confusion, call =  match.call())
class(results) <- "PPforest"

return(results)

} 
