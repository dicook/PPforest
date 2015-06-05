#' Projection Pursuit Random Forest
#'
#'\code{PPforest} implements a random forest using projection pursuit trees algorithm (based on PPtreeViz package).
#' @usage PPforest(data, size.tr, testap = TRUE, m, PPmethod, size.p, 
#' strata = TRUE, lambda=.1)
#' @param data a data frame with the class variable in the first column.
#' @param size.tr is the size proportion of the training if we want to split the data in training and test.
#' @param testap If set to \code{TRUE}(default) indicate a test data will be used of size \code{1-size.tr}.
#' @param m is the number of bootstrap replicates, this corresponds with the number of trees to grow. To ensure that each observation is predicted a few times we have to select this nunber no too small.
#' @param PPmethod is the projection pursuit index to optimize in each classification tree. The options are \code{LDA} and \code{PDA}, linear discriminant and penalized linear discriminant. By default it is \code{LDA}.
#' @param size.p proportion of variables randomly sampled in each split.
#' @param strata if set \code{TRUE} then the bootrap samples are stratifyed by class variable.
#' @param lambda a parameter for \code{PDA} index.
#' @return An object of class \code{PPforest} with components.
#' \item{prediction.training}{predicted values for training data set.}
#' \item{training.error}{error of the training data set.}
#' \item{prediction.test}{predicted values for the test data set if \code{testap = TRUE}(default).}
#' \item{error.test}{error of the test data set if \code{testap = TRUE}(default).}
#' \item{oob.error.forest}{out of bag error in the forest.}
#' \item{oob.error.tree}{out of bag error for each tree in the forest.}
#' \item{boot.samp}{information of bootrap samples.}
#' \item{output.trees}{output from a \code{trees_pp} for each bootrap sample.}
#' \item{proximity}{Proximity matrix, if two cases are classified in the same terminal node then the proximity matrix is increased by one in \code{PPforest} there are one terminal node per class.}
#' \item{votes}{ a matrix with one row for each input data point and one column for each class, giving the fraction of (OOB) votes from the \code{PPforest}.}
#' \item{n.tree}{number of trees grown in \code{PPforest}.}
#' \item{n.var}{number of predictor variables selected to use for spliting at each node.}
#' \item{type}{classification.}
#' \item{confusion}{confusion matrix of the prediction (based on OOb data).}
#' \item{call}{the original call to \code{PPforest}.}
#' \item{train}{is the training data based on \code{size.tr} sample proportion}
#' \item{test}{is the test data based on \code{1-size.tr} sample proportion}
#' @export
#' @examples
#' ppfr.iris <- PPforest(data = iris[,5:1], size.tr=2/3, testap = TRUE, m = 500, size.p = .9, 
#' PPmethod = 'LDA', strata = TRUE)
 PPforest <- function(data, size.tr=2/3, testap = TRUE, m, PPmethod, size.p, strata = TRUE, lambda=.1) {

  tr.index <- train_fn(data[, 1], size.tr)
  train <- data[sort(tr.index$id), ]

  colnames(train)[1] <- "class"
  type="Classification"
  nam <- colnames(train[,-1])
  var.num <- 1:length(nam)
  var.sel <- floor(length(var.num) * size.p)
  
  if (strata == TRUE) {
    data.b <- bootstrap(train, m, strata)
    output <- trees_pp(data.b, size.p, PPmethod, lambda = .1)
    } 
  else{
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
  
  votes<- matrix(0, ncol = length(unique(train[, 1])), nrow = nrow(train))
  colnames(votes) <- unique(train[, 1])
  
  for (i in 1:nrow(train)){ 
    cond <- colnames(votes) %in% names(oob.mat[[i]])
    votes[i, cond] <- oob.mat[[i]]
  }
  
  vote.matrix.prop <- votes/apply(votes,1,sum)
  
  oob.error <- 1-sum(diag(table(oob.pred, train[, 1])))/length(train[, 1])
  
  oob.err.tree <- sapply(X = 1:m, FUN=function(i) {
    dd <- diag(table( pred.tr[[2]][i,oob.obs[i, ] == TRUE] , train[oob.obs[i, ] == TRUE, 1]))
    1-sum(dd)/sum(oob.obs[i, ] == TRUE)
  })

  error.tr <- 1 - sum(train[, 1] == pred.tr[[3]])/length(pred.tr[[3]])
  
  if(testap == TRUE) {
    test <- data[-tr.index$id, -1 ]
    pred.test <- forest_ppred(test , output)
    error.test <- 1 - sum(data[-tr.index$id, 1] == pred.test[[3]])/length(pred.test[[3]])
  } 
  else{
    pred.test <- NULL
    error.test <- NULL
  }
  
  tab.tr <- table(Observed=train[,1],Predicted=oob.pred)

  class.error <- 1-diag(tab.tr)/((addmargins(tab.tr,2))[,"Sum"])
  confusion <- cbind(tab.tr,class.error=round(class.error,2))
  
 results <- list(prediction.training = pred.tr[[3]], training.error = error.tr, prediction.test = pred.test[[3]],
              error.test = error.test, oob.error.forest = oob.error, oob.error.tree = oob.err.tree, boot.samp = data.b, 
              output.trees = output, proximity = proximity, votes= vote.matrix.prop, n.tree = m , n.var = var.sel, 
              type="Classification", confusion=confusion, call =  match.call(), train = train, test = test)
class(results) <- "PPforest"

return(results)

} 

