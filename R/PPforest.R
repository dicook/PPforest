#' Run a Projection Pursuit forest
#'
#' @param train is a data frame with the training data with class in the first column
#' @param testap is TRUE if we want to predict a new data set
#' @param test is a data frame it the new test data to predict
#' @param m number of bootstrap replicate
#' @param PPmethod to run the PPtree_plit function, options LDA or PDA, by default it is LDA.
#' @param size.p proportion of random sample variables in each split.
#' @param strata identify if the bootrap samples are stratify by class
#' @param lambda a parameter for PDA index
#' @return predicted values, error, bootstrap samples, trees ,training set from PPforest  
#' @export
#' @examples
#' tr.index <- train_fn(iris[, 5], 2/3)
#' te.index <- as.vector(1:length(iris[, 5]))[!(1:length(iris[, 5]) %in% (sort(tr.index$id)))]
#' train <- iris[sort(tr.index$id), 5:1 ]
#' test <- iris[-tr.index$id, 5:1 ]
#' ppfr.iris <- PPforest(train = train, testap = TRUE, test = test, m = 500, size.p = .9, 
#' PPmethod = 'LDA', strata = TRUE)
PPforest <- function(train, testap = TRUE, test, m, PPmethod, size.p, strata = TRUE, lambda=.1) {
  colnames(train)[1] <- "class"
  
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
  
  return(list(pred.tr[[3]], error.tr, pred.test[[3]], error.test, oob.error, oob.err.tree, data.b, output, proximity, vote.matrix))
} 
