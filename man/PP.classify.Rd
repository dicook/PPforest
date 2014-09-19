\name{PP.classify}
\alias{PP.classify}

\title{Predict class for the test set and calculate prediction error}

\usage{
PP.classify(test.data, true.class, Tree.result, Rule, ...) 
}

\description{
After finding tree structure, predict class for the test set and calculate prediction error.
}
\arguments{
  \item{test.data}{ the test dataset}
  \item{true.class}{true class of test dataset if available}
  \item{Tree.result}{the result of PP.Tree}
  \item{Rule}{split rule

  1 - mean of two group means

  2 - weighted mean of two group means
 
  3 - mean of max(left group) and min(right group)

  4 - weighted mean of max(left group) and min(right group)}
  \item{...}{...}
}

\value{A list with components:
\item{predict.class}{predicted class}
\item{predict.error}{prediction error}
}
\references{
  Lee E., Cook D., and Klinke, S.  (2002)
 \emph{Projection Pursuit indices for supervised classification}

}
\author{Eun-kyung Lee}

\seealso{\code{\link{PPindex.class}}, \code{\link{PP.optimize}},
  \code{\link{PP.Tree}}}

\examples{
data(iris)
n <- nrow(iris)
n.train <- round(n*0.9)
train <- sample(n, n.train)

Tree.result <- PP.Tree("LDA", iris[train,5], iris[train, 1:4])
tree.train <- PP.classify(iris[train, 1:4], iris[train, 5], Tree.result,
                          Rule=1)
tree.train
tree.test <- PP.classify(iris[-train, 1:4], iris[-train, 5],
                         Tree.result, Rule=1)
tree.test
}
\keyword{tree}
