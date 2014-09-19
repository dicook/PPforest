\name{LDA.Tree}
\alias{LDA.Tree}

\title{Find PP tree structure using LDA}

\usage{
LDA.Tree( i.class, i.data, weight = TRUE, ...) 
}
\description{
Find tree structure using linear discriminant in each split.
}
\arguments{
  \item{i.data}{A training data  without class information}
  \item{i.class}{class information}
  \item{weight}{weight flag using in LDA index}
 \item{...}{...}
}

\value{
\item{Tree.Struct}{Tree structure}
\item{Alpha.Keep}{1D projection of each split}
\item{C.Keep}{spliting rule for each split}
}
\references{
  Lee, E., Cook, D., and Klinke, S.(2002)
 \emph{Projection Pursuit indices for supervised classification}

}
\author{Eun-kyung Lee}

\seealso{\code{\link{PPindex.class}}, \code{\link{PP.optimize}}}

\examples{

data(iris)
n <- nrow(iris)
tot <- c(1:n)
n.train <- round(n*0.9)
train <- sample(tot,n.train)
test <- tot[-train]

Tree.result <- LDA.Tree(iris[train,5],iris[train,1:4])
Tree.result
}
\keyword{tree}