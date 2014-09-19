\name{PP.Tree}
\alias{PP.Tree}

\title{Find PP tree structure}

\usage{
PP.Tree(PPmethod, i.class, i.data, weight = TRUE, r = NULL,  
          lambda = NULL, cooling = 0.999, temp = 1, energy = 0.01,  ...) 
}
\description{
Find tree structure using projection pursuit in each split.
}
\arguments{
  \item{PPmethod}{Selected PP index

  ``LDA'' - LDA index

  ``Lp''  - Lp index;

  ``PDA''  - PDA index}
  \item{i.data}{A training data  without class information}
  \item{i.class}{class information}
  \item{weight}{weight flag using in LDA index}
  \item{r}{a parameter for \eqn{L_r} index}
  \item{lambda}{a parameter for PDA index}
  \item{cooling}{parameter for optimization}
  \item{temp}{inital temperature for optimization}
  \item{energy}{parameter for simulated annealing optimization}
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

Tree.result <- PP.Tree("LDA",iris[train,5],iris[train,1:4])
Tree.result
}
\keyword{tree}