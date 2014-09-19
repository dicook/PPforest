\name{PPindex.class}
\alias{PPindex.class}
\alias{PPindex.LDA}
\alias{PPindex.Lp}
\alias{PPindex.PDA}
\title{Calculate Projection Pursuit index}

\description{
For given projected data and class information, calculate projeciton pursuit index.
}

\usage{
 PPindex.class(PPmethod, data, class, weight=TRUE, r=NULL, lambda=NULL, ...)
 PPindex.LDA(data, class, weight=TRUE, ...) 
 PPindex.Lp(data, class, r, ...) 
 PPindex.PDA(data, class, lambda, ...)
}

\arguments{
  \item{PPmethod}{Selected PP index

  ``LDA" - LDA index

  ``Lp"  - Lp index

  ``PDA"  - Entropy-class index}

  \item{data}{A data  without class information}
  \item{class}{class information}
  \item{weight}{weight flag using in LDA index}
  \item{r}{a parameter for \eqn{L^r} index}
  \item{lambda}{a parameter for PDA index} 
 \item{...}{...}

}

\value{
  The value is an projection pursuit index for given data.
}
\references{
  Lee, E., Cook, D., and Klinke, S.(2002)
 \emph{Projection Pursuit indices for supervised classification}

}
\author{Eun-kyung Lee}


\seealso{\code{\link{PP.optimize}}}

\examples{
data(iris)

PPindex.class("LDA",iris[,1:2],iris[,5])
PPindex.class("LDA",iris[,1:2],iris[,5],weight=FALSE)
PPindex.class("Lp",iris[,1:2],iris[,5],r=1)
PPindex.class("PDA",iris[,1:2],iris[,5],lambda=0.1)

}
\keyword{multivariate}
