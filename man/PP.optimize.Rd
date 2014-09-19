\name{PP.optimize}
\alias{PP.optimize}
\alias{PP.optimize.random}
\alias{PP.optimize.anneal}
\alias{PP.optimize.Huber}
\alias{PP.optimize.plot}

\title{Find optimal projection by maximizing selected PP index}

\usage{
PP.optimize.random(PPmethod, projdim, data, class, std=TRUE,
                   cooling=0.99, temp=1, r=NULL, lambda=NULL, weight=TRUE, ...)
PP.optimize.anneal(PPmethod, projdim, data, class, std=TRUE,
                   cooling=0.999, temp=1, energy=0.01,
                   r=NULL, lambda=NULL, weight=TRUE, ...)
PP.optimize.Huber(PPmethod, projdim, data, class, std=TRUE,
                  cooling=0.99, temp=1, r=NULL, lambda=NULL,
                  weight=TRUE, ...)
PP.optimize.plot(PP.opt, data, class, std=TRUE)
}

\description{
Find optimal projection using PP index.
}
\arguments{
  \item{PPmethod}{Selected PP index

  ``LDA" - LDA index

  ``Lp"  - Lp index;

  ``PDA"  - PDA index}

  \item{projdim}{dimension of projection that you want to find}
  \item{data}{data  without class information}
  \item{class}{class information}
  \item{std}{decide whether data will be standardized or not before applying projection pursuit} 
  \item{weight}{weight flag using in LDA index}
  \item{cooling}{parameter for optimization}
  \item{temp}{inital temperature for optimization}
  \item{energy}{parameter for simulated annealing optimization}
  \item{r}{a parameter for \eqn{L_r} index}
  \item{lambda}{a parameter for PDA index}
  \item{PP.opt}{the optimal projection}
  \item{...}{...}
}

\value{
\item{index.best}{PP index for optimal projected data}
\item{proj.best}{optimal projection}
}
\references{
  Lee E., Cook D., and Klinke, S.  (2002)
 \emph{Projection Pursuit indices for supervised classification}

}
\author{Eun-kyung Lee}

\seealso{\code{{PPindex.class}}}



\examples{
data(iris)

PP.opt<-PP.optimize.random("LDA",1,iris[,1:4],iris[,5],cooling=0.999,temp=1)

PP.opt$index.best
PP.optimize.plot(PP.opt,iris[,1:4],iris[,5])


PP.opt<-PP.optimize.anneal("LDA",1,iris[,1:4],iris[,5],cooling=0.999,temp=1,energy=0.01)
PP.opt$index.best

PP.optimize.plot(PP.opt,iris[,1:4],iris[,5])


PP.opt<-PP.optimize.Huber("LDA",2,iris[,1:4],iris[,5],cooling=0.999,r=1)
PP.opt$index.best
PP.optimize.plot(PP.opt,iris[,1:4],iris[,5])


}
\keyword{optimize}
