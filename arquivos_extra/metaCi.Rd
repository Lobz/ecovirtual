\name{metaCi}

\title{
Internal Colonization for metapopulations
}

\description{
Model a population with internal colonization
}

\usage{
metaCi(cl, rw, fi, ci, pe, tmax)
}

\arguments{
  \item{cl}{
  number of columns
}
  \item{rw}{
  number of rowns
}
  \item{fi}{
  initial fraction of patchs occupaded
}
  \item{ci}{
  colonization coefficient
}
  \item{pe}{
  probability of extinction
}
 \item{tmax}{
  maximum simulation time
}
}

\value{
array with simulation data and a graphic device representing it
}

\references{
Gotelli, N.J. 1991. Metapopulation models: the rescue effect, the propagule rain, and the core-satellite hypothesis. The American Naturalist 138:768-776.
}

\author{
Alexandre Adalardo de Oliveira <aleadalardo@gmail.com>
}

\examples{
metaCi(tmax=100,cl=10,rw=10,fi=0.1,ci=1,pe=0.5)
}

\keyword{metapopulation}
\keyword{simulation}
