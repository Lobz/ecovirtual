\name{metaEr}

\title{
  Rescue Effect Simulation
}

\description{
  Simulate a population with rescue effect
}

\usage{
metaEr(cl, rw, fi, pc, ce, tmax)
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
  \item{pc}{
     colonization probability
}
  \item{ce}{
     extiction coeficient
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
Alexandre Adalardo de Oliveira <aleadalardo@gmail.com>}

\examples{
metaEr(tmax=100, cl=10, rw=10, fi=0.2, pc=0.2, ce=0.15)
}

\keyword{simulation}
\keyword{metapopulation}
