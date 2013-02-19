\name{metaCiEr}

\title{
Internal Colonization And Rescue Effect Simulation
}

\description{
Simulate a population with internal colonization and rescue effect
}

\usage{
metaCiEr(cl, rw, fi, ci, ce, tmax)
}

\arguments{
  \item{cl}{
     arena number of colunms
}
  \item{rw}{
      arena number of rowns
}
  \item{fi}{
     fraction of patches ocuppied
}
  \item{ci}{
     colonization coeficient
}
  \item{ce}{
     extinction coeficiente
}
  \item{tmax}{
   maximum simulation time
}
}

value{
array with simulation data and a graphic device representing it
}

\references{
Gotelli, N.J. 1991. Metapopulation models: the rescue effect, the propagule rain, and the core-satellite hypothesis. The American Naturalist 138:768-776.
}

\author{
Alexandre Adalardo de Oliveira <aleadalardo@gmail.com>}

\examples{
metaCiEr(tf=100, cl=10, ln=10, fi=0.2, i=0.2, e=0.15)
}

\keyword{simulation}
\keyword{metapopulation}
