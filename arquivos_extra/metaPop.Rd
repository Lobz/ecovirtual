\name{metaPop}

\title{
Metapopulation Model
}

\description{
Simulate a metapopulation dynamics
}

\usage{
metaPop(cl,rw, fi, pc, pe, tmax)
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
  probability of colonization
}
  \item{pe}{
  probability of extinction
}
  \item{tmax}{
    maximum simulation time
}
}

\value{
return a grafic with the simulation and an array with all simulation data
} 

\references{
Gotelli, N.J. 1991. Metapopulation models: the rescue effect, the propagule rain, and the core-satellite hypothesis. The American Naturalist 138:768-776.
}

\author{
Alexandre Adalardo de Oliveira <aleadalardo@gmail.com>
}

\examples{
metapop(tmax=100,cl=10,rw=10,fi=0.5,pc=0.3,pe=0.15)
}
\keyword{metapopulation}
\keyword{simulation}
