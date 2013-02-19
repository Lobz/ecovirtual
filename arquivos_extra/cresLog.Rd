\name{crescLog}

\title{
Logistic Population Growth
}

\description{
Simulate a logistic population growth for discrete and continuos populations models.
}

\usage{
crescLog(N0, r, K, tmax)
}

\arguments{
  \item{N0}{
    initial population size.
}
  \item{r}{
    intrinsic growth rate.
}
  \item{K}{
    carrying capacity.
}
  \item{tmax}{
    maximum simulation time.
}
}

\details{
computes the logistic population for continuos growth model:
Nt = K/1+[(K-N0)/N0]exp(-rt)
and for discrete growth model:
Nt = N0+rd*N0(1-N0/K)


}

\value{
'crescLog' return a grafic with the population size in time for the continuous and discrete population models.

\references{
Stevens, MHH. 2009. A Primer of Eoclogy with R. Springer, New York.

}

\author{
Alexandre Adalardo de Oliveira <aleadalardo@gmail.com>
}

\examples{
crescLog(N0=10, r=0.05, K=80, tmax=100)
}

\keyword{population dynamics}
\keyword{simulation}
