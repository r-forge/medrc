\name{nfa}
\alias{nfa}
\docType{data}
\title{Network Formation Assay Data}
\description{Neurotoxicity test using a network formation assay studying
  the inhibition of network formation at acrylamide exposure.}
\usage{data(nfa)}
\format{
  A data frame with 45 observations on the following 4 variables.
  \describe{
    \item{\code{chip}}{chip ID}
    \item{\code{conc}}{7 concentrations of acrylamide, ranging from
      0-5mM}
    \item{\code{experiment}}{factor with levels 1 or 2 denoting two
      consecutive experiments}
    \item{\code{response}}{Number of connections [\%]}
  }
}
\references{
  Frimat, JP, Sisnaiske, J, Subbiah, S, Menne, H, Godoy, P, Lampen, P,
  Leist, M, Franzke, J, Hengstler, JG, van Thriel, C, West, J. The
  network formation assay: a spatially standardized neurite outgrowth
  analytical display for neurotoxicity screening. Lab Chip 2010; 10:701-709.
}
\examples{
data(nfa)

## set of nonlinear mixed models
nfa.LL4.mixed <- medrm(response ~ conc, fct=LL.4(),
 data=nfa, random=d ~ 1 | experiment)
nfa.LN4.mixed <- medrm(response ~ conc, fct=LN.4(),
 data=nfa, random=d ~ 1 | experiment)
nfa.W14.mixed <- medrm(response ~ conc, fct=W1.4(),
 data=nfa, random=d ~ 1 | experiment)
nfa.W24.mixed <- medrm(response ~ conc, fct=W2.4(),
 data=nfa, random=d ~ 1 | experiment)

summary(nfa.LL4.mixed)
summary(nfa.LN4.mixed)
summary(nfa.W14.mixed)
summary(nfa.W24.mixed)


## information criteria
medrcselect(nfa.LL4.mixed, nfa.LN4.mixed,
            nfa.W14.mixed, nfa.W24.mixed)


## Background p0=0.01
nfa0.01 <- mmaED(nfa.LL4.mixed, nfa.LN4.mixed,
      nfa.W14.mixed, nfa.W24.mixed,
      respLev=c(1,5,10,20),
      interval = "buckland", bmd = "extra", background=0.01,
      dmList=list(LL4.loged, LN4.loged,
                  W14.loged, W24.loged))
exp(nfa0.01$retMat[, c(1, 3)])

## Background p0=0.05 (default)
nfa0.05 <- mmaED(nfa.LL4.mixed, nfa.LN4.mixed,
      nfa.W14.mixed, nfa.W24.mixed,
      respLev=c(1,5,10,20),
      interval = "buckland", bmd = "extra", background=0.05,
      dmList=list(LL4.loged, LN4.loged,
                  W14.loged, W24.loged))
exp(nfa0.05$retMat[, c(1, 3)])

## Background p0=0.1
nfa0.10 <- mmaED(nfa.LL4.mixed, nfa.LN4.mixed,
      nfa.W14.mixed, nfa.W24.mixed,
      respLev=c(1,5,10,20),
      interval = "buckland", bmd = "extra", background=0.1,
      dmList=list(LL4.loged, LN4.loged,
                  W14.loged, W24.loged))
exp(nfa0.10$retMat[, c(1, 3)])

}


\keyword{datasets}
