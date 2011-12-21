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

## information criteria
medrcselect(nfa.LL4.mixed, nfa.LN4.mixed,
            nfa.W14.mixed, nfa.W24.mixed)

## BMDL estimation for BMR 1, 5, 10, 20
mmaED(nfa.LL4.mixed, nfa.LN4.mixed,
      nfa.W14.mixed, nfa.W24.mixed,
      respLev=c(1,5,10,20),
      interval = "buckland", bmd = "additive",
      dmList=list(LL4.loged, LN4.loged,
                  W14.loged, W24.loged))
}


\keyword{datasets}
