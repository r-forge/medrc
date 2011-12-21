\name{ctb}
\alias{ctb}
\docType{data}
\title{CellTiter-Blue Cell Viability Assay Data}
\description{Neurotoxicity test using the CellTiter-Blue Cell Viability
  Assay on SH-SY5Y cells for increasing concentrations of acrylamide.}
\usage{data(ctb)}
\format{
  A data frame with 647 observations on the following 5 variables.
  \describe{
    \item{\code{well}}{well ID of a 96 well plate}
    \item{\code{conc}}{12 concentrations of acrylamide, ranging from
      0-500mM}
    \item{\code{fluorescence}}{measured fluorescence after adding the
      resazurin reagent into the wells}
    \item{\code{day}}{integer denoting 3 different days}
    \item{\code{plate}}{factor with 7 levels representing the plate ID}
  }
}
\references{
  Frimat, JP, Sisnaiske, J, Subbiah, S, Menne, H, Godoy, P, Lampen, P,
  Leist, M, Franzke, J, Hengstler, JG, van Thriel, C, West, J. The
  network formation assay: a spatially standardized neurite outgrowth
  analytical display for neurotoxicity screening. Lab Chip 2010; 10:701-709.
}

\examples{
data(ctb)

## set of nonlinear mixed models
ctb.LL4.mixed <- medrm(fluorescence  ~ conc, fct=LL.4(),
 data=ctb, random=d  ~ 1 | day/plate)
ctb.LN4.mixed <- medrm(fluorescence  ~ conc, fct=LN.4(),
 data=ctb, random=d  ~ 1 | day/plate)
ctb.W14.mixed <- medrm(fluorescence  ~ conc, fct=W1.4(),
 data=ctb, random=d  ~ 1 | day/plate)
ctb.W24.mixed <- medrm(fluorescence  ~ conc, fct=W2.4(),
 data=ctb, random=d  ~ 1 | day/plate)
ctb.FPL4a.mixed <- medrm(fluorescence ~ conc, fct=FPL.4(-2, 0.5),
 data=ctb, random=d  ~ 1 | day/plate)
ctb.FPL4b.mixed <- medrm(fluorescence ~ conc, fct=FPL.4(-1, 1),
 data=ctb, random=d ~ 1 | day/plate)
ctb.FPL4c.mixed <- medrm(fluorescence ~ conc, fct=FPL.4(-1, 2),
 data=ctb, random=d  ~ 1 | day/plate)
ctb.FPL4d.mixed <- medrm(fluorescence ~ conc, fct=FPL.4(-0.5, 3),
 data=ctb, random=d  ~ 1 | day/plate)

## information criteria
medrcselect(ctb.LL4.mixed, ctb.LN4.mixed, ctb.W14.mixed,
            ctb.W24.mixed, ctb.FPL4a.mixed, ctb.FPL4b.mixed,
            ctb.FPL4c.mixed, ctb.FPL4d.mixed)

## BMDL estimation for BMR 1, 5, 10, 20
mmaED(ctb.LL4.mixed, ctb.LN4.mixed, ctb.W14.mixed,
      ctb.W24.mixed, ctb.FPL4a.mixed, ctb.FPL4b.mixed,
      ctb.FPL4c.mixed, ctb.FPL4d.mixed,
      respLev=c(1,5,10,20),
      interval = "buckland", bmd = "additional")
}

\keyword{datasets}
