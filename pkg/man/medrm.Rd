\name{medrm}
\alias{medrm}

\title{Mixed effects dose-response curves}
\description{
Implements drc nonlinear functions into the nlme framework for mixed
effects dose-response modeling.
}
\usage{
medrm(form, data, fct, random, correlation = NULL, weights = NULL, control = NULL, start = NULL)
}

\arguments{
  \item{form}{Formula describing the dose-response relationship}
  \item{data}{a data.frame object}
  \item{fct}{a function of the drc package}
  \item{random}{a list or one-sided formula describing the random effects}
  \item{correlation}{additional corClasses object}
  \item{weights}{additional varClasses object}
  \item{control}{list with nlme control arguments}
  \item{start}{optional list with initial values for the fixed and/or
  random components}
}

\value{An object of class medrc}

\author{Daniel Gerhard}

\seealso{
  \code{\link{drm}}, \code{\link{nlme}}
}
\examples{
library(nlme)
data(DNase)

library(ggplot2)
ggplot(DNase, aes(y=density, x=conc, colour=Run)) + geom_point()


## fit a 3, 4, and 5 parameter log-logistic model
# with random upper asymptote
mm3 <- medrm(density ~ conc, fct=LL.3(), data=DNase, random=d  ~ 1 | Run)
mm4 <- medrm(density ~ conc, fct=LL.4(), data=DNase, random=d  ~ 1 | Run)
mm5 <- medrm(density ~ conc, fct=LL.5(), data=DNase, random=d  ~ 1 | Run)

# model selection by information criteria
medrcselect(mm3, mm4, mm5)

## the mixed model framework allows even more flexibility
# by adding random terms
mm5b <- medrm(density ~ conc, fct=LL.5(), data=DNase, random=e + d  ~ 1 | Run)
mm5c <- medrm(density ~ conc, fct=LL.5(), data=DNase, random=b + e + d  ~ 1 | Run)
mm5d <- medrm(density ~ conc, fct=LL.5(), data=DNase, random=b + e + d + c  ~ 1 | Run)

medrcselect(mm5, mm5b, mm5c, mm5d)

### plot residuals vs fitted
plot(predict(mm5d), residuals(mm5d))


## adding an exponential variance function
mm5dv <- medrm(density ~ conc, fct=LL.5(), data=DNase, random= b + e + d + c ~ 1 | Run, weights=varExp(form=~conc))

plot(predict(mm5dv), residuals(mm5dv))

medrcselect(mm5d, mm5dv)

## plot predicted curves
# (for each run, fixed effects as black line)
plot(mm5dv)
# on logarithmic concentration scale
plot(mm5dv, logx=TRUE)

## estimated coefficients
mm5dv

### ED estimation based on fixed effects
ED(mm5dv, respLev=c(10,25,50,75,90), interval="delta")

### ED estimation based on model averaged fixed effects
mmaED(mm3, mm4, mm5, mm5b, mm5c, mm5d, mm5dv, respLev=c(10,25,50,75,90), interval="buckland", level = 0.95, bmd="none", background = 0.05, dmList = NULL)
}

\keyword{ models }
\keyword{ nonlinear }

