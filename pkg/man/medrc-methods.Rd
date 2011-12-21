\name{medrc-methods}
\alias{df.residual.medrc}
\alias{plot.medrc}
\alias{predict.medrc}
\alias{vcov.medrc}
\alias{summary.medrc}
\alias{residuals.medrc}
\alias{ranef.medrc}

\title{medrc methods}
\description{Methods for objects of class medrc}
\usage{
\method{df.residual}{medrc}(object, ...)
\method{plot}{medrc}(x, ..., logx = FALSE)
\method{predict}{medrc}(object, ..., newdata = NULL, level = NULL)
\method{residuals}{medrc}(object, ...)
\method{vcov}{medrc}(object, ...)
\method{summary}{medrc}(object, ...)
\method{ranef}{medrc}(object, ...)
}

\arguments{
  \item{object}{An object of class medrc}
  \item{x}{An object of class medrc}
  \item{logx}{If TRUE, plot x-axis on a logarithmic scale}
  \item{newdata}{an optional data frame to be used for obtaining the predictions. All variables used in the fixed and random effects models, as well as the grouping factors, must be present in the data frame. If missing, the fitted values are returned.}
  \item{level}{an optional integer vector giving the level(s) of grouping to be used in obtaining the predictions. Level values increase from outermost to innermost grouping, with level zero corresponding to the population predictions. Defaults to the highest or innermost level of grouping.}
  \item{...}{further arguments}
}

\author{Daniel Gerhard}



\keyword{ methods }
