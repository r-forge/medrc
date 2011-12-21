vcov.medrc <-
function(object, ...){
  est <- fixef(object$fit)
  stdFixed <- sqrt(diag(as.matrix(object$fit$varFix)))
  std <- sqrt(object$fit$dims$N/(object$fit$dims$N - length(stdFixed))) * stdFixed
  cr <- array(t(object$fit$varFix/stdFixed)/stdFixed, dim(object$fit$varFix), list(names(est), names(est)))
  return(cor2cov(round(cr,10), std))
}

