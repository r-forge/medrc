residuals.medrc <-
function(object, ...){
  fct <- object$fct
  drcfunction <- function(){
    if (is.null(fct$fixed)) fct$fixed <- rep(NA, length(fct$names))
    parmVec <- fct$fixed
    notFixed <- is.na(parmVec)
    numParm <- length(parmVec)
    .value <- fct$fct(dose, eval(parse(text=paste("cbind(", paste(fct$names, collapse=", "),")", sep = ""))))
    .actualArgs <- as.list(match.call()[fct$names])
    if (all(unlist(lapply(.actualArgs, is.name)))) {
      .grad <- fct$deriv1(dose, eval(parse(text=paste("cbind(", paste(fct$names, collapse=", "),")", sep = ""))))
      dimnames(.grad) <- list(NULL, .actualArgs)
      attr(.value, "gradient") <- .grad
    }
    .value
  }
  aargs <- paste("dose=,", paste(fct$names, collapse="=,"), "=", sep="")
  formals(drcfunction) <- eval(parse(text = paste("alist(", aargs,")", sep = "")))
  attr(drcfunction, "pnames") <- fct$names
  attr(drcfunction, "class") <- "selfStart"
  attr(drcfunction,"initial") <- function(mCall, data, LHS){
    xy <- sortedXyData(mCall[["dose"]], LHS, data)
    val <- fct$ssfct(xy)
    names(val) <- fct$names
    val
  }
  assign("drcfunction", drcfunction, envir=.GlobalEnv)
  resid(object$fit, type="n")
}
