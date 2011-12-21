medrm <-
function(form, data, fct, random, correlation=NULL, weights=NULL, control=NULL, start=NULL){
  require(nlme)
  require(drc)
  callDetail <- match.call()
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
  if (is.null(start)){
    md <- drm(form, data=data, fct=fct)
    ini <- coefficients(md)
    names(ini) <- fct$names
    rstr <- reStruct(eval(parse(text=paste("(", deparse(callDetail$random), ")", sep=""))))
    nr <- names(rstr)
    if (length(nr) == 1){
      cf <- data[,names(rstr)]
      assign("cf", cf, envir=.GlobalEnv)
      mdr <- drm(form, curve=cf, fct=fct, data=data)
      wr <- names(unlist(sapply(fct$names, function(x) grep(x, as.character(random[2])))))
      srand <- scale(matrix(coefficients(mdr), nrow=length(levels(cf)), dimnames=list(unique(cf), fct$names)), scale=FALSE)[, which(fct$names %in% wr), drop=FALSE]
      ini <- list(fixed = ini, random=srand)
    } 
  } else {
    ini <- start
  }
  mform <- deparse(as.formula(paste(as.character(form)[2], as.character(form)[1], "drcfunction(", as.character(form)[3], ", ", paste(fct$names, collapse=", "), ")", sep="")))
  mfixed <- deparse(as.formula(paste(paste(fct$names, collapse=" + "), "~ 1")))
  nlmecall <- paste("nlme(", mform, ", fixed=", mfixed, ", data=data, random=", deparse(callDetail$random), ", start=ini, correlation=correlation, weights=weights, control=control)", sep="")
  fmmixed <- eval(parse(text=nlmecall))
  ### output
  out <- list()
  out$call <- callDetail
  out$form <- form
  out$rform <- random
  out$fit <- fmmixed
  out$fct <- fct
  out$data <- data
  out$coefficients <- fixef(fmmixed)
  out$parmMat <- cbind(fixef(fmmixed))
  colnames(out$parmMat) <- "1"
  out$indexMat <- cbind(1:length(fixef(fmmixed)))
  out$type = "continuous"
  pll <- attr(logLik(fmmixed), "df")
  nll <- nobs(logLik(fmmixed))
  out$mselect <- c("AIC"=AIC(fmmixed), "AICc"=AIC(fmmixed) + ((2*pll*(pll+1))/(nll-pll-1)), "BIC"=BIC(fmmixed), "logLik"=logLik(fmmixed), "df"=pll)
  out$start <- ini
  class(out) <- c("medrc", "drc")
  return(out)
}

