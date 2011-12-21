maplot <-
function(object, ..., ic="AIC", logx=FALSE){
  require(ggplot2)
  lllist <- list(object, ...)
  ismedrc <- sapply(lllist, function(x) inherits(x, "medrc"))
  mllist <- lllist[ismedrc]
  if (length(mllist) == 0) stop("No medrc object available")
  Call <- match.call()
  Call$ic <- NULL
  Call$logx <- NULL
  nresponse <- as.character(object$form[[2]])
  ndose <- as.character(object$form[[3]])
  response <- object$data[,nresponse]
  dose <- object$data[,ndose]
  odat <- data.frame(response, dose)
  if (logx == TRUE) dseq <- exp(seq(log(min(dose)), log(max(dose)), length=250)) else dseq <- seq(min(dose), max(dose), length=250)
  pdat <- data.frame(dose=dseq)
  names(pdat) <- ndose
  fixpred <- data.frame(sapply(mllist, function(object){
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
    predict(object$fit, newdata=pdat, level=0)
  }))
  colnames(fixpred) <- as.character(Call[-1L])[ismedrc]
  mmdat <- melt(data.frame(pdat, fixpred), id=ndose)
  names(mmdat) <- c("dose", "variable", "value")

  msl <- sapply(mllist, function(x) x$mselect)
  colnames(msl) <- as.character(Call[-1L])[ismedrc]
  aic <- t(msl)[,ic]
  d <- aic-min(aic)
  wi <- exp(-0.5*d)/sum(exp(-0.5*d))
  wpred <- apply(fixpred, 1, function(x) sum(x*wi))
  wmdat <- data.frame(dose=dseq, response=wpred)
        
  ggplot(odat, aes(y=response, x=dose)) + geom_point() + geom_line(data=mmdat, aes(y=value, colour=variable)) + geom_line(data=wmdat, aes(colour=NULL), colour="black", linetype=2) + ylab(nresponse) + xlab(ndose)
}

