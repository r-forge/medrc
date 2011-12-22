plot.medrc <-
function(x, ..., logx=FALSE){
  require(ggplot2)
  fct <- x$fct
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
  nresponse <- as.character(x$form[[2]])
  ndose <- as.character(x$form[[3]])
  ngroup <- strsplit(as.character(x$rform[[3]][3]), split="/")[[1]]
  if (length(ngroup) > 1){
    many <- TRUE
    ngroup <- ngroup[1]
  } else many <- FALSE
  response <- x$data[,nresponse]
  dose <- x$data[,ndose]
  group <- x$data[,ngroup]
  odat <- data.frame(response, dose, group)
  if (logx == TRUE){
    if (min(dose) == 0){
      m0 <- mean(unique(dose)[order(unique(dose))][1:2])
      odat$dose[dose == 0] <- m0
      dose[dose == 0] <- m0
    }
    dseq <- exp(seq(log(min(dose)), log(max(dose)), length=250))
  } else {
    dseq <- seq(min(dose), max(dose), length=250)
  }
  
  pdat <- with(odat, expand.grid(dose=dseq, group=levels(group)))
  names(pdat) <- c(ndose, ngroup)
  if (many){
    pdat$response <- predict(x$fit, newdata=pdat, level=0)
  } else {
    pdat$response <- predict(x$fit, newdata=pdat)
  }
  pdat$fixresponse <- predict(x$fit, newdata=pdat, level=0)
  names(pdat) <- c("dose", "group", "response", "fixresponse")
  if (logx == TRUE){
    ggplot(odat, aes(x=dose, y=response, colour=group)) + geom_point() + geom_line(data=pdat) + geom_line(data=pdat, aes(y=fixresponse), colour="black") + xlab(ndose) + ylab(nresponse) + scale_colour_discrete(name=ngroup) + coord_trans(x = "log")
  } else {
    ggplot(odat, aes(x=dose, y=response, colour=group)) + geom_point() + geom_line(data=pdat) + geom_line(data=pdat, aes(y=fixresponse), colour="black") + xlab(ndose) + ylab(nresponse) + scale_colour_discrete(name=ngroup)
  }
}

