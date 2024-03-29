plot.medrc <- function(x, ..., ndose=25, ranef=FALSE, level=NULL, logx=FALSE){
  require(ggplot2)
  require(plyr)
  fct <- x$fct
  makehelpfunction(fct)
  
  yname <- as.character(x$form)[2]
  xname <- as.character(x$form)[3]
  if (is.null(x$curveid)){
    pform <- x$form
  } else {
    fname <- as.character(x$curveid)[3]
    pform <- paste(yname, "~", xname, "+", fname)
  }
  mf <- model.frame(pform, data=x$data)
  
  if (logx == TRUE){
    if (min(mf[,2]) == 0){
      m0 <- mean(unique(mf[,2])[order(unique(mf[,2]))][1:2])
    } else {
      m0 <- min(mf[,2]) 
    }
    dr <- exp(seq(log(m0), log(max(mf[,2])), length=ndose))
  } else {
    dr <- seq(min(mf[,2]), max(mf[,2]), length=ndose)
  } 
  
  if (is.null(x$curveid)){
    if (ranef == TRUE){
      if (is.null(level)){
        cf <- coef(x$fit)
      } else {
        cf <- coef(x$fit, level=level)
      }
      predictions <- stack(data.frame(apply(cf, 1, function(para) x$fct$fct(dr, rbind(para)))))$values
      pdat <- data.frame(predictions, dose=rep(dr, times=nrow(cf)), ID=rep(rownames(cf), each=ndose))   
      if (logx == TRUE){
        eval(parse(text=paste("ggplot(mf, aes(x=log(",xname,"), y=",yname,")) + geom_point(alpha=0.3) + geom_line(data=pdat, aes(x=log(dose), y=predictions, colour=ID))", sep="")))
      } else {
        eval(parse(text=paste("ggplot(mf, aes(x=",xname,", y=",yname,")) + geom_point(alpha=0.3) + geom_line(data=pdat, aes(x=dose, y=predictions, colour=ID))", sep="")))
      }
    } else {      
      predictions <- x$fct$fct(dr, rbind(fixef(x$fit)))
      pdat <- data.frame(predictions, dose=dr)   
      if (logx == TRUE){
        eval(parse(text=paste("ggplot(mf, aes(x=log(",xname,"), y=",yname,")) + geom_point(alpha=0.3) + geom_line(data=pdat, aes(x=log(dose), y=predictions), colour='blue3')", sep="")))
      } else {
        eval(parse(text=paste("ggplot(mf, aes(x=",xname,", y=",yname,")) + geom_point(alpha=0.3) + geom_line(data=pdat, aes(x=dose, y=predictions), colour='blue3')", sep="")))
      }
    }
  } else {
    if (ranef == TRUE){
      print("Sorry, ranef=TRUE for different curveids is not yet implemented...")
    } else {
      flev <- length(levels(mf[,3])) 
      cf <- matrix(coefficients(x), ncol=flev, byrow=TRUE)
      predictions <- stack(data.frame(apply(cf, 2, function(para) x$fct$fct(dr, rbind(para)))))$values
      pdat <- data.frame(predictions, dose=rep(dr, times=ncol(cf)), curve=rep(levels(mf[,3]), each=ndose))
      if (logx == TRUE){
        eval(parse(text=paste("ggplot(mf, aes(x=log(",xname,"), y=",yname,", colour=", fname,")) + geom_point(alpha=0.3) + geom_line(data=pdat, aes(x=log(dose), y=predictions, colour=curve))", sep="")))
      } else {
        eval(parse(text=paste("ggplot(mf, aes(x=",xname,", y=",yname,", colour=", fname,")) + geom_point(alpha=0.3) + geom_line(data=pdat, aes(x=dose, y=predictions, colour=curve))", sep="")))
      }
    }    
  }  
}

