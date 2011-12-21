df.residual.medrc <-
function(object, ...){
 summary(object$fit)$fixDF$X["e"]
}

