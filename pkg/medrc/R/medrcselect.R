medrcselect <-
function(object, ...){
  lllist <- list(object, ...)
  ismedrc <- sapply(lllist, function(x) inherits(x, "medrc"))
  mllist <- lllist[ismedrc]
  if (length(mllist) == 0) stop("No medrc object available")
  msl <- sapply(mllist, function(x) x$mselect)
  Call <- match.call()
  Call$k <- NULL
  colnames(msl) <- as.character(Call[-1L])[ismedrc]
  t(msl)
}

