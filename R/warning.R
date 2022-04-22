.warning <- function(...,w=NULL) {
  warning(...,call.=FALSE)
  tmp <- list(NULL)
  names(tmp) <- paste(...,sep="")
  return(c(w,tmp))
}
