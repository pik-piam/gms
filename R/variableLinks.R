variableLinks <- function(x,name) {
  tmp <- sapply(x,function(x,y)return(y%in%x),name)
  return(names(tmp)[tmp])
}