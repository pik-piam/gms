#' rmAllbut
#' 
#' Removes all objects except specified ones from the workspace
#' 
#' Helps to clean the workspace. Only objects specified in \code{...} survive.
#' Specify clean =TRUE to really free the memory.
#' 
#' @param ... Objects that should be kept
#' @param list List specifying the objects to be kept. Same as in
#' \code{\link[base]{rm}}.
#' @param clean Boolean to specify if a \code{\link[base]{gc}} shall be
#' executed
#' @author Markus Bonsch
#' @export
#' @seealso \code{\link[base]{rm}},\code{\link[base]{ls}}
#' @examples
#' 
#' #Create some objects
#' a<-1
#' b<-2
#' c<-3
#' #show them
#' ls()
#' #delete all but b and c
#' rmAllbut(b,c)
#' ls()
#' #delete all but b
#' test<-"b"
#' rmAllbut(list=test)
#' ls()
#' 
rmAllbut<-function(...,list=character(),clean=TRUE){
  keepObjects<-as.list(substitute(list(...)))
  keepObjects[[1]]<-NULL
#   return(keepObjects)
# }
  keepObjects<-lapply(keepObjects,as.character)
  if(!is.null(list)){ 
    if(all(sapply(list,is.character))){
      keepObjects<-append(keepObjects,list)
    } else {
      stop("list argument can only contain characters")  
    }
  }
  if(length(keepObjects)==0) stop("No argument to rmAllbut specified. Use rm(list=ls()) instead")
  wsp<-ls(name=sys.frame(which=-1))
  wsp<-wsp[-which(wsp %in% as.vector(keepObjects))]
  rm(list=wsp,pos=sys.frame(which=-1))
  if(clean) gc()
}