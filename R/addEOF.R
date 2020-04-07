#' add EOF comment
#' 
#' Add EOF text to all files in the code in order to ease debugging
#' 
#' 
#' @param path path to the main folder of the model
#' @param filetypes file types the function should be applied to
#' @author Anastasis Giannousakis
#' @seealso \code{\link{removeEOF}}
#' @export
#' @examples
#' 
#' \dontrun{
#' addEOF()}
#' 
addEOF<-function(path = ".",filetypes=c("inc","prn","gms")) {
  
  all_files <- list.files(path = path, pattern = paste("\\.(",paste(filetypes,collapse="|"),")$",sep=""),recursive = TRUE)
  
  for (i in 1:length(all_files)) {
    expr<-paste("*** EOF ",all_files[i],sep="")
    cat(expr,file=all_files[i],append=TRUE)
  }
}

