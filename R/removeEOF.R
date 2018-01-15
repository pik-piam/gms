#' add EOF comment
#' 
#' remove EOF text from all files in the code
#' 
#' 
#' @usage removeEOF(path = ".",filetypes=c("inc","prn","gms"))
#' @param path path to the main folder of the model
#' @param filetypes file types the function should be applied to
#' @author Anastasis Giannousakis
#' @export
#' @seealso \code{\link{addEOF}}
#' @examples
#' 
#' \dontrun{
#' removeEOF()}
#' 
removeEOF<-function(path = ".",filetypes=c("inc","prn","gms")) {

all_files <- list.files(path = path, pattern = paste("\\.(",paste(filetypes,collapse="|"),")$",sep=""),recursive = TRUE)

for (i in 1:length(all_files)) {
  values <- suppressWarnings(readLines(all_files[i]))
  if (grepl("EOF",values[length(values)])) {
    values[length(values)]<-""
    writeLinesDOS(values,all_files[i])
  }
}
}