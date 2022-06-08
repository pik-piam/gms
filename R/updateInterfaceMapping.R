#' updateInterfaceMapping
#' 
#' Function to update the mapping between interfaces and their origin modules.
#' 
#' 
#' @param path path of the main folder of the model
#' @param modulepath path to the module folder relative to "path"
#' @author Jan Philipp Dietrich
#' @export
#' @importFrom utils read.csv write.csv
#' @seealso \code{\link{codeCheck}}

updateInterfaceMapping <- function(path=".",modulepath="modules") {
  choose_module <- function(modules,title) {
    message("\n\n",title,":\n\n")
    message(paste(1:length(modules), modules, sep=": ", collapse="\n"))
    message("\nNumber: ")
    identifier <- getLine()
    identifier <- as.numeric(strsplit(identifier,",")[[1]])
    if (any(!(identifier %in% 1:length(modules)))) stop("This choice (",identifier,") is not possible. Please type in a number between 1 and ",length(modules))
    return(modules[identifier])
  }
  
  a <- codeCheck(path,modulepath="modules")  
  interfaces <- unique(as.vector(unlist(a)))
  
  #read current table
  out <- read.csv(path(path,modulepath,"module_interface_mapping.csv"))
  out[,1] <- as.character(out[,1])
  out[,2] <- as.character(out[,2])
  rownames(out) <- out[,"interface"]

  #remove nonexisting interfaces
  out <- out[rownames(out) %in% interfaces,]
  
  tmp <- matrix(NA,length(setdiff(interfaces,rownames(out))),2)
  rownames(tmp) <- setdiff(interfaces,rownames(out))
  tmp[,1] <- setdiff(interfaces,rownames(out))
  
  colnames(tmp) <- colnames(out)
  out <- rbind(out,tmp)
  
  tmp_in <- function(x,y) return(y%in%x)
  for(i in interfaces[is.na(out[interfaces,"module"])]) {
    m <- sapply(a,tmp_in,i)
    out[i,2] <- paste(choose_module(names(m)[m],paste("To which module belongs the interface ",i,"?",sep="")),collapse="|")  
  }
  write.csv(out,path(path,modulepath,"module_interface_mapping.csv"),row.names=FALSE,eol="\r\n")
}
