#' @export
model_unlock <- function(id,folder=".",file=".lock",oncluster=TRUE) {
  lfile <- path(folder,file)
  if(!oncluster) {
      if(!file.exists(lfile)) stop("Lock file does not exist!")
      load(lfile)
      row <- which(as.integer(lock_queue[,1])==id)
      if(length(row)==0) stop("Could not find a process with the given id!")
      if(length(row)>1) stop("More than one lock entry for the given id!")
      if(dim(lock_queue)[1]>1) {
        lock_queue <- lock_queue[-row,,drop=FALSE]
        save(lock_queue,file=lfile)
      } else {
        unlink(lfile)  
      }
      cat("...entry removed from queue!\n")
  }
  else {
    if(!system(paste0("rm -r ",lfile),intern=F,ignore.stdout=T,ignore.stderr=T)) {
      cat("The model was unlocked\n")
    } else {
      stop("Lockfile does not exist")
    }
  }
}