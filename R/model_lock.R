#' Model lock/unlock
#' 
#' Functions that indicate whether a model folder is currently locked by
#' another process or not. This helps to prevent unintended interactions
#' between processes.
#' 
#' 
#' @aliases model_lock model_unlock
#' @usage model_lock(folder=".", file=".lock", timeout1=NULL, timeout2=NULL,
#' check_interval=1, oncluster=TRUE)
#' model_unlock(id,folder=".",file=".lock",oncluster=TRUE)
#' @param folder model folder
#' @param file file name of the lock file containing the process queue
#' @param timeout1 Time in hours the top process in the queue is allowed to run
#' before the current process is stopped.
#' @param timeout2 Time in hours the processed is allowed to wait in the queue
#' before it is stopped
#' @param check_interval Time in seconds between checking the current position
#' in the queue.
#' @param id process id as returned by model_lock.
#' @param oncluster a logical indicating whether the script is run on cluster
#' or not. On windows a lock file is created, which does not prevent
#' simulatneous access to the model. On the cluster the system command 'mkdir'
#' is used to prevent simultaneous access. This atomicity of check-and-create
#' is ensured at the operating system kernel level.
#' @return model_lock returns the process id which is needed (only on Windows)
#' to identify the process in model_unlock.
#' @author Jan Philipp Dietrich, David Klein
#' @export
#' @seealso \code{\link{check_config}}
#' @examples 
#' #lock folder
#' id <- model_lock(tempdir())
#' 
#' #unlock folder
#' model_unlock(id,tempdir())
model_lock <- function(folder=".",file=".lock",timeout1=NULL,timeout2=NULL,check_interval=1,oncluster=TRUE) {
  .queue_ready <- function(id,lock_queue) {
    return(as.integer(lock_queue[1,1])==id)
  }
  
  .gdate <- function(x) {
    return(as.POSIXct(substring(x,5),format="%b %d %H:%M:%S %Y")) 
  }
  
  lfile <- path(folder,file)

  if(!oncluster) {
      start_date <- date()
      if(file.exists(lfile)) {
        load(lfile)
        #create id
        id <- max(as.integer(lock_queue[,1])) + 1
        #add entry to queue
        lock_queue <- rbind(lock_queue,c(id=id,date=date()))
        save(lock_queue,file=lfile)
        
        #wait for being the first in the queue
        message("Start waiting in the queue...")
        while(!.queue_ready(id,lock_queue)){
          load(lfile)
          #check whether the running process is already running longer than the timeout time
          if(!is.null(timeout1)) if(as.numeric(.gdate(date()) - .gdate(lock_queue[1,2]),units="hours") > timeout1) {
            model_unlock(id,folder,file)
            stop("Timeout! The lock date of the running process (",lock_queue[1,2],") is older than the timeout1 time (",timeout1," hours). This could indicate that there was an error in a previous run. Please check your model folder!")
          }
          if(!is.null(timeout2)) if(as.numeric(.gdate(date()) - .gdate(start_date),units="hours") > timeout2) {
            model_unlock(id,folder,file)
            stop("Timeout! The process was waiting longer than timeout2 (",timeout2," hours) in the queue and has therefore be stopped!")
          }
          Sys.sleep(check_interval)
        }
        message("...Waiting finished. Ready to start!")
        #refresh date
        lock_queue[1,2] <- date()
        
      } else {
        id <- 1
        lock_queue <- matrix(c(id=id,date=date()),1,2)
      } 
      save(lock_queue,file=lfile)
  }
  else { # If running on cluster
    if(system(paste("mkdir",lfile),intern=F,ignore.stdout=T,ignore.stderr=T)) {
      message("The model folder is already locked by another process. Waiting for unlock...")
      while(system(paste("mkdir",lfile),intern=F,ignore.stdout=T,ignore.stderr=T)) {
        Sys.sleep(check_interval)
      }
      message("The model folder was unlocked by another process.")
    }
    message("The model folder was locked by this process.")
    id<-NULL
  }
  return(id)  
}
