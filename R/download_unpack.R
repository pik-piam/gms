#' Download and unpack compressed data from repositories
#' 
#' Downloads a list of tgz files from a list of repos and unpacks them
#' 
#' @param input a vector of files to be downloaded or a cfg list with settings to be used (e.g. containing
#' cfg$input, cfg$repositories). Settings in the config list will be overwritten by other arguments of
#' this function if they are not set to NULL
#' @param targetdir directory the files should be downloaded and extracted to
#' @param repositories a vector of repositories the files should be looked for. Files will be searched
#' for in all repositories until they are found, always starting with the first repository in the 
#' list.
#' @param username username for accessing the repo (if required). If set to NULL will be tried to be set
#' through getOption("username")
#' @param ssh_private_keyfile path to private ssh key (if required). If set to NULL will be tried to be set
#' through getOption("ssh_private_keyfile")
#' @param ssh_public_keyfile path to public ssh key (if required). If set to NULL will be tried to be set
#' through getOption("ssh_public_keyfile")
#' @param debug switch for debug mode with additional diagnostic information
#' @param ... further options provided to curl handle
#' @author Jan Philipp Dietrich
#' @importFrom utils untar
#' @export

download_unpack <- function(input, targetdir="input", repositories=NULL, username=NULL, ssh_private_keyfile=NULL, ssh_public_keyfile=NULL, debug=FALSE, ...) {
  
  if(is.list(input)) {
    files <- input$input
    if(is.null(repositories))        repositories <- input$repositories
    if(is.null(username))            username <- input$username
    if(is.null(ssh_private_keyfile)) ssh_private_keyfile <- input$ssh_private_keyfile
    if(is.null(ssh_public_keyfile))  ssh_public_keyfile <- input$ssh_public_keyfile
    if(is.null(debug))               debug <- input$debug
  } else {
    files <- input
  }
  
  if(is.null(username)) username <- getOption("username")
  if(is.null(ssh_private_keyfile)) ssh_private_keyfile <- getOption("ssh_private_keyfile")
  if(is.null(ssh_public_keyfile)) ssh_public_keyfile <- getOption("ssh_public_keyfile")
  
  if(!dir.exists(targetdir)) dir.create(targetdir)
  
  writeLines(files,paste0(targetdir,"/source_files.log"))
  
  # create curl handle
  if(any(grepl("://",repositories))) {
    if(!requireNamespace("curl", quietly = TRUE)) stop("The package curl is required for downloading files!")
    h <- curl::new_handle(verbose=debug)
    if(!is.null(username)) try(curl::handle_setopt(h, username=username))
    if(!is.null(ssh_private_keyfile)) try(curl::handle_setopt(h, ssh_private_keyfile=ssh_private_keyfile))
    if(!is.null(ssh_public_keyfile)) try(curl::handle_setopt(h, ssh_public_keyfile=ssh_public_keyfile))
    if(length(list(...))>0) try(curl::handle_setopt(h, ...))
  }
  
  cat("Load data from repository\n")
  anydatafound <- FALSE
  md5sum <- list()
  for(i in 1:length(files)) {
    file <- files[i]
    cat(" Search for",file,"...\n")
    filepath <- NULL
    for(repo in repositories) {
      cat("  Try",repo,"...")
      path <- paste0(sub("/$","",repo),"/",file)
      if(grepl("://",path)) {
        tmpdir <- tempdir()
        if(debug) tmpdir <- targetdir
        tmp <- try(curl::curl_download(path,paste0(tmpdir,"/",file),handle=h),silent = !debug)
        if("try-error" %in% class(tmp)) {
          cat(" failed!\n")
          if(debug) {
            cat(tmp)
          }
        } else {
          names(files)[i] <- repo
          filepath <- paste0(tmpdir,"/",file)
          cat(" success!\n")
          break
        }
      } else {
        if(file.exists(path)) {
          names(files)[i] <- repo
          filepath <- path
          cat(" success!\n")
          break
        } else {
          cat(" failed!\n")
        }
      }
    }
    if(is.null(filepath)) {
      warning(file," could not be found in any of the specified repositories!")
    } else {
      anydatafound <- TRUE
      md5sum[[files[i]]] <- tools::md5sum(filepath)
      untar(filepath,exdir=targetdir)
    }
  }
  if(!anydatafound) stop("None of the provided input data files could be found! In the case of remote access: Did you provide proper username and ssh_private_keyfile?")
  attr(files,"md5") <- md5sum
  return(files)
}