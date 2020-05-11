#' buildLibrary
#' 
#' Builds R libraries. Includes checks for consistency.
#' 
#' This function is designed to help building R libraries. It performs the
#' following steps: \itemize{ \item Version: Determination
#' of a new version number (Can also be defined by the user). \item Date:
#' Determination of a the date of the build (Can also be defined by the user).
#' \item R check: Check whether the library is consistent and can be built.
#' \item Package building Builds the .zip and .tar.gz packages under windows.
#' Under linux, only the .tar.gz package is built. } The commit has to be performed by the user still.
#' 
#' @param lib Path to the package
#' @param cran If cran-like test is needed
#' @param update_type 1 if the update is a major revision, 2 (default) for minor, 3 for patch, 4 only for packages in development stage
#' @author Jan Philipp Dietrich, Anastasis Giannousakis, Markus Bonsch
#' @seealso \code{\link{codeExtract}},\code{\link{readDeclarations}}
#' @importFrom citation package2zenodo
#' @export
#' @examples
#' 
#' \dontrun{buildLibrary()}
#' 
buildLibrary<-function(lib=".",cran=TRUE, update_type=NULL){

    get_line <- function(){
    # gets characters (line) from the terminal or from a connection
    # and returns it
    if(interactive()){
      s <- readline()
    } else {
      con <- file("stdin")
      s <- readLines(con, 1, warn=FALSE)
      on.exit(close(con))
    }
    return(s);
  }  
  
  didYouPull <- function() {
    cat("Is your repository up-to-date? Did you pull immediatly before running this check? (yes/no)") 
    s <- get_line()
    if(!(tolower(s) %in% c("y","yes"))) stop("Please update your repository first, before you proceed!")
  }
  didYouPull()
  
  OS<-Sys.info()["sysname"]
  thisdir<-getwd()
  if(lib!=".") setwd(lib)
  on.exit(setwd(thisdir))
  
  ####################################################################
  #Remove the auxiliary Rcheck folders
  ###################################################################
  rcheckfolders <- grep(".Rcheck$",base::list.dirs(full.names = FALSE,recursive = FALSE),value=TRUE)
  unlink(rcheckfolders,recursive=TRUE)
  
  ####################################################################
  #Check if roxygen is used and run roxygenize if required
  ################################################################### 
  descfile<-readLines("DESCRIPTION")
  if(any(grepl("RoxygenNote",descfile))) {
    devtools::document(pkg=".",roclets=c('rd', 'collate', 'namespace', 'vignette'))
    roxygen <- TRUE
  } else {
    roxygen <- FALSE
  }
  
  ############################################################
  #check the library
  ############################################################
  ck <- devtools::check(".",cran=cran)
  
  #Filter warnings and notes which are accepted
  accepted_warnings <- c("Warning: package '.*' was built under R version",
                         "Warning: namespace '.*' is not available and has been replaced")
  for(aw in accepted_warnings) {
    ck$warnings <- grep(aw, ck$warnings, value=TRUE,invert=TRUE)
  }
  print(ck)
  
  if(length(ck$errors)>0) {
    stop("The package check showed errors. You need to fix these errors first before submission!")
  }  
  
  if(length(ck$warnings)>0) {
    stop("The package check showed warnings. You need to take care of these warnings first before submission!")
  }

  if(length(ck$notes)>0) {
    stop("The package check showed notes. You need to take care of these notes first before submission!")
  }
  
  ##########################################################
  #Check for version numbers
  ##########################################################
  #Version number in the man file
  #Version number in the description file
  descfile<-readLines("DESCRIPTION")
  descfile_version<-sub("[^(0-9)]*$","",sub("Version:[^(0-9)]*","",grep("Version",descfile,value=T),perl=T),perl=T)
  
  version <- descfile_version
  
  autoversion <- function(old_version, upt, defLengths=3) {
    old_version <- numeric_version(old_version)
    if(upt==0) return(old_version)
    for(i in 1:upt) if(is.na(old_version[1,i])) old_version[1,i] <- 0
    if(old_version[1,upt] == 0 & upt==4) old_version[1,upt] <- 9000
    old_version[1,upt] <- as.numeric(old_version[1,i]) + 1
    if(defLengths>upt) {
      for(i in (upt+1):defLengths) {
        old_version[1,i] <- 0
      }
    }
    old_version <- old_version[1,1:max(upt,defLengths)]
    return(old_version)
  }

  
  choose_module <- function(Rfolder,title="Package check successful! Please choose an update type") {
    update_type <- c("major revision (for major rewrite of the whole package)", 
                     "minor revision (for new features or improvements)", 
                     "patch (for bugfixes and corrections)", 
                     "only for packages in development stage",
                     "no version increment (only to use if version is already incremented!)")
    cat(title,":\n")
    cat(paste(c(1:(length(update_type)-1),0), update_type, sep=": " ),sep="\n")
    cat("\nNumber: ")
    identifier <- get_line()
    identifier <- as.numeric(strsplit(identifier,",")[[1]])
    if (any(!(identifier %in% (1:length(update_type)-1)))) stop("This choice (",identifier,") is not possible. Please type in a number between 0 and ",length(update_type)-1)
    return(identifier)
  }
  if (is.null(update_type)) update_type <- choose_module(".")
  version <- autoversion(version,update_type)
  
  #Change the version in descfile
  descfile[grep("Version",descfile)]<-sub(descfile_version,version,descfile[grep("Version",descfile)])
  
  ############################################################
  #Check for the date
  ############################################################
  if(any(grepl("Date:",descfile))) {
    descfile_date<-sub("[^(0-9)]*$","",sub("Date:[^(0-9)]*","",grep("Date:",descfile,value=T),perl=T),perl=T)
    date <- Sys.Date()
    #Change the date in descfile      
    descfile[grep("Date:",descfile)]<-sub(descfile_date,date,descfile[grep("Date:",descfile)])
  }
    
  ############################################################
  # Update validation key
  ############################################################
  if(cran) {
    vkey <- paste("ValidationKey:", validationkey(version,date))
  } else {
    vkey <- "ValidationKey: 0"
  }
  if(any(grepl("ValidationKey:",descfile))) {
    descfile[grep("ValidationKey:",descfile)]<- vkey
  } else {
    descfile <- c(descfile,vkey)
  }
  
  ############################################################
  # Write the modified description files and update metadata
  ############################################################
  writeLines(descfile,"DESCRIPTION")
  package2zenodo(".")
  
  ############################################################
  # Verbosity for version information and git commands
  ############################################################
  
  if(update_type != 0){
    cat(paste0("* updating from version"), descfile_version, "to version", toString(version), "... OK\n")
    if(file.exists(".git")){
      cat("* git repository detected... OK\n")
      cat("* command suggestions for updating your git repository:\n")
      cat(rep("=", options()$width), "\n", sep="")
      cat(paste0('adding and committing: $ git add . && git commit -m "type ', update_type, ' lucode upgrade"\n'))
      cat(paste0("version tagging: $ git tag ", version, "\n"))
      cat("push commits to github: $ git push <remote> <branch>\n")
      cat("push new tag to github: $ git push --tags\n")
      cat(rep("=", options()$width), "\n", sep="")
    }
  }
  cat("done")
}
