#' Download and unpack compressed data from repositories
#' 
#' Downloads a list of tgz files from a list of repos and unpacks them
#' 
#' @param files a vector of files containing input data
#' @param repositories a list of repositories (please pay attention to the list format!) in which the files 
#' should be searched for. Files will be searched in all repositories until found, always starting with the 
#' first repository in the list. The argument must have the format of a named list with the url of the repository
#' as name and a corresponding list of options such as username or password to access the repository as value. If
#' no options are required the value has to be NULL. (e.g. 
#' list("ftp://my_pw_protected_server.de/data"=list(user="me",password=12345), "http://free_server.de/dat"=NULL))
#' @param modelfolder main model folder
#' @param additionalDelete information which additonal data should be deleted before new data are downloaded and distributed
#' @param debug switch for debug mode with additional diagnostic information
#' @return Information about the download process in form of a data.frame with data sets as row names and repositories
#' (where it was downloaded from) and corresponding md5sum as columns
#' @author Jan Philipp Dietrich, Lavinia Baumstark
#' @export

download_distribute <- function(files,
                                repositories=list("/p/projects/rd3mod/inputdata/output"=NULL),
                                modelfolder=".",
                                additionalDelete=NULL,
                                debug=FALSE) {
  
  cdir <- getwd()
  setwd(modelfolder)
  on.exit(setwd(cdir))
  
  ########## GENERATE INFORMATION FOR DESTINATION ############################
  file2destination <- getfiledestinations()
  
  ########## GENERAL CLEAN UP ################################################
  # delete old input files to avoid mixed inputs in the case that data
  # download fails at some point
  message("Delete old data in input folders ... ")
  # delete files which will be copied/moved later on with copy_input
  delete_olddata(file2destination)
  # delete additional files not treated by copy_input
  if(!is.null(additionalDelete)) delete_olddata(additionalDelete)
  message("done!\n")
  
  ########## DATA DOWNLOAD ###################################################
  # load data from source and unpack it
  filemap <- download_unpack(input=files, targetdir="input", repositories=repositories, debug=debug)
  
  ########## COPY MAGPIE INPUT FILES #########################################
  # In the following input files in MAgPIE format are converted (if required) to
  # csX files and copied into the corresponding input folders. In this step also
  # the resolution information in the file name (if existing) is removed to
  # allow a resolution-indepedent gams-sourcecode.
  low_res  <- get_info("input/info.txt","^\\* Output ?resolution:",": ")
  copy_input(x=file2destination, sourcepath="input", suffix=low_res, move=!debug)
  return(filemap)
}