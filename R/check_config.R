#' Check config
#' 
#' Checks a model configuration file for consistency by comparing it to a
#' reference config file and the given module structure of the model. The
#' function will throw out an error if settings are missing in the config which
#' exist in the reference config, of if settings are set in the config which do
#' not exist in the reference config file or if a realization is chosen for a
#' module which does not exist, not allowed setting combinations.
#' 
#' 
#' @param icfg Input config which should be checked for consistency (either as
#' the config itself or as a file path linking to the config)
#' @param reference_file Reference config which is having the right format
#' (either as the config itself or as a file path linking to the config)
#' @param modulepath The path where the modules are stored. If set to NULL the
#' corresponding module check is deactivated.
#' @param settings_config path where the table of possible setting combinations
#' is stored, if NULL it is ignored
#' @return The checked config as a config list ready for further usage.
#' @author Jan Philipp Dietrich, Lavinia Baumstark
#' @seealso \code{\link{getModules}}
#' @export
#' @importFrom utils read.csv2
check_config <- function(icfg,reference_file="config/default.cfg",modulepath="modules/",settings_config=NULL) {
  
.source_config <- function(icfg) {
  cfg <- NULL
  # set the cfg object
  if(!is.list(icfg)) {
    if(is.character(icfg)) {
      if(file.exists(path("config",icfg))) icfg <- path("config",icfg) 
      source(icfg,local=TRUE)
      if(!is.list(cfg)) stop("Wrong input file format: config file does not contain a cfg list!")
      icfg <- cfg
      rm(cfg)
    } else {
      stop("Wrong input format: cfg is neither a list nor a character!")
    }
  }  
  return(icfg)
}


  icfg <- .source_config(icfg)
  cfg  <- .source_config(reference_file) 
  
  missing_settings <- names(cfg)[!(names(cfg) %in% names(icfg))]
  extra_settings   <- names(icfg)[!(names(icfg) %in% names(cfg))]
  
  list_elems <- names(cfg)[unlist(lapply(cfg,is.list))]
  for(l in list_elems) {
    tmp <- names(cfg[[l]])[!(names(cfg[[l]]) %in% names(icfg[[l]]))]
    if(length(tmp)>0) missing_settings <- c(missing_settings,paste(l,tmp,sep="$"))
    if(is.list(icfg[[l]])) {
      tmp <- names(icfg[[l]])[!(names(icfg[[l]]) %in% names(cfg[[l]]))]   
      if(length(tmp)>0) extra_settings <- c(extra_settings,paste(l,tmp,sep="$"))
    } else {
      warning("Setting ",l," is - contrary to the reference config file - not a list!",call.=FALSE)
    }
  }
  if(length(extra_settings)>0)   warning("Settings are unknown in provided cfg (",paste("cfg",extra_settings,sep="$",collapse=", "),")!",call.=FALSE)
  if(length(missing_settings)>0) stop("Settings are missing in provided cfg (",paste("cfg",missing_settings,sep="$",collapse=", "),")!",call.=FALSE)
  
  #check whether all modules are correctly adressed
  if(!is.null(modulepath)) {
    m <- getModules(modulepath=modulepath)
    for(i in 1:dim(m)[1]) {
      r <- icfg$gms[m[i,"name"]]
      if(is.null(r)) stop("No setting found for module \"",m[i,"name"],"\"")
      if(!(r %in% strsplit(m[i,"realizations"],",")[[1]])) stop("Chosen realization \"",r,"\" does not exist for module \"",m[i,"name"],"\"",call.=FALSE)    
    }
  }
    
  # check for inconsistent setting combinations
  if(!is.null(settings_config)){
    # Read-in table of possible settings
    possible_settings <- read.csv2(settings_config,row.names=1,check.names=FALSE)
    
    # make list of settings of icfg
    cfg_gms_list <- paste(names(icfg$gms),"|", icfg$gms,sep="")
    
    for (j in intersect(rownames(possible_settings),cfg_gms_list)){
      notallowed_settings <- colnames(possible_settings)[possible_settings[j,]==0]
      inconsistent_settings <- intersect(notallowed_settings,cfg_gms_list)
      if(length(inconsistent_settings) > 0) stop(j, " is not consistent with ",paste(inconsistent_settings,collapse=", "),call.=FALSE)
    }
    
  }
  return(icfg)
}
