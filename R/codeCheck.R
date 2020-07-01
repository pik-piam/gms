#' codeCheck
#' 
#' Checks GAMS code for consistency. Throws out warnings if something is wrong
#' in the code and returns a list containing the interfaces of each module of
#' the code.
#' 
#' 
#' @param path path of the main folder of the model
#' @param modulepath path to the module folder relative to "path"
#' @param core_files list of files that belong to the core (wildcard expansion is supported)
#' @param debug If TRUE additional information will be returned usefule for
#' debugging the codeCheck function
#' @param interactive activates an interactive developer mode in which some of
#' the warnings can be fixed interactively.
#' @param test_switches (boolean) Should realization switches in model core be tested for completness?
#' Usually set to TRUE but should be set to FALSE for standalone models only using a subset of
#' existing modules
#' @param strict (boolean) test strictness. If set to TRUE warnings from codeCheck will stop calculations
#' at the end of the analysis. Useful to enforce clean code. 
#' @param details (boolean) If activated the function will return more detailed output. Besides interface information
#' it will provide a table containing all declarations in the code, an appearance table listing the appearance of all
#' objects in the code and information about the existing modules. The format is 
#' list(interfaceInfo,declarations,appearance,modulesInfo). This setting will be ignored
#' when debug is set to TRUE.
#' @return A list of all modules containing the interfaces for each module. Or more detailed output if either
#' \code{details} or \code{debug} is set to TRUE.
#' @author Jan Philipp Dietrich
#' @export
#' @seealso \code{\link{codeExtract}},\code{\link{readDeclarations}}
#' @importFrom utils write.table read.csv
#' @examples
#' # check code consistency of dummy model
#' codeCheck(system.file("dummymodel",package="gms"))
#'

codeCheck <- function(path=".",modulepath="modules", core_files = c("core/*.gms","main.gms"), debug=FALSE, interactive=FALSE, test_switches=TRUE, strict=FALSE, details=FALSE) {

.check_input_files <- function(w,path=".", modulepath="modules") {
  inputgms <- Sys.glob(paste0(path,"/",modulepath,"/*/*/input.gms"))
  for(gms in inputgms) {
    #read $include lines
    includes <- grep("\\$include",readLines(gms,warn = FALSE),value = TRUE)
    realization <- gsub("^.*/[0-9]{2}_[^/]*/(.*)/[^\\.]*\\..*$", "\\1",gms)
    inputfolders <- gsub("^.*/[0-9]{2}_[^/]*/(.*)/[^\\.]*\\..*$", "\\1",includes)
    inputfolders <- gsub("^.*\\./(.*)/[^\\.]*\\..*$", "\\1",inputfolders)
    tmp <- gsub(paste0("^",realization,"/"),"",inputfolders)
    folderok <- grepl("^input",tmp)
    #folderok <- (inputfolders %in% c("input",paste0(realization,"/input")))
    if(any(!folderok)) {
      for(f in which(!folderok)) w <- .warning("Input file in ",sub(paste0(path,"/",modulepath),"",gms)," read from illegal location (",includes[f],"). Allowed folders are <module>/input or <module>/<realization>/input.",w=w)
    }
  }
  return(w)
}
  
  .get_line <- function(){
    # gets characters (line) from the terminal of from a connection
    # and stores it in the return object
    if(interactive()){
      s <- readline()
    } else {
      con <- file("stdin")
      s <- readLines(con, 1, warn=FALSE)
      on.exit(close(con))
    }
    return(s);
  }
  
  .choose_option <- function(options,...) {
    title <- paste0(...)
    message("\n\n",title)
    message(paste(1: length(options), options, sep=": ", collapse="\n"))
    message("\nNumber: ")
    identifier <- .get_line()
    identifier <- as.numeric(strsplit(identifier,",")[[1]])
    if (any(!(identifier %in% 1:length(options)))) stop("This choice (",identifier,") is not possible. Please type in a number between 1 and ",length(options))
    return(options[identifier])
  }
  
  message("\n Running codeCheck...")
  ptm <- proc.time()["elapsed"]
  all_files <-list.files(path=path,pattern="\\.gms$",recursive=TRUE)
  module_files <-  paste0(path,"/",grep(paste("^",modulepath,sep=""),all_files,value=TRUE))
  core_files <- Sys.glob(paste0(path,"/",core_files))
  core <- codeExtract(core_files,"core")
  modulesInfo <- getModules(paste0(path,"/",modulepath))
  modules <- list()
  for (i in 1:dim(modulesInfo)[1]) {
      m <- modulesInfo[i, ]
      for (j in strsplit(m["realizations"], ",")[[1]]) {
          tmp <- codeExtract(grep(paste0(m["folder"],"/",j,"/"), module_files,value = TRUE),name=paste(m["name"],j,sep="."))
          modules$code <- c(modules$code, tmp$code)
          modules$declarations <- rbind(modules$declarations, tmp$declarations)
          not_used <- paste0(path,"/",modulepath,"/",m["folder"],"/", j,"/not_used.txt")
          if (file.exists(not_used)) {
              tmp <- as.matrix(suppressWarnings(read.csv(not_used, as.is = TRUE, comment.char = "#")))
              dimnames(tmp)[[1]] <- rep(paste(m["name"], j,sep = "."), dim(tmp)[1])
              modules$not_used <- rbind(modules$not_used, tmp)
          }
      }
  }

  gams <- list(code=c(core$code,modules$code),declarations=rbind(core$declarations,modules$declarations),not_used = modules$not_used)

  message(" Finished data collection...            (time elapsed: ",format(proc.time()["elapsed"]-ptm,width=6,nsmall=2,digits=2),")")
  if(debug) gams_backup <- gams

  # from here on core contains the core code (without comments) and the corresponding declarations
  # and modules contains the same information for all realizations of all modules
  # now the code checking can start
  
  w <- NULL
  
  # Do all declarations follow the naming conventions (see http://redmine.pik-potsdam.de/projects/mo/wiki/Coding_Etiquette for further informations)
  # Remove objects which not follow the naming conventions from declarations set as
  # they would cause otherwise problems in the following
  tmp <- grep("^[qvsfipoxcm]{1}[cqv]?(m|[0-9]{2}|)_",gams$declarations[,"names"],invert=TRUE)
  tmp <- tmp[gams$declarations[tmp,"type"]!="set"] #remove set entries from analysis
  if(length(tmp)>0) {
    no_ <- gams$declarations[tmp,"names"]
    nodesc_ <- gams$declarations[tmp,"description"]
    gams$declarations <- gams$declarations[-tmp,]
    for(i in 1:length(no_)) w <- .warning(names(no_)[i],": \"",no_[i],"\" does not follow the given naming convetions (description = \"",nodesc_[i],"\")!",w=w)
  }
  if (!is.null(gams$not_used)) {
    tmp <- grep("_", gams$not_used[, "name"], invert = TRUE)
    if (length(tmp) > 0) {
      no_ <- gams$not_used[tmp, "name"]
      names(no_) <- dimnames(gams$not_used)[[1]][tmp]
      gams$not_used <- gams$not_used[-tmp, , drop = FALSE]
      for (i in 1:length(no_)) w <- .warning(names(no_)[i], ": \"",no_[i], "\" does not follow the given naming convetions!",w=w)
    }
  }

  message(" Naming conventions check done...       (time elapsed: ",format(proc.time()["elapsed"]-ptm,width=6,nsmall=2,digits=2),")")
  
  # Check appearance of objects
  ap <- checkAppearance(gams) 
  w <- c(w,ap$warnings)
  
  message(" Investigated variable appearances...   (time elapsed: ",format(proc.time()["elapsed"]-ptm,width=6,nsmall=2,digits=2),")")  
  
    
  #are any non-interface core variables used in other places than the core?
  wrong <- names(ap$type)[ap$type == "" & (rowSums(ap$appearance) > 1 | !ap$appearance[, "core"])]
  for (w in wrong) {
    mod <- unique(sub("\\.[^\\.]*$", "", dimnames(ap$appearance)[[2]][ap$appearance[w,]>0]))
    w <- .warning(w, " appears in \"", paste(mod, collapse = "\", \""),"\" but its name suggests that it is core only!",w=w)
  }
  
  #are any module variables used somewhere else?
  for(i in 1:dim(modulesInfo)[1]) {
    mod <- modulesInfo[i,"name"]
    number <- modulesInfo[i,"number"]
    var <- names(ap$type)[ap$type==number]
    for(v in var) {
      outside_appearance <- ap$appearance[v,grep(mod,dimnames(ap$appearance)[[2]],invert=TRUE)]
      if(any(outside_appearance>0)) w <- .warning(v, " appears outside of module \"",mod,"\"! (", paste(names(outside_appearance)[outside_appearance],collapse=", "),")",w=w)
    } 
  }
  
  #are any module numbers of modules used which do not exist?
  var <- names(ap$type)[!(sub("o","",ap$type) %in% c("","m",modulesInfo[,"number"]))]
  for(v in var) {
    w <- .warning(v, " uses the number of a non-existing module!",w=w)
  } 
  
  message(" Appearance check done...               (time elapsed: ",format(proc.time()["elapsed"]-ptm,width=6,nsmall=2,digits=2),")")  
  
  sap <- checkSwitchAppearance(gams$code)
  

  message(" Switch Appearance check done...        (time elapsed: ",format(proc.time()["elapsed"]-ptm,width=6,nsmall=2,digits=2),")")  
  
  
  #setting up a list of used interfaces for each module
  interfaceInfo <- list()
  ifs <- unique(names(ap$type)[ap$type=="m"])
  for(i in ifs) {
    mod <- unique(sub("\\.[^\\.]*$","",dimnames(ap$appearance)[[2]][ap$appearance[i,]>0]))
    if(length(mod)==1) {
      w <- .warning(i, " appears only in \"",mod,"\" even though it is supposed to be an interface (perhaps only in not_used.txt?)!",w=w)
    } else {
      where_declared <- unique(sub("\\.[^\\.]*$","",rownames(gams$declarations)[gams$declarations[,1]==i]))
      if(length(where_declared)>1) w <- .warning(i, " is declared more than once in the following parts of the code: ",paste(where_declared,collapse=", "),w=w)
      if(length(where_declared)==0) {
        w <- .warning("Could not find any declaration for ",i,w=w)
        where_declared <- "NOWHEREATALL!"
      }
      for(m in mod) {
        if(m==where_declared[1]) {
          interfaceInfo[[m]] <- c(interfaceInfo[[m]],"out"=i)
        } else {
          interfaceInfo[[m]] <- c(interfaceInfo[[m]],"in"=i)
        }
      }
    }
  }
  
  #does the core contain switches for all modules?
  if(test_switches) {
    if(!all(names(interfaceInfo) %in% c("core",sap$switches))){
      for(f in names(interfaceInfo)[!(names(interfaceInfo) %in% c("core",sap$switches))]) {
        w <- .warning("Switch for module \"",f,"\" is missing in the code!",w=w) 
      }  
    }
  }
  
  #create an object containing switches not related to modules
  modules <- c("core",getModules(paste0(path,"/",modulepath))[,"name"])
  esap <- sap
  esap$switches <- sap$switches[!(sap$switches%in%modules)]
  esap$appearance <- sap$appearance[!(rownames(sap$appearance)%in%modules),]
  esap$type <- sap$type[!(names(sap$type)%in%modules)]
  
  #check whether switches contain which are not module switches and which do not contain a prefix
  if(length(esap$type)>0) {
    for(i in 1:length(esap$type)) {
      if(esap$type[i]=="") {
        w <- .warning("\"",names(esap$type)[i],"\" is neither a module switch nor does it start with prefix \"c\"!",w=w) 
      } else if(esap$type[i]=="c") {
        if(any(esap$appearance[names(esap$type)[i],colnames(esap$appearance)!="core"])) {
          w <- .warning("\"",names(esap$type)[i],"\" should be core only!",w=w) 
        } 
      } else if(esap$type[i]=="cm") {
        #you can add here tests for interface switches!
      } else if(!suppressWarnings(is.na(as.integer(substring(esap$type[i],2))))) {
        #check whether switch is only used inside the given module
        tmp <- modulesInfo[modulesInfo[,"number"]==substring(esap$type[i],2),"name"]
        tmp2 <- esap$appearance[names(esap$type)[i],grep(paste("^",tmp,"\\.",sep=""),colnames(esap$appearance),invert=TRUE)]
        if(any(tmp2)) w <- .warning("\"",names(esap$type)[i],"\" does appear in modules ",paste(names(tmp2)[tmp2],collapse=", ")," but should only appear in module ",tmp,"!",w=w) 
      } else {
        w <- .warning("\"",names(esap$type)[i],"\" does not follow any of the given name conventions!",w=w) 
      }
    }
  }
  
  #do interfaces appear only in not_used.txt files of a module?
  for(m in names(interfaceInfo)) {
    r <- grep(paste("^",m,"(\\.|$)",sep=""),dimnames(ap$appearance)[[2]])
    for(v in interfaceInfo[[m]]) {
      if(all(ap$appearance[v,r]!=1)) {
        if(interactive) {
          tmp <- .choose_option(options=c("yes","no"),"\"",v,"\" appears in some not_used.txt files of module \"",m,"\" but is not used in the code! Should it be removed?") 
          if(tmp=="yes") {
            tmp_path <- paste(path, modulepath, paste0("[0-9]*_",m), "*/not_used.txt", sep="/") 
            not_used <- Sys.glob(tmp_path)
            for(n in not_used) {
              tmp <- read.csv(n,stringsAsFactors = FALSE, comment.char = "#") 
              tmp <- tmp[tmp$name != v,]
              write.table(tmp,n,sep=",",quote = FALSE,row.names = FALSE)
            }
            message('"',v,'" has been removed from not_used.txt files!\n')
          }
        } else {
          w <- .warning("\"",v,"\" appears in some not_used.txt files of module \"",m,"\" but is not used in the code!",w=w)
        }
      }
    }
  }
  
  #are all interfaces of a module addressed in all of its realizations?
  for(m in names(interfaceInfo)) {
    r <- grep(paste("^",m,"(\\.|$)",sep=""),dimnames(ap$appearance)[[2]])
    for(v in interfaceInfo[[m]]) {
      if(!all(ap$appearance[v,r]>0)) {
        realization <- sub("^[^\\.]*\\.","",dimnames(ap$appearance)[[2]][r])
        availability <- ap$appearance[v,r]
        if(interactive) {
          for(i in 1:length(availability)) {
            if(availability[i]==0) {
              tmp <- .choose_option(options=c("yes","no"),'"',v,'" is not addressed in realization "',realization[i],'" of module "',m,'"! Does that make sense?') 
              if(tmp=="no") stop("You need to fix the model code before we can proceed!")
              if(grepl("^v",v)) {
                  tmp <- .choose_option(options=c("no","yes"),'"',v,'" is a variable. Are you sure that it does not need to be treated in realization "',realization[i],'" (e.g. fixed to a value)?') 
                  if(tmp=="no") stop("You need to fix the model code before we can proceed!")
              }  
              tmp <- .choose_option(options=c("yes","no"),'Should "',v,'" be written to not_used.txt in realization "',realization[i],'" of module "',m,'"?')
              if(tmp=="yes") {
                tmp_path <- paste(path, modulepath, paste0("[0-9]*_",m), realization[i], sep="/")
                not_used <- paste(Sys.glob(tmp_path),"not_used.txt",sep="/")
                if(!file.exists(not_used)) w <- .warning('Do not forget to add the not_used.txt file in realization "',realization[i],'" of module "',m,'" to the repository!',w=w)
                tmp <- data.frame(name=v,type="input",reason="questionnaire")
                write.table(tmp,not_used,sep=",",quote = FALSE,row.names = FALSE, append=file.exists(not_used), col.names = !file.exists(not_used))
                message('"',v,'" has been written to not_used.txt!')
              }
            }
          }
        } else {
          w <- .warning("\"",v,"\" is not addressed in all realizations of module \"",m,"\"! (",paste(realization,availability,collapse=", ",sep="="),") (0 = missing, 1 = in code, 2 = in not_used.txt)",w=w)
        }
      }
    }
  }
  
  message(" Interface collection and check done... (time elapsed: ",format(proc.time()["elapsed"]-ptm,width=6,nsmall=2,digits=2),")")
  

  w <- .check_input_files(w=w,path=path,modulepath=modulepath)
  message(" Input folder check done...             (time elapsed: ",format(proc.time()["elapsed"]-ptm,width=6,nsmall=2,digits=2),")")
  
  
  # Do all declarations come with a description?
  w <- checkDescription(gams,w)
  
  message(" Description check done...              (time elapsed: ",format(proc.time()["elapsed"]-ptm,width=6,nsmall=2,digits=2),")")
  
  if(debug) {
    out <- list(interfaceInfo=interfaceInfo,ap=ap,gams=gams,gams_backup=gams_backup,sap=sap,esap=esap,modulesInfo=modulesInfo)
  } else if(details) {
    d <- gams$declarations
    d <- cbind(d,origin=rownames(d))
    rownames(d) <- NULL
    d <- as.data.frame(d, stringsAsFactors = FALSE)
    mi <- as.data.frame(modulesInfo,stringsAsFactors = FALSE)
    out <- list(interfaceInfo=interfaceInfo,declarations=d,appearance=ap$appearance,setappearance=ap$setappearance,modulesInfo=mi)
  } else {
    out <- interfaceInfo
  }

  if(is.null(w)) {
    message(" All codeCheck tests passed!")
  } else {
    if(strict) stop("codeCheck returned warnings. Fix warnings to proceed!")
    message(" codeCheck reported code inconsistencies. Please fix the given warnings!")
  }
  attr(out,"last.warning") <- w
  return(out)
}
