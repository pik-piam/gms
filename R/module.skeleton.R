#' Create a Module skeleton
#' 
#' This function creates you a module skeleton which you can use to easily
#' create your own modules.
#' 
#' @param number Number of your module, typically something between 0-99. Sorts
#' the execution of your modules. Please use a number which is not used, yet.
#' @param name Name of your module (please choose a short name). If you want to
#' extend an existing module (add a new realisation) use the name of the
#' existing one.
#' @param types Vector of names for the different module types (e.g. "on" or
#' "off"). If you want to extend an existing module (add a new realisation),
#' put here the additional type(s)
#' @param modelpath Path of the MAgPIE version that should be updated (main
#' folder).
#' @param modulepath Module path within MAgPIE (relative to the MAgPIE main
#' folder)
#' @param includefile Name and location of the file which includes all modules
#' (relative to main folder)
#' @param version version of the modular GAMS code structure (1 or 2)
#' @note Module phases are automatically detected checking the main code of the
#' model, but not checking code in modules. If you want to use additional
#' phases which are only included within a module, you need to specify them
#' manually by adding a comment into your gams code indicating that there is an
#' additional phase. The syntax is "* !add_phase!: <phase>", e.g. "*
#' !add_phase!: new_phase"
#' @author Jan Philipp Dietrich
#' @export
#' @examples
#' # copy dummymodel to temporary directory and add new module "bla"
#' file.copy(system.file("dummymodel",package="gms"),tempdir(), recursive = TRUE)
#' model   <- paste0(tempdir(),"/dummymodel")
#' module.skeleton(number="03", name="bla", types=c("on","off"), modelpath=model)
#' 
module.skeleton <- function(number, name, types,modelpath=".", modulepath="modules/",includefile="modules/include.gms", version=is.modularGAMS(modelpath,version=TRUE)) {
  if(any(getOption("gms_reserved_types") %in% types)) stop("You tried to use reserved type name(s) (",paste(intersect(getOption("gms_reserved_types"),types),collapse=", "),") Please use different name(s)!")

  name <- paste(number,name,sep="_")
  mtypes_raw <- "\n*###################### R SECTION START (MODULETYPES) ##########################\n\n*###################### R SECTION END (MODULETYPES) ############################"
  phases_raw <- "\n*####################### R SECTION START (PHASES) ##############################\n\n*######################## R SECTION END (PHASES) ###############################" 
  #extract phases in the order as it is used in the code
  #this part of the functions assumes that batincludes are used exclusively
  #for module executions
  if(file.exists(path(modelpath,"main.gms"))) {
    code <- suppressWarnings(readLines(path(modelpath,"main.gms")))
  } else if(file.exists(path(modelpath,"magpie.gms"))) {
    code <- suppressWarnings(readLines(path(modelpath,"magpie.gms")))
  } else {
    stop("Could not find model main file. Neither main.gms nor magpie.gms do exist!")
  } 
  #connect whole code to one object by replacing $incude commands with
  #corresponding code (.csv includes and batincludes are excluded)
  repeat {
    i <- grep("$include",code,fixed=TRUE)[1]
    if(is.na(i)) break
    tmppath <- strsplit(sub("^.*\\$include","$include",code[i]),"\"")[[1]][2]
    if(length(grep("\\.(csv|inc|cs3r|cs4r|cs2|cs3|cs4)",tmppath,fixed=FALSE))==0) {
      if(file.exists(path(modelpath,tmppath))) {
        tmp <- suppressWarnings(readLines(path(modelpath,tmppath)))
        code <- c(code[1:(i-1)],tmp,code[(i+1):length(code)])
      } else {
        warning(path(modelpath,tmppath)," could not be found!")
        code[i] <- "REMOVED"
      }
    } else {
      code[i] <- "REMOVED"
    }
  }
  batincludes <- code[grep("$batinclude",code,fixed=TRUE)]
  phases <- gsub("^.*\\$batinclude .*\" ","",batincludes)
  phases <- gsub(" ","",phases)
  
  #detect additional phases
  phasecode <- "^\\* *\\!add_phase\\!\\: *"
  tmp <- grep(phasecode,code, value=TRUE) 
  add_phases <- gsub(phasecode,"", tmp)
  phases <- unique(c(phases, add_phases))

  if(nchar(name) > 30) warning("Your name is very long (more than 30 letters). Isn't it possible to choose a shorter name?")
  if(any(nchar(types) > 30)) warning("At least one type name is very long (more than 30 letters). Isn't it possible to choose a shorter type name?")
  
  module_folder <- path(modelpath,modulepath,name) 
  if(!file.exists(module_folder)) dir.create(module_folder)
  mainfile <- switch(version,
                     "1" = path(module_folder,name,ftype="gms"),
                     "2" = path(module_folder,"module",ftype="gms"))
  if(!file.exists(mainfile)) writeLines(mtypes_raw,mainfile)
  for(t in types) {
    type_folder <- path(module_folder,t)
    if(!file.exists(type_folder)){
      dir.create(type_folder)
      typefile <- switch(version,
                         "1" = path(module_folder,t,ftype="gms"),
                         "2" = path(module_folder,t,"realization",ftype="gms"))
      if(!file.exists(typefile)) writeLines(phases_raw,typefile)  
      for(phase in phases) {
        writeLines("",path(type_folder,phase,ftype="gms"))
      }
    }
  }
  update_modules_embedding(modelpath=modelpath,modulepath=modulepath,includefile=includefile)
}

