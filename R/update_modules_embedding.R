#' Update Modules Embedding in GAMS code
#'
#' A function that updates in the GAMS code all include commands which are
#' related to Modules. The function automatically checks which modules exist
#' and which files in these modules exist and creates the corresponding include
#' commands in GAMS
#'
#' @param modelpath Path to the model that should be updated (main
#' folder).
#' @param modulepath Module path within the model (relative to the model main
#' folder)
#' @param includefile Name and location of the file which includes all modules
#' (relative to main folder)
#' @param verbose Defines whether additional information should be printed or
#' not.
#' @note Module phases are automatically detected checking the main code of the
#' model, but not checking code in modules. If you want to use additional
#' phases which are only included within a module, you need to specify them
#' manually by adding a comment into your gams code indicating that there is an
#' additional phase. The syntax is "* !add_phase!: <phase>", e.g. "*
#' !add_phase!: new_phase"
#' @author Jan Philipp Dietrich
#' @export
#' @importFrom utils data
#' @examples
#' # copy dummymodel to temporary directory and update module embedding
#' file.copy(system.file("dummymodel", package="gms"), tempdir(), recursive = TRUE)
#' model   <- paste0(tempdir(),"/dummymodel")
#' update_modules_embedding(model)
#'
update_modules_embedding <- function(modelpath = ".", modulepath = "modules/",
                                     includefile = "modules/include.gms", verbose = FALSE) {

  version <- is.modularGAMS(path = modelpath, modulepath = modulepath, version = TRUE)
  if (version == 0) stop("Model does not follow modular structure.")

  #set LC_ALL to C to avoid locale warnings
  Sys.setlocale("LC_ALL", "C")
  #extract phases in the order as it is used in the code
  #this part of the functions assumes that batincludes are used exclusively
  #for module executions
  if (file.exists(path(modelpath, "main.gms"))) {
    code <- suppressWarnings(readLines(path(modelpath, "main.gms")))
  } else if (file.exists(path(modelpath, "magpie.gms"))) {
    code <- suppressWarnings(readLines(path(modelpath, "magpie.gms")))
  } else {
    stop("Could not find model main file. Neither main.gms nor magpie.gms do exist!")
  }

  code <- c("", code, "") # includes in first or last line let repeat statement run infinitely
  #connect whole code to one object by replacing $include commands with
  #corresponding code (.csv, .cs2 includes and batincludes are excluded)
  repeat {
    i <- grep("^\\$include", code)[1]
    if (is.na(i)) break
    tmppath <- strsplit(code[i], "\"")[[1]][2]
    if (length(grep("\\.cs.{1,2}", tmppath)) == 0 && file.exists(path(modelpath, tmppath))) {
        tmp <- suppressWarnings(readLines(path(modelpath, tmppath)))
        code <- c(code[1:(i - 1)], tmp, code[(i + 1):length(code)])
    } else {
      if (length(grep("\\.cs.{1,2}", tmppath)) == 0 && ! file.exists(path(modelpath, tmppath))) {
        warning("Could not open include ", path(modelpath, tmppath))
      }
      code[i] <- "REMOVED"
    }
  }
  batincludes <- code[grep("$batinclude", code, fixed = TRUE)]
  phases <- gsub("^.*\\$batinclude .*\" ", "", batincludes)
  phases <- gsub(" ", "", phases)

  #detect additional phases
  phasecode <- "^\\* *\\!add_phase\\!\\: *"
  tmp <- grep(phasecode, code, value = TRUE)
  add_phases <- gsub(phasecode, "", tmp)
  phases <- unique(c(phases, add_phases))

  fullmodulepath <- path(modelpath, modulepath)
  modules        <- getModules(fullmodulepath)[, "folder"]
  moduleGMSpathsAll <- switch(version,
                         "1" = path(modelpath, modulepath, modules, modules, ftype = "gms"),
                         "2" = path(modelpath, modulepath, modules, "module", ftype = "gms"))
  # excludes modules that have no gms file
  if (! all(file.exists(moduleGMSpathsAll))) {
    warning("Missing module gms files: ",
      paste(moduleGMSpathsAll[! file.exists(moduleGMSpathsAll)], collapse = ", "))
  }
  modules        <- modules[file.exists(moduleGMSpathsAll)]
  moduleGMSpaths <- moduleGMSpathsAll[file.exists(moduleGMSpathsAll)]
  moduleNames    <- getModules(fullmodulepath)[file.exists(moduleGMSpathsAll), "name"]
  code <- paste("$include \"", moduleGMSpaths, "\"", sep = "")
  replace_in_file(path(modelpath, includefile), code, subject = "MODULES")

  for (m in seq_along(modules)) {
    module <- modules[m]

    #set links to module types
    types <- base::list.dirs(path = path(fullmodulepath, module), full.names = FALSE, recursive = FALSE)
    #remove reserved module type names
    types <- setdiff(types, getOption("gms_reserved_types"))
    realizationGMSpaths <- switch(version,
                           "1" = path(modelpath, modulepath, module, types, ftype = "gms"),
                           "2" = path(modelpath, modulepath, module, types, "realization", ftype = "gms"))
    emptyrealization <- ! file.exists(realizationGMSpaths)
    if (any(emptyrealization)) {
      warning("For module ", module, ", the following realizations lack their main gms file and cannot be used: ",
              paste(types[emptyrealization], collapse = ", "))
    }
    types <- types[! emptyrealization]
    realizationGMSpaths <- realizationGMSpaths[! emptyrealization]
    code <- paste0("$Ifi \"%", substring(module, 4), "%\" == \"", types, "\" $include \"", realizationGMSpaths, "\"")
    replace_in_file(moduleGMSpaths[m], code, subject = "MODULETYPES")

    #set links to different module phases
    for (ti in seq_along(types)) {
      t <- types[ti]
      code <- NULL
      for (phase in phases) {
        if (file.exists(path(modelpath, modulepath, module, t, phase, ftype = "gms"))) {
          if (verbose) message(module, " ", t, ": ", phase, " is used")
          code <- c(code, paste("$Ifi \"%phase%\" == \"", phase, "\" $include \"",
                    path(modelpath, modulepath, module, t, phase, ftype = "gms"), "\"", sep = ""))
        } else if (verbose) message(module, " ", t, ": ", phase, "is not used")
      }
      replace_in_file(realizationGMSpaths[ti], code, subject = "PHASES")
    }
  }

  ############# ADD MODULE INFO IN SETS  ###################### START ##########################################

  if (length(grep("module2realisation", readLines(path(modelpath, "core", "sets.gms")))) > 0) {
    content <- NULL
    modification_warning <- c(
      '*** THIS CODE IS CREATED AUTOMATICALLY, DO NOT MODIFY THESE LINES DIRECTLY',
      '*** ANY DIRECT MODIFICATION WILL BE LOST AFTER NEXT MODEL START')
    content <- c(modification_warning, '', 'sets')
    content <- c(content, '', '       modules "all the available modules"')
    content <- c(content, '       /', paste0("       ", moduleNames), '       /')
#    content <- c(content, '', '       realisations "all the active realisations"')
#    content <- c(content, '       /', paste0("       ",unlist(unique(cfg$gms[getModules("modules/")[,"name"]]))), "       /")
    content <- c(content, '', '      module2realisation(modules,*) "mapping of modules and active realisations" /')
    content <- c(content, paste0("       ", moduleNames, " . %", moduleNames, "%"))
    content <- c(content, '      /', ';')
    replace_in_file(path(modelpath, "core", "sets.gms"), content, "MODULES", comment = "***")


  ############# ADD MODULE INFO IN SETS  ###################### END ############################################
  }
}
