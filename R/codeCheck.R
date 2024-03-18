#' codeCheck
#'
#' Checks GAMS code for consistency. Throws out warnings if something is wrong
#' in the code and returns a list containing the interfaces of each module of
#' the code.
#'
#' Additional settings can be provided via a yaml file ".codeCheck" in the main
#' folder of the model. Currently supported settings are:
#' - capitalExclusionList: a list of names that should be ignored when checking
#' for unified capitalization of variables
#'
#' @param path path of the main folder of the model
#' @param modulepath path to the module folder relative to "path"
#' @param core_files list of files that belong to the core (wildcard expansion is supported)
#' @param returnDebug If TRUE additional information will be returned useful for
#' debugging the codeCheck function
#' @param interactive activates an interactive developer mode in which some of
#' the warnings can be fixed interactively.
#' @param test_switches (boolean) Should realization switches in model core be tested for completeness?
#' Usually set to TRUE but should be set to FALSE for standalone models only using a subset of
#' existing modules
#' @param strict (boolean) test strictness. If set to TRUE warnings from codeCheck will stop calculations
#' at the end of the analysis. Useful to enforce clean code.
#' @param details (boolean) If activated the function will return more detailed output. Besides interface information
#' it will provide a table containing all declarations in the code, an appearance table listing the appearance of all
#' objects in the code and information about the existing modules. The format is
#' list(interfaceInfo,declarations,appearance,modulesInfo). This setting will be ignored
#' when returnDebug is set to TRUE.
#' @return A list of all modules containing the interfaces for each module. Or more detailed output if either
#' \code{details} or \code{returnDebug} is set to TRUE.
#' @author Jan Philipp Dietrich
#' @export
#' @seealso \code{\link{codeExtract}},\code{\link{readDeclarations}}
#' @importFrom utils write.table read.csv
#' @importFrom stats setNames
#' @importFrom stringr str_pad
#' @examples
#' # check code consistency of dummy model
#' codeCheck(system.file("dummymodel", package = "gms"))
#'
codeCheck <- function(path = ".",
                      modulepath = "modules",
                      core_files = c("core/*.gms", "main.gms"),  # nolint: object_name_linter
                      returnDebug = FALSE,
                      interactive = FALSE,
                      test_switches = TRUE,  # nolint: object_name_linter
                      strict = FALSE,
                      details = FALSE) {

  .checkInputFiles <- function(w, path = ".", modulepath = "modules") {
    inputgms <- Sys.glob(paste0(path, "/", modulepath, "/*/*/input.gms"))
    for (gms in inputgms) {
      # read $include lines
      includes <- grep("\\$include", readLines(gms, warn = FALSE), value = TRUE)
      realization <- gsub("^.*/[0-9]{2}_[^/]*/(.*)/[^\\.]*\\..*$", "\\1", gms)
      inputfolders <- gsub("^.*/[0-9]{2}_[^/]*/(.*)/[^\\.]*\\..*$", "\\1", includes)
      inputfolders <- gsub("^.*\\./(.*)/[^\\.]*\\..*$", "\\1", inputfolders)
      tmp <- gsub(paste0("^", realization, "/"), "", inputfolders)
      folderok <- grepl("^input", tmp)
      if (any(!folderok)) {
        for (f in which(!folderok)) {
           w <- .warning("Input file in ",
                         sub(paste0(path, "/", modulepath), "", gms),
                         " read from illegal location (",
                         includes[f],
                         "). Allowed folders are <module>/input or <module>/<realization>/input.",
                         w = w)
        }
      }
    }
    return(w)
  }

  .collectData <- function(path, modulepath, coreFiles, modulesInfo) {
    # returns a named list containing the code (without comments) and the corresponding declarations,
    # each divided into a core part and a part with the code/declarations of all modules.
    # Additionally, the named list also contains the not_used information from the modules.
    path <- normalizePath(path, winslash = "/")
    foldersToSearch <- list.dirs(path = path, recursive = FALSE)
    foldersToSearch <- foldersToSearch[!basename(foldersToSearch) %in% c("renv", ".git", "output")]
    allFiles <- list.files(path = foldersToSearch, pattern = "\\.gms$", full.names = TRUE, recursive = TRUE)
    allFiles <- c(allFiles, list.files(path = path, pattern = "\\.gms$", full.names = TRUE, recursive = FALSE))
    allFiles <- sub(paste0(path, "/"), "", normalizePath(allFiles, winslash = "/", mustWork = FALSE), fixed = TRUE)

    moduleFiles <-  paste0(path, "/", grep(paste("^", modulepath, sep = ""), allFiles, value = TRUE))
    coreFiles <- Sys.glob(paste0(path, "/", coreFiles))
    core <- codeExtract(coreFiles, "core")
    modules <- list()
    for (i in seq_len(dim(modulesInfo)[1])) {
        m <- modulesInfo[i, ]
        for (j in strsplit(m["realizations"], ",")[[1]]) {
            tmp <- codeExtract(grep(paste0(m["folder"], "/", j, "/"), moduleFiles, value = TRUE),
                               name = paste(m["name"], j, sep = "."))
            modules$code <- c(modules$code, tmp$code)
            modules$declarations <- rbind(modules$declarations, tmp$declarations)
            notUsedPath <- paste0(path, "/", modulepath, "/", m["folder"], "/", j, "/not_used.txt")
            if (file.exists(notUsedPath)) {
                tmp <- as.matrix(suppressWarnings(read.csv(notUsedPath, as.is = TRUE, comment.char = "#")))
                dimnames(tmp)[[1]] <- rep(paste(m["name"], j, sep = "."), dim(tmp)[1])
                modules$not_used <- rbind(modules$not_used, tmp)
            }
        }
    }

    gams <- list(code = c(core$code, modules$code),
                 declarations = rbind(core$declarations, modules$declarations),
                 not_used = modules$not_used)
    return(gams)
  }

  .checkNamingConventions <- function(gams, w) {
    # Do all declarations follow the naming conventions? (see the coding etiquette at
    # https://github.com/magpiemodel/tutorials/blob/master/4_GAMScodeStructure.md#23-coding-etiquette-variable-and-parameter-naming # nolint
    # for further information)
    # Remove objects which do not follow the naming conventions from the declarations set as
    # they would otherwise cause problems in what follows
    tmp <- grep("^[qvsfipoxcm]{1}[cqv]?(m|[0-9]{2}|)_", gams$declarations[, "names"], invert = TRUE)
    tmp <- tmp[gams$declarations[tmp, "type"] != "set"] # remove set entries from analysis
    if (length(tmp) > 0) {
      declarationNames <- gams$declarations[tmp, "names"]
      declarationDescription <- gams$declarations[tmp, "description"]
      gams$declarations <- gams$declarations[-tmp, ]
      for (i in seq_along(declarationNames)) {
        w <- .warning(names(declarationNames)[i],
                      ": \"",
                      declarationNames[i],
                      "\" does not follow the given naming conventions (description = \"",
                      declarationDescription[i],
                      "\")!",
                      w = w)
        }
      }
    if (!is.null(gams$not_used)) {
      tmp <- grep("_", gams$not_used[, "name"], invert = TRUE)
      if (length(tmp) > 0) {
        declarationNames <- gams$not_used[tmp, "name"]
        names(declarationNames) <- dimnames(gams$not_used)[[1]][tmp]
        gams$not_used <- gams$not_used[-tmp, , drop = FALSE]
        for (i in seq_along(declarationNames)) {
          w <- .warning(names(declarationNames)[i],
                        ": \"",
                        declarationNames[i],
                        "\" does not follow the given naming conventions!",
                        w = w)
        }
      }
    }
    return(list(gams = gams, w = w))
  }

  .getInterfaceInfo <- function(ap, gams, w) {
    # setting up a list of used interfaces for each module
    interfaceInfo <- list()
    ifs <- unique(names(ap$type)[ap$type == "m"])
    for (i in ifs) {
      mod <- unique(sub("\\.[^\\.]*$", "", dimnames(ap$appearance)[[2]][ap$appearance[i, ] > 0]))
      if (length(mod) == 1) {
        w <- .warning(i,
                      " appears only in \"",
                      mod,
                      "\" even though it is supposed to be an interface!",
                      w = w)
      } else {
        whereDeclared <- unique(sub("\\.[^\\.]*$", "", rownames(gams$declarations)[gams$declarations[, 1] == i]))
        if (length(whereDeclared) > 1) {
          w <- .warning(i,
                        " is declared more than once in the following parts of the code: ",
                        paste(whereDeclared, collapse = ", "),
                        w = w)
        }
        if (length(whereDeclared) == 0) {
          w <- .warning("Could not find any declaration for ", i, w = w)
          whereDeclared <- "NOWHEREATALL!"
        }
        for (m in mod) {
          if (m == whereDeclared[1]) {
            interfaceInfo[[m]] <- c(interfaceInfo[[m]], "out" = i)
          } else {
            interfaceInfo[[m]] <- c(interfaceInfo[[m]], "in" = i)
          }
        }
      }
    }
    return(list(interfaceInfo = interfaceInfo, w = w))
  }

  .emitTimingMessage <- function(message, ptm) {
    # Print a timing message, nicely formatted
    message(str_pad(message, 40, "right"),
            "(time elapsed: ",
            format(proc.time()["elapsed"] - ptm, width = 6, nsmall = 2, digits = 2),
            ")")
  }

  message("\n Running codeCheck...")

  ptm <- proc.time()["elapsed"]

  modulesInfo <- getModules(paste0(path, "/", modulepath))

  # warnings
  w <- NULL

  gams <- .collectData(path = path, modulepath = modulepath, coreFiles = core_files, modulesInfo = modulesInfo)

  # mark instances only showing up in not_used, but are never declared
  notDeclared <- ! gams$not_used[, "name"] %in% gams$declarations[, "names"]
  interfacesOnlyNotused <- list()
  if (length(notDeclared) > 0 && any(notDeclared)) {
    interfacesOnlyNotused <- as.list(setNames(gams$not_used[, "name"],
                                     gsub("\\..*$", "", rownames(gams$not_used)))[notDeclared])
    gams$not_used <- gams$not_used[! notDeclared, ]
  }

  if (returnDebug) {
    gamsBackup <- gams
  }

  .emitTimingMessage(" Finished data collection...", ptm)

  ret <- .checkNamingConventions(gams = gams, w = w)
  gams <- ret$gams
  w <- ret$w

  .emitTimingMessage(" Naming conventions check done...", ptm)

  # Check appearance of objects

  capitalExclusionList <- NULL

  if (file.exists(file.path(path, ".codeCheck"))) {
    # read in exclusions for capitalization check from .codeCheck
    capitalExclusionList <- read_yaml(file.path(path, ".codeCheck"))[["capitalExclusionList"]]
  }

  ap <- checkAppearance(gams, capitalExclusionList = capitalExclusionList)
  w <- c(w, ap$warnings)

  .emitTimingMessage(" Investigated variable appearances...", ptm)

  w <- .checkAppearanceUsage(ap = ap, modulesInfo = modulesInfo, w = w)

  .emitTimingMessage(" Appearance and usage check done...", ptm)

  sap <- checkSwitchAppearance(gams$code)

  .emitTimingMessage(" Switch Appearance check done...", ptm)

  ret <- .getInterfaceInfo(ap = ap, gams = gams, w = w)
  interfaceInfo <- ret$interfaceInfo
  w <- ret$w

  # does the core contain switches for all modules?
  if (test_switches) {
    if (!all(names(interfaceInfo) %in% c("core", sap$switches))) {
      for (f in names(interfaceInfo)[!(names(interfaceInfo) %in% c("core", sap$switches))]) {
        w <- .warning("Switch for module \"", f, "\" is missing in the code!", w = w)
      }
    }
  }

  # create an object containing switches not related to modules
  modules <- c("core", modulesInfo[, "name"])
  esap <- sap
  esap$switches <- sap$switches[!(sap$switches %in% modules)]
  esap$appearance <- sap$appearance[!(rownames(sap$appearance) %in% modules), ]
  esap$type <- sap$type[!(names(sap$type) %in% modules)]

  # check whether switches contain which are not module switches and which do not contain a prefix
  if (length(esap$type) > 0) {
    for (i in seq_along(esap$type)) {
      if (esap$type[i] == "") {
        w <- .warning("\"",
                      names(esap$type)[i],
                      "\" is neither a module switch nor does it start with prefix \"c\"!",
                      w = w)
      } else if (esap$type[i] == "c") {
        if (any(esap$appearance[names(esap$type)[i], colnames(esap$appearance) != "core"])) {
          w <- .warning("\"", names(esap$type)[i], "\" should be core only!", w = w)
        }
      } else if (esap$type[i] == "cm") {
        # you can add here tests for interface switches!
      } else if (!suppressWarnings(is.na(as.integer(substring(esap$type[i], 2))))) {
        # check whether switch is only used inside the given module
        tmp <- modulesInfo[modulesInfo[, "number"] == substring(esap$type[i], 2), "name"]
        tmp2 <- esap$appearance[names(esap$type)[i],
                                grep(paste("^", tmp, "\\.", sep = ""), colnames(esap$appearance), invert = TRUE)]
        if (any(tmp2)) {
          w <- .warning("\"",
                        names(esap$type)[i],
                        "\" does appear in modules ",
                        paste(names(tmp2)[tmp2], collapse = ", "),
                        " but should only appear in module ",
                        tmp,
                        "!",
                        w = w)
        }
      } else {
        w <- .warning("\"", names(esap$type)[i], "\" does not follow any of the given name conventions!", w = w)
      }
    }
  }

  # do interfaces appear only in not_used.txt files of a module?
  for (m in names(interfaceInfo)) {
    r <- grep(paste("^", m, "(\\.|$)", sep = ""), dimnames(ap$appearance)[[2]])
    for (v in interfaceInfo[[m]]) {
      if (all(ap$appearance[v, r] != 1)) {
        interfacesOnlyNotused <- append(interfacesOnlyNotused, setNames(v, m))
      }
    }
  }

  if (length(interfacesOnlyNotused) > 0) {
    if (! isTRUE(interactive)) {
      w <- .warning(paste(unique(interfacesOnlyNotused), collapse = ", "),
                    " was never declared, but exists only in not_used.txt of ",
                    paste(unique(names(interfacesOnlyNotused)), collapse = ", "),
                    w = w)
    } else {
      .emitTimingMessage(" Cleanup not_used.txt...", ptm)
      for (i in seq_along(interfacesOnlyNotused)) {
        m <- names(interfacesOnlyNotused[i])
        r <- grep(paste("^", m, "(\\.|$)", sep = ""), dimnames(ap$appearance)[[2]])
        v <- interfacesOnlyNotused[[i]]
        notUsedGlob <- paste(path, modulepath, paste0("[0-9]*_", m), "*/not_used.txt", sep = "/")
        notUsedPath <- Sys.glob(notUsedGlob)
        for (n in notUsedPath) {
          comment <- grep("^#", readLines(n), value = TRUE)
          tmp <- read.csv(n, stringsAsFactors = FALSE, comment.char = "#")
          tmp <- tmp[tmp$name != v, ]
          writeLines(c(comment, paste(colnames(tmp), collapse = ",")), n)
          write.table(tmp, n, sep = ",", quote = FALSE, row.names = FALSE, append = TRUE, col.names = FALSE)
        }
        if (v %in% rownames(ap$appearance)) ap$appearance[v, r] <- 0
        message("'", v, "' has been removed as interface and has been deleted from all ",
                length(notUsedPath), " not_used.txt files of module '", m, "'!\n")
      }

      # changes in not_used.txt require repeating the interface check
      ret <- .getInterfaceInfo(ap = ap, gams = gams, w = w)
      w <- ret$w
    }
  }

  # are all interfaces of a module addressed in all of its realizations?
  for (m in names(interfaceInfo)) {
    r <- grep(paste("^", m, "(\\.|$)", sep = ""), dimnames(ap$appearance)[[2]])
    for (v in interfaceInfo[[m]]) {
      if (!all(ap$appearance[v, r] > 0) && !all(ap$appearance[v, r] == 0)) {
        realization <- sub("^[^\\.]*\\.", "", dimnames(ap$appearance)[[2]][r])
        availability <- ap$appearance[v, r]
        if (interactive) {
          notaddressed <- realization[availability == 0]
          userinfo <- paste0("In module '", m, "', '", v, "' is not addressed in those ", length(notaddressed), " realizations: ", paste(notaddressed, collapse = ", "),
                             ".", if (grepl("^v", v)) "\nIt is an interface to other modules and might need to be take care of in these realizations (e.g. fixed to a value).",
                             "\nIf you are sure that it does not need to be addressed at all you can add this to 'not_used.txt'.",
                             "\nType the reason why it does not need to be addressed, or type 'n' if it actually needs to be addressed in the code.")
          message(userinfo)
          answer <- getLine()
        } else {
          answer <- FALSE
        }
        if (! answer %in% c(FALSE, "n", "N")) {
          for (n in notaddressed) {
            notUsedGlob <- paste(path, modulepath, paste0("[0-9]*_", m), n, sep = "/")
            notUsedPath <- paste(Sys.glob(notUsedGlob), "not_used.txt", sep = "/")
            tmp <- data.frame(name = v, type = "input", reason = if (answer == "") "added by codeCheck" else answer)
            write.table(tmp,
                        notUsedPath,
                        sep = ",",
                        quote = FALSE,
                        row.names = FALSE,
                        append = file.exists(notUsedPath),
                        col.names = ! file.exists(notUsedPath))
          }
        } else {
          w <- .warning("'", v, "' is not addressed in all realizations of module '", m,
                        "'! (", paste(realization, availability, collapse = ", ", sep = "="),
                        ") (0 = missing, 1 = in code, 2 = in not_used.txt)", w = w)
        }
      }
    }
  }

  .emitTimingMessage(" Interface collection and check done...", ptm)

  w <- .checkInputFiles(w = w, path = path, modulepath = modulepath)

  .emitTimingMessage(" Input folder check done...", ptm)

  # Do all declarations come with a description?
  w <- checkDescription(gams, w)

  .emitTimingMessage(" Description check done...", ptm)

  if (returnDebug) {
    out <- list(interfaceInfo = interfaceInfo,
                ap = ap,
                gams = gams,
                gams_backup = gamsBackup,
                sap = sap,
                esap = esap,
                modulesInfo = modulesInfo)
  } else if (details) {
    d <- gams$declarations
    d <- cbind(d, origin = rownames(d))
    rownames(d) <- NULL
    d <- as.data.frame(d, stringsAsFactors = FALSE)
    mi <- as.data.frame(modulesInfo, stringsAsFactors = FALSE)
    out <- list(interfaceInfo = interfaceInfo,
                declarations = d,
                appearance = ap$appearance,
                setappearance = ap$setappearance,
                modulesInfo = mi)
  } else {
    out <- interfaceInfo
  }

  if (is.null(w)) {
    message(" All codeCheck tests passed!")
  } else {
    if (strict) stop("codeCheck returned warnings. Fix warnings to proceed!")
    message(" codeCheck reported code inconsistencies. Please fix the given warnings!")
  }
  attr(out, "last.warning") <- w  # nolint: object_name_linter
  return(out)
}


.checkAppearanceUsage <- function(ap, modulesInfo, w) {
  # are any non-interface core variables used in other places than the core?
  wrong <- names(ap$type)[ap$type == "" & (rowSums(ap$appearance) > 1 | !ap$appearance[, "core"])]
  for (wrong_item in wrong) {
    mod <- unique(sub("\\.[^\\.]*$", "", dimnames(ap$appearance)[[2]][ap$appearance[wrong_item, ] > 0]))
    w <- .warning(wrong_item,
                  " appears in \"",
                  paste(mod, collapse = "\", \""),
                  "\" but its name suggests that it is core only!",
                  w = w)
  }

  # are any module variables used somewhere else?
  for (i in seq_len(dim(modulesInfo)[1])) {
    mod <- modulesInfo[i, "name"]
    number <- modulesInfo[i, "number"]
    var <- names(ap$type)[ap$type == number]
    for (v in var) {
      outsideAppearance <- ap$appearance[v, grep(mod, dimnames(ap$appearance)[[2]], invert = TRUE)]
      if (any(outsideAppearance > 0)) {
        w <- .warning(v,
                      " appears outside of module \"",
                      mod,
                      "\"! (",
                      paste(names(outsideAppearance)[outsideAppearance], collapse = ", "),
                      ")",
                      w = w)
      }
    }
  }

  # are any module numbers of modules used which do not exist?
  var <- names(ap$type)[!(sub("o", "", ap$type) %in% c("", "m", modulesInfo[, "number"]))]
  for (v in var) {
    w <- .warning(v, " uses the number of a non-existing module!", w = w)
  }

  return(w)
}
