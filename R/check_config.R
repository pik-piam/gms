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
#' @param extras vector of setting names that are allowed to appear in the input config
#' even if they are missing in the reference config. That can be useful to allow for
#' additional settings/information which does not necessarily have to exist
#' @param saveCheck additional check which makes sure that saving and loading it to/from
#' YAML format does not change its contents
#' @return The checked config as a config list ready for further usage.
#' @author Jan Philipp Dietrich, Lavinia Baumstark
#' @seealso \code{\link{getModules}}
#' @export
#' @importFrom utils read.csv2
check_config <- function(icfg, reference_file = "config/default.cfg", modulepath = "modules/", # nolint
                         settings_config = NULL, extras = NULL, saveCheck = FALSE) {           # nolint

  .sourceConfig <- function(icfg) {
    cfg <- NULL
    # set the cfg object
    if (!is.list(icfg)) {
      if (is.character(icfg)) {
        if (file.exists(file.path("config", icfg))) icfg <- file.path("config", icfg)
        source(icfg, local = TRUE) # nolint
        if (!is.list(cfg)) stop("Wrong input file format: config file does not contain a cfg list!")
        raw <- paste(readLines(icfg), collapse = " ")
        attr(cfg, "items") <- substring(unique(str_extract_all(raw, "cfg\\$[a-zA-Z0-9_]*")[[1]]), 5)
        icfg <- cfg
        rm(cfg)
      } else {
        stop("Wrong input format: cfg is neither a list nor a character!")
      }
    }
    return(icfg)
  }

  .extendedNames <- function(cfg) {
    return(union(names(cfg), attr(cfg, "items")))
  }

  icfg <- .sourceConfig(icfg)
  cfg  <- .sourceConfig(reference_file)

  if (saveCheck) {
    rmItems <- function(x) {
      attr(x, "items") <- NULL
      return(x)
    }
    if (!identical(rmItems(icfg), loadConfig(saveConfig(icfg)))) {
      warning("Config looks different when stored via saveConfig and loaded via loadConfig!")
    }
  }

  missingSettings <- setdiff(names(cfg), names(icfg))
  extraSettings   <- setdiff(names(icfg), .extendedNames(cfg))

  listElems <- names(cfg)[unlist(lapply(cfg, is.list))]
  for (l in listElems) {
    tmp <- names(cfg[[l]])[!(names(cfg[[l]]) %in% names(icfg[[l]]))]
    if (length(tmp) > 0) missingSettings <- c(missingSettings, paste(l, tmp, sep = "$"))
    if (is.list(icfg[[l]])) {
      tmp <- names(icfg[[l]])[!(names(icfg[[l]]) %in% names(cfg[[l]]))]
      if (length(tmp) > 0) extraSettings <- c(extraSettings, paste(l, tmp, sep = "$"))
    } else {
      warning("Setting ", l, " is - contrary to the reference config file - not a list!", call. = FALSE)
    }
  }
  if (!is.null(extras)) {
    extraSettings <- extraSettings[!(sub("\\$.*$", "", extraSettings) %in% extras) & ! extraSettings %in% extras]
  }
  if (length(extraSettings) > 0) {
    warning("Settings are unknown in provided cfg (", paste("cfg", extraSettings, sep = "$", collapse = ", "),
            ")!", call. = FALSE)
  }
  if (length(missingSettings) > 0) {
    stop("Settings are missing in provided cfg (", paste("cfg", missingSettings, sep = "$", collapse = ", "),
         ")!", call. = FALSE)
  }

  # check whether all modules are correctly adressed
  if (!is.null(modulepath)) {
    m <- getModules(modulepath = modulepath)
    for (i in 1:dim(m)[1]) {
      r <- icfg$gms[m[i, "name"]]
      if (is.null(r)) stop("No setting found for module \"", m[i, "name"], "\"")
      if (!(r %in% strsplit(m[i, "realizations"], ",")[[1]])) {
        stop("Chosen realization \"", r, "\" does not exist for module \"", m[i, "name"], "\"", call. = FALSE)
      }
    }
  }

  # check for inconsistent setting combinations
  if (!is.null(settings_config)) {
    # Read-in table of possible settings
    possibleSettings <- read.csv2(settings_config, row.names = 1, check.names = FALSE)

    # make list of settings of icfg
    cfgGMSList <- paste(names(icfg$gms), "|", icfg$gms, sep = "")

    for (j in intersect(rownames(possibleSettings), cfgGMSList)) {
      notallowedSettings <- colnames(possibleSettings)[possibleSettings[j, ] == 0]
      inconsistentSettings <- intersect(notallowedSettings, cfgGMSList)
      if (length(inconsistentSettings) > 0) {
        stop(j, " is not consistent with ", paste(inconsistentSettings, collapse = ", "), call. = FALSE)
      }
    }

  }
  return(icfg)
}
