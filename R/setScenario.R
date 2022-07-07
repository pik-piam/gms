#' setScenario
#' 
#' setScenario is adapting a given config to a predefined scenario, meaning
#' that all settings which are fixed for the given scenario are written to the
#' config. Settings not defined by the scenario remain unchanged.
#' 
#' 
#' @param cfg Input config which should be adapted to the given scenario
#' @param scenario name of scenario (e.g. "SSP2"). Can also be a vector of
#' scenarios. In this case scenario settings are applied in the given order
#' @param scenario_config The path where the scenario config table is stored.
#' @return The updated config as a config list ready for further usage.
#' @note The scenario config table is a table which contains as columns the
#' different scenarios and as rows the different settings. Empty entries for a
#' given scenario-setting combination indicate that this setting is not defined
#' by the scenario and should not be changed by set Scenario!
#' @author Jan Philipp Dietrich, Anastasis Giannousakis
#' @export
#' @importFrom utils read.csv
#' @seealso \code{\link{check_config}},\code{\link{getModules}}
setScenario <- function(cfg,scenario,scenario_config="config/scenario_config.csv"){
  
  # setwd("~/0_SVN/MAgPIE_SSP")
  # source("config/default_test.cfg")
  # source("config/default.cfg")
  # scenario <- "n600"
  # x <- scenario
  # scenario_config <-"config/scenario_config_test.csv"
  # scenario_config <-"config/scenario_config.csv"
  
    if (!is.list(cfg)) {
      if (is.character(cfg)) {
        if (file.exists(path("config", cfg))) 
          cfg <- path("config", cfg)
        source(cfg, local = TRUE)
        if (!is.list(cfg)) 
          stop("Wrong input file format: config file does not contain a cfg list!")
      }
      else {
        stop("Wrong input format: cfg is neither a list nor a character!")
      }
    }
    tmp <- try(read.csv(scenario_config, as.is = TRUE, check.names = FALSE, colClasses = "character"), silent = TRUE)
    #check whether reading the table was succesfull, if not try to read it differently
    if(inherits(tmp, "try-error") || all(dimnames(tmp)[[1]]==as.character(1:dim(tmp)[1]))) {
      tmp <- read.csv(scenario_config, as.is=TRUE, sep=";", check.names=FALSE, colClasses = "character")
      dimnames(tmp)[[1]] <- tmp[,1]
      tmp <- tmp[,-1]
    }
    scenario_config <- tmp
    
    
  for (x in scenario) {
    if(!(x %in% colnames(scenario_config))) stop("No settings for scenario ", x, " found in scenario config!")
    message("\nApply ", x, " settings on config:")
    
    # if there are no switches containing "$" all switches are interpreted as gams switches
    if (!any(grepl("\\$",rownames(scenario_config)))) {
      rownames(scenario_config) <- paste0("gms$",rownames(scenario_config))
    }
    
    # loop over switches to chanhe their values
    for(i in rownames(scenario_config)){
      from <- eval(parse(text=paste0("cfg$",i))) # this way, because i can contain a list. In this case access with [[i]] would not work
      to   <- scenario_config[i,x]
      if (is.null(from)) stop("Scenario setting ",i," could not be found in the cfg provided.")
      if(from!=to & to!=""){
        message("  Update setting | ",i,":",from, " -> ",to)
        if(suppressWarnings(!is.na(as.numeric(to)))) to <- as.numeric(to)
        if(grepl(",",to,fixed=TRUE)) to <- strsplit(to,",")[[1]]
        # finally set the switch
        eval(parse(text=paste0("cfg$",i, "<- to")))
      }   
    }
    message("Config successfully updated!\n")
  }
    
  return(cfg)  
}
