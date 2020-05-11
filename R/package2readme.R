#' package2readme
#' 
#' Creates a README.md for a R package. 
#' 
#' @param package either the path to the main folder of a package (containing a DESCRIPTION file)
#' or the name of the package
#' @author Jan Philipp Dietrich
#' @importFrom desc desc
#' @importFrom utils citation vignette
#' @examples
#' 
#' package2readme("lucode")
#' @export
package2readme <- function(package=".") {
  if(file.exists(paste0(package,"/DESCRIPTION"))) {
    d <- desc(file=paste0(package,"/DESCRIPTION"))
    folder <- package
  } else {
    d <- desc(package=package)
    folder <- NULL
  }
  
  fillTravis <- function(d){
    pkg <- d$get("Package")
    z <- grep("github",d$get_urls(),value=TRUE)
    if(length(z)==0) return("")
    path <- strsplit(z,"github.com",fixed=TRUE)[[1]][2]
    out <- paste0("[![Travis build status](https://travis-ci.com", 
                  path, ".svg?branch=master)](https://travis-ci.com",
                  path, ")")
    return(out)
  }
  
  fillZenodo <- function(d) {
    z <- grep("zenodo",d$get_urls(),value=TRUE)
    if(length(z)==0) return("")
    doi <- strsplit(z,"doi.org/",fixed=TRUE)[[1]][2]
    out <- paste0("[![DOI](https://zenodo.org/badge/DOI/",doi,
                  ".svg)](https://doi.org/",doi,")")
    return(out)
  }
  
  fillCite <- function(d) {
    out <- c("\nTo cite package **",d$get("Package"),"** in publications use:\n\n",
             format(citation(package=d$get("Package")),style="text"),
             "\n\nA BibTeX entry for LaTeX users is\n\n ```latex\n",
             format(citation(package=d$get("Package")),style="bibtex"),"\n```")
    return(paste(out,collapse=""))
  }
  
  fillVignette <- function(d) {
    v <- vignette(package=d$get("Package"))
    if(dim(v$results)[1]==0) return("")
    else if(dim(v$results)[1]==1) {
      vtext <- "a vignette"
      vtext2 <- "it"
    } else {
      vtext <- "vignettes"
      vtext2 <- "them"
    }
    tmp <- paste0("vignette(",v$results[,"Item"],")")
    tmp <- format(tmp,width = max(nchar(tmp)))
    vig <- paste0(tmp," # ", sub("(source, html)","",v$results[,"Title"],fixed=TRUE), 
                  collapse="\n")
    out <- c("\n## Tutorial\n\n",
             "The package comes with ",vtext," describing the basic functionality ",
             "of the package and how to use it. You can load ",vtext2," with the following ",
             "command (the package needs to be installed):\n\n",
             "```r\n",
             vig,
             "\n```\n")
    return(paste(out,collapse=""))
  }
  
  fillTemplate <- function(x,fill) {
    for(what in names(fill)) {
      x <- gsub(paste0("[:",what,":]"),fill[[what]],x,fixed=TRUE)
    }
    return(x)
  }
  
  template <- readLines(system.file("extdata","README_template.md",package = "lucode"))
 
  fill <- list(title       = d$get("Title"),
               package     = d$get("Package"),
               description = d$get("Description"),
               version     = d$get("Version"),
               maintainer  = d$get_maintainer(),
               zenodo      = fillZenodo(d),
               travis      = fillTravis(d),
               cite        = fillCite(d),
               vignette    = fillVignette(d))
  
  out <- fillTemplate(template, fill)

  if(!is.null(folder)) {
    readmefile <- paste0(folder,"/README.md")
    if(file.exists(readmefile)) message("Updated README.md file")
    else message("Added README.md file")
    writeLines(out,readmefile)
  } else {
    cat(out,sep="\n")
  }
  invisible(out)
}
