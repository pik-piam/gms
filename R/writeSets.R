#' writeSets
#' 
#' Function to write a set declaration in GAMS syntax given either a
#' a character vector or a data.frame (latter to represent a mapping set)
#' 
#' @param sets a list of sets where every set is another list consisting of 
#' three entries: name (set name as character), desc (set description as 
#' character) and items (set elements, either as character vector or data.frame)
#' @param file Name of the file the set declarations should be written to. If
#' NULL the result will just be returned by the function. The file must exist
#' and contain a line of the form "##### R SECTION START (SETS) #####"
#' followed by a line of the form ##### R SECTION END (SETS) ##### to indicate
#' the part of the file that should be replaced with the set declaration!
#' @return set declaration in GAMS syntax
#' @author Jan Philipp Dietrich, Florian Humpenöder
#' @examples 
#' countries <- c("DEU", "FRA", "ENG", "ITA", "NLD", "POL")
#' map <- data.frame(region = rep(c("REG1","REG2"), 3), countries = countries)
#' sets <- list(list(name  = "countries", 
#'                   desc  = "list of countries", 
#'                   items = countries),
#'              list(name = "country2region",
#'                   desc = "mapping between countries and regions",
#'                   items = map))
#'  cat(writeSets(sets), sep="\n")
#' @export
#' 
writeSets <- function(sets, file = NULL) {
  
  if (!is.list(sets)) stop("sets must be a list!")
  if (!is.list(sets[[1]])) sets <- list(sets)
  
  header <- c(
    '* THIS CODE IS CREATED AUTOMATICALLY, DO NOT MODIFY THESE LINES DIRECTLY',
    '* ANY DIRECT MODIFICATION WILL BE LOST AFTER NEXT AUTOMATIC UPDATE!')
  
  content <- c(header,'','sets','')
  
  for (s in sets) {
    content <- c(content, paste0('  ', s$name, ' ', s$desc))
    content <- c(content, .setFormatting(s$items))
    content <- c(content,'')
  }
  content <- c(content,';')
  
  if (is.null(file)) return(content)
  replace_in_file(file, content, 'SETS')
}

.setFormatting <- function(x, maxchar = 80, raw = FALSE) {
  
  if (is.data.frame(x)) {
    if (dim(x)[2] == 1) {
      x <- x[[1]]
    } else if (dim(x)[2] == 2) {
      for (i in 1:2) x[[i]] <- as.character(x[[i]])
      out <- NULL
      for (i in unique(x[[1]])) {
        j <- x[[2]][x[[1]] == i]
        out <- c(out, paste0(i," . (",.setFormatting(j, maxchar = maxchar - nchar(i) - 5, raw = TRUE), ")"))
      }
      x <- out
    } else { 
      stop("Mappings with ", dim(x)[2], "columns are currently not supported!")
    }
  }
  
  x <- as.character(x)
  
  content <- NULL
  isMap <- function(x) return(grepl(".", x[1], fixed = TRUE))
  if (isMap(x)) {
    n <- 1
  } else {
    maxlen <- max(nchar(x))
    if (maxlen > 5) {
      n <- 1
    } else {
      n <- max(1, floor((maxchar - 6)/(maxlen + 2)))
    }
  }
  tmp <- lapply(split(x, ceiling(seq_along(x)/n)), paste, collapse = ", ")
  end <- ifelse(raw || isMap(x), "",",")
  start <- ifelse(raw, "","    / ")
  for (i in 1:length(tmp)) {
    if (i == length(tmp) && !raw) end <- " /"
    content <- c(content,paste0(start,tmp[[i]],end))
    if (!raw) start <- "      "
  }
  return(content)
}
