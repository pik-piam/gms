#' gamsequation2tex
#' 
#' Convert a gams equation into latex code
#' 
#' 
#' @param x GAMS equation provided as character
#' @return GAMS equation converted to latex code
#' @author Jan Philipp Dietrich
#' @export
#' @importFrom stringi stri_extract_all_regex stri_replace_first_regex
#' @seealso \code{\link{goxygen}}
#' @examples
#' 
#'   x <- "eq_1 .. v_a =e= sum(j,v_b(j)*((1-s_c)+sum(cell(i,j),v_d(i)/f_d(i))));"
#'   cat(gamsequation2tex(x))

gamsequation2tex <- function(x) {
  
  convert_side <- function(x) {
    
    extract_vars <- function(x, variable = "[\\w]+(\\([\\w,]*\\)|)", code="v") {
      if(length(x)!=1) stop("Works only for 1 element!")
      
      vars <- stri_extract_all_regex(x,variable)[[1]]
      names(vars) <- paste0("#",code,1:length(vars),"#")
      names(vars) <- gsub("(\\w)","\\1|:.",names(vars))
      for(v in names(vars)) {
        x <- stri_replace_first_regex(x,variable,v)
      }
      names(vars) <- gsub("|:.","",names(vars),fixed=TRUE)
      x <- gsub("|:.","",x,fixed=TRUE)
      return(list(x=x,vars=vars))
    } 
    
    convert_sumprod <- function(x) {
      sum <- "(#\\w+#)\\(([^(,]+|\\(.*?\\)),"
      x <- gsub(sum,"\\1_{\\2}(",x)
      return(x)
    }
    
    convert_vars <- function(v) {
      v <- sub("^(sum|prod)$","\\\\\\1",v)
      return(gsub("\\_","\\\\_",v))
    }
    
    extract_braceblocks <- function(x, level=1) {
      braceblock <- "\\([^\\)\\(]*\\)"
      y <- extract_vars(x,braceblock,paste0("b",level,"."))
      y$vars <- gsub("[\\(\\)]","",y$vars)
      if(grepl("\\(",y$x)) {
        tmp <- extract_braceblocks(y$x, level=level+1)
        y$x <- tmp$x
        y$vars <- c(tmp$vars,y$vars)
      }
      return(y)
    }
    
    convert_blocks <- function(x, addbraces=FALSE) {
      names <- names(x)
      # handle exponents
      x <- gsub("\\*\\*([^*+-/]+)","^{\\1}",x)
      # handle divisions
      x <- gsub("([^*+-/]+)/([^*+-/]+)","\\\\frac{\\1}{\\2}",x)
      #handle multiplications
      x <- gsub("*", " \\cdot ", x, fixed=TRUE)
      #add braces back
      if(addbraces) x <- paste0("\\left(",x,"\\right)")
      names(x) <- names
      return(x)
    }
    
    merge_back <- function(x,vars) {
      for(i in names(vars)) x <- sub(i,vars[i],x,fixed=TRUE)
      return(x)
    }  
    
    
    variable <- "[\\w]{2,}(\\([\\w,\"]*\\)|)"
    y <- extract_vars(x,variable,"v")
    y$vars <- convert_vars(y$vars)
    y$x <- convert_sumprod(y$x)
    
    z <- extract_braceblocks(y$x)
    
    z$x    <- convert_blocks(z$x)
    z$vars <- convert_blocks(z$vars, addbraces=TRUE)
    
    x <- merge_back(z$x,c(z$vars,y$vars))
    
    return(x)
  }
  
  #remove spaces and line breaks
  x <- gsub("[\n ]","",x)
  
  # split name and equation
  pattern <- "^(.*)\\.\\.([^;]*);?$"
  name <- sub(pattern,"\\1",x)
  eq <- sub(pattern,"\\2",x)
  
  #split sides
  pattern <- "^(.*)(=[lgen]=)(.*)$"
  left <- sub(pattern,"\\1",eq)
  middle <- sub(pattern,"\\2",eq)
  right <- sub(pattern,"\\3",eq)
  
  middle <- switch(middle,
                   "=e=" = "=",
                   "=l=" = "\\\\leq",
                   "=g=" = "\\\\geq",
                   "=n=" = "\\\\neq")
  
  
  left <- convert_side(left)
  right <- convert_side(right)
  
  return(paste(left,middle,right))
}




  

