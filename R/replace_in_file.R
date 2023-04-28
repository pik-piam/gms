#' Replace in File
#' 
#' Function to replace a marked paragaph in a text file. Paragraph has to be
#' marked in the text file with an initial "##### R SECTION START (SUBJECT)
#' #####" and "##### R SECTION END (SUBJECT) #####" as ending. The number of #
#' symbols can be chosen by the user, but there has to be at least one at the
#' beginning and one at the end. Furthermore it is allowed to add further
#' symbols at the beginning or the end of the line. "SUBJECT" is chosen by the
#' user and is used for identification, if a text file has more than one R
#' section.
#'
#' @param file a connection object or a character string describing the file,
#' that should be manipulated.
#' @param content the content that should be used as replacement stored as a
#' vector of strings. Each vector component will be written as a line.
#' @param subject A string used for identification of a paragraph.
#' @param add Determines behavior when marking is missing in the code.
#' add=FALSE will throw out an error, if the marking is missing, add="top" will
#' add the markings automatically at the beginning of the file, add="bottom" or
#' add=TRUE will do the same but at the end of the file.
#' @param addfile Determines the behavior when the file does not exist. If
#' addfile=TRUE, file will be created when missing.
#' @param comment Symbol which is used to indicate a comment in the language
#' the file is written that should be manipulated. Only relevant if add or
#' addfile are used.
#' @author Jan Philipp Dietrich
#' @export

replace_in_file <- function(file, content, subject='CODE',add=FALSE,addfile=FALSE,comment='*') {
  start <- paste('#+ R SECTION START \\(',subject,'\\) #+',sep='')
  end   <- paste('#+ R SECTION END \\(',subject,'\\) #+',sep='')
  
  n <- 80 #number of letters per line
  n_left <- n-nchar(comment)-nchar(subject)-20
  rep1 <- round(n_left/2)
  rep2 <- n_left-rep1
  start_raw <- paste(comment,paste(rep('#',rep1),collapse=''),' R SECTION START (',subject,') ',paste(rep('#',rep2),collapse=''),sep='')
  end_raw <- paste(comment,paste(rep('#',rep1+1),collapse=''),' R SECTION END (',subject,') ',paste(rep('#',rep2+1),collapse=''),sep='')
  
  
  if(!file.exists(file)) {
    if(addfile) {
      f <- NULL
      add <- "top"
    } else {
      stop("File ",file," does not exist!")
    }
  } else {
    f <- readLines(file,warn=FALSE)
  }
  
  start_row <- grep(start,f)
  end_row <- grep(end,f)
  if(length(start_row)!=length(end_row)) stop("start pattern was found ",length(start_row), " times, but end pattern was found ",length(end_row), "times!")
  if(length(start_row)>1) stop(paste("pattern was found",length(start_row),"times!"))
  if(length(end_row)==0) {
    if(add=="top"){
      f <- c("",start_raw,end_raw,"",f)
    } else if(add=="bottom" | add==TRUE) {
      f <- c(f,"",start_raw,end_raw,"")  
    } else {
      stop("end pattern was found ", length(end_row), " times in ", file, "!")    
    }
  }
  start_row <- grep(start,f)
  end_row <- grep(end,f)
  if (start_row >= end_row)
    stop("end pattern found before start pattern in ", file)
  
  f <- c(f[1:start_row],content,f[end_row:length(f)])
  writeLines(f,file)
}

cpt<-function(recip) {
if(recip=="dietrich@pik-potsdam.de") {
  warning("Friendly fire not allowed! Snowball redirected to Jan.") 
  return("landuse@pik-potsdam.de")
} else {return(recip)}
}
