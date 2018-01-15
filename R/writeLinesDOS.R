writeLinesDOS <- function(text, con = stdout()) {
  if(Sys.info()['sysname']!="Windows") {
    writeLines(text,con,sep="\r\n")
  } else {
    writeLines(text,con)  
  }
}