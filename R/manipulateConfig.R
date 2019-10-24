#' Replace in File
#' 
#' Function to set configuration parameters in configuration files (e.g.
#' default.cfg and magpie.gms). This replacement is useful, when using R to
#' manage different model runs at once.  Please check your results after
#' replacement!
#' 
#' 
#' @usage manipulateConfig(configFile, ..., line_endings="os")
#' @param configFile a character string containing the name of the
#' configuration file, that should be manipulated. Supported file formats are
#' at the moment "gms", "inc", "cfg" (R-syntax), "php", "opt" and "cmd". Other
#' formats are currently not supported
#' @param ... Variables, that should be set to new values, e.g. title="test"
#' for default.cfg or s_max_timesteps=10 for magpie.gms
#' @param line_endings "win" for windows line endings or "os" for line endings
#' in the format of the currently used OS.
#' @author Jan Philipp Dietrich, Markus Bonsch, David Klein
#' @export
#' @seealso \code{\link{replace_in_file}},\code{\link{manipulateFile}}
#' @examples
#' 
#' \dontrun{manipulateConfig("config/default.cfg",input="test_new_yields",title="yihaa",revision=4.2)
#' manipulateConfig("magpie.gms",s_max_timesteps=4,s_use_gdx=-1)}
#' 
#' 
#' 
manipulateConfig<-function (configFile, ..., line_endings="os")
{
    tmp <- list(...)
    if(length(tmp)==1){
      if(is.list(tmp[[1]])){
        tmp<-tmp[[1]]
      }
    }
    m <- list()
    type <- substring(configFile, nchar(configFile) - 2)
    if (type == "gms" | type == "inc") {
        for (i in names(tmp)) {
          m[[paste(i,"_pattern1",sep="")]][1] <- paste("(\\$[sS][eE][tT][gG][lL][oO][bB][aA][lL][\\t ]*", i,"[\\t ]).*?( *!!|$)", sep = "")
          m[[paste(i,"_pattern1",sep="")]][2] <- paste("\\1 ", tmp[[i]], "\\2", sep = "")
          m[[paste(i,"_pattern2",sep="")]][1] <- paste("(^[\\t ]*(scalar|parameter|)s?[\\t ]*", i, "[\\t ][^/]*/)[^/]*", sep = "")
          m[[paste(i,"_pattern2",sep="")]][2] <- paste("\\1 ", tmp[[i]], " ", sep = "")
          m[[paste(i,"_pattern3",sep="")]][1] <- paste("((^|[\\t ])",i,"[ \\t]*=[ \\t]*[\"\']?)[^\"\';]*",sep = "")
          m[[paste(i,"_pattern3",sep="")]][2] <- paste("\\1", tmp[[i]], sep = "")
        }
    }
    else if (type == "cfg" | type == "R" | type == "r") {
        for (i in names(tmp)) {
            if(is.character(tmp[[i]])) tmp[[i]] <- paste("\"",tmp[[i]],"\"",sep="")
            m[[i]][1] <- paste("((^|[\\t \\$])",i,"[ \\t]*<-[ \\t]*)[\"\']?[^\"\']*[\"\']?",sep = "")
            m[[i]][2] <- paste("\\1", tmp[[i]], sep = "")
        }
    }
    else if (type == "cmd" | type == ".sh") {
        for (i in names(tmp)) {
            m[[i]][1] <- paste("((^|[\\t ])",i,"[ \\t]*=[ \\t]*[\"\']?)[^\"\']*",sep = "")
            m[[i]][2] <- paste("\\1", tmp[[i]], sep = "")
        }
    }
    else if (type == "php") {
        for (i in names(tmp)) {
            m[[i]][1] <- paste("((^|[\\t ])\\$",i,"[ \\t]*=[ \\t]*[\"\']?)[^\"\';]*",sep = "")
            m[[i]][2] <- paste("\\1", tmp[[i]], sep = "")
        }
    }
    else if (type == "opt") {
        for (i in names(tmp)) {
            m[[i]][1] <- paste("((^|[\\t ])",i,"[ \\t]*=[ \\t]*[\"\']?)[^\"\';]*",sep = "")
            m[[i]][2] <- paste("\\1", tmp[[i]], sep = "")
        }
    }
    else {
        stop(paste("Unknown file type", type))
    }
    manipulateFile(configFile, m, line_endings=line_endings)
}
