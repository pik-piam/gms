% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loadConfig.R
\name{loadConfig}
\alias{loadConfig}
\title{Load Config}
\usage{
loadConfig(cfg)
}
\arguments{
\item{cfg}{Either a character string naming a file which containes the config
or a character string containing the config as YAML code.}
}
\description{
Load config in YAML format as written via
\code{\link{saveConfig}}.
}
\details{
To read in the default configuration (stored as R list in default.cfg or in a gams file), use
\code{\link{readDefaultConfig}} instead.
}
\examples{
cfg <- list(input = c(data1 = "blub.tgz", data2 = "bla.tgz"), mode = "default")
yml <- saveConfig(cfg)
loadConfig(yml)
}
\seealso{
\code{\link{saveConfig}}
}
\author{
Jan Philipp Dietrich
}
