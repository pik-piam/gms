% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readDefaultConfig.R
\name{readDefaultConfig}
\alias{readDefaultConfig}
\title{readDefaultConfig}
\usage{
readDefaultConfig(path)
}
\arguments{
\item{path}{path of the main folder of the model}
}
\value{
A vector of parameter values and their names.
}
\description{
Reads the default configuration of the model. Uses default.cfg or main.cfg as the source as appropriate.
To read a configuration from YAML format, use \code{\link{loadConfig}} instead.
}
\seealso{
\code{\link{loadConfig}}
}
\author{
Mika Pflüger
}
