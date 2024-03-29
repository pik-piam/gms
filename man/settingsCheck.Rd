% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/settingsCheck.R
\name{settingsCheck}
\alias{settingsCheck}
\title{settingsCheck}
\usage{
settingsCheck(path = ".", modulepath = "modules", fileName = "main.gms")
}
\arguments{
\item{path}{path of the main folder of the model}

\item{modulepath}{path to the module folder relative to "path"}

\item{fileName}{name of the file containing setglobals, relative to "path"}
}
\value{
Nothing is returned.
}
\description{
Checks GAMS setglobals in code for consistency. Creates a warning if a
setglobal command for an existing module is missing or a module is set to a
realization which does not exist.
}
\seealso{
\code{\link{codeCheck}}
}
\author{
Jan Philipp Dietrich
}
