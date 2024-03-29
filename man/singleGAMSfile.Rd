% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/singleGAMSfile.R
\name{singleGAMSfile}
\alias{singleGAMSfile}
\title{Merge GAMS code into single file}
\usage{
singleGAMSfile(
  modelpath = ".",
  mainfile = "main.gms",
  output = "full.gms",
  embedRScripts = FALSE
)
}
\arguments{
\item{modelpath}{The path where the model is stored}

\item{mainfile}{The path to the main gams file (relative to the model path)}

\item{output}{Name of the single output GAMS file.}

\item{embedRScripts}{If TRUE, R scripts called by GAMS via Execute are also embedded. Default FALSE}
}
\description{
This function merges GAMS code which is distributed over severals files into
a single GAMS file. Optionally, it also embeds R scripts into the single GAMS
file
}
\examples{
# copy dummymodel create single gms file out of it
file.copy(system.file("dummymodel", package = "gms"), tempdir(), recursive = TRUE)
model      <- paste0(tempdir(), "/dummymodel")
singlefile <- paste0(tempdir(), "/full.gms")
singleGAMSfile(modelpath = model, output = singlefile)

}
\author{
Jan Philipp Dietrich, Anastasis Giannousakis
}
