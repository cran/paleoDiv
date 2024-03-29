% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/paleoDiv.R
\name{tsconv}
\alias{tsconv}
\title{Convert geological ages for accurate plotting alongside a calibrated phylogeny}
\usage{
tsconv(x, phylo0 = NULL, root.time = phylo0$root.time)
}
\arguments{
\item{x}{A vector of geological ages to be converted.}

\item{phylo0}{Phylogeny from which to take root.age}

\item{root.time}{Numeric root age, if not taken from a phylogeny}
}
\value{
A numeric() containing the converted geological ages
}
\description{
Convert geological ages for accurate plotting alongside a calibrated phylogeny
}
\examples{
tsconv(c(252,201,66), root.time=300)
}
