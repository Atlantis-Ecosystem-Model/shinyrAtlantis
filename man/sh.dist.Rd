% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shdist.R
\name{sh.dist}
\alias{sh.dist}
\title{Shiny application for generating horizontal distributions}
\usage{
sh.dist(map.object)
}
\arguments{
\item{map.object}{R list object generated from \code{\link[shinyrAtlantis]{make.sh.dist.object}}.}
}
\value{
Object of class 'shiny.appobj' see \code{\link[shiny]{shinyApp}}.
}
\description{
Takes data from a .bgm box geometry file used by Atlantis to define box boundaries and provides
a visualisation of the data in the form of a shiny application.
The .bgm file must first be pre-processed by \code{\link[shinyrAtlantis]{make.sh.dist.object}}, which generates a
list object that is the parameter to \code{sh.dist}.
The application allows users to create probability distributions that
describe how a species is distributed across boxes assuming that the distribution
is uniform within the spatial range defined by the user. The output produced within the shiny output can then
be cut-and-pasted into an Atlantis .prm file.
}
\examples{
\dontrun{
bgm.file <- "gbr_box_03012012.bgm"
dist.object <- make.sh.dist.object(bgm.file)
sh.dist(dist.object)
}
}
