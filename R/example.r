#' Spatial Distributions Shiny example
#'
#' @return nothing
#' @export
#'
#' @examples
#' shinyrAtlantis::SpatialDistributionsExample()
SpatialDistributionsExample <- function() {
  
  library(shiny)
  library(dplyr)
  library(ggplot2)
  library(DT)
  library(stringr)
  
  library(shinyrAtlantis)
  # ====================================================================
  # code to choose the spatial data file (.bgm)
  
  bgm.file <- system.file("extdata", "BanzareAtlantis.bgm", package = "shinyrAtlantis")
  
  # ====================================================================
  # code to collect the spatial data and view
  
  map.object <- make.dist.object(bgm.file)
  sh.dist(map.object)
}