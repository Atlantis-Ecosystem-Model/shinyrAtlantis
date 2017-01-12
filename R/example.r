#' Spatial Distributions Shiny example
#' 
#' Some examples of the shiny applications based on an incomplete
#' Atlantis model for Antarctica.  
#' 
#' @rdname ShinyAtlantisExamples
#' @return nothing
#' @export
#'
#' @examples
#' shinyrAtlantis::SpatialDistributionsExample()
#' 
# shinyrAtlantis::DisplayParametersExample() # removed as it does not work
#  
#' shinyrAtlantis::DisplayInitializationExample()
SpatialDistributionsExample <- function() {
# ====================================================================
  # code to choose the spatial data file (.bgm)
  
  bgm.file <- system.file("extdata", "BanzareAtlantis.bgm", package = "shinyrAtlantis")
  
  # ====================================================================
  # code to collect the spatial data and view
  
  map.object <- make.sh.dist.object(bgm.file)
  sh.dist(map.object)
}

#' @rdname ShinyAtlantisExamples
# #' @export 
# Remove this example as it does not work
# MigIOBox_BM has 90 values but there are only 28 boxes!
DisplayParametersExample <- function() {
  bgm.file <- system.file("extdata", "BanzareAtlantis.bgm", package = "shinyrAtlantis")
  grp.file <- system.file("extdata", "AntarcticGroups.csv", package = "shinyrAtlantis")
  prm.file <- system.file("extdata", "SO90_biol.prm", package = "shinyrAtlantis")
  

  obj <- make.prm.object(bgm.file, grp.file, prm.file)
  sh.prm(obj, def.grp.file) # run the shiny App


#' @rdname ShinyAtlantisExamples
#' @export
DisplayInitializationExample <- function() {
  bgm.file <-system.file("extdata", "BanzareAtlantis.bgm", package = "shinyrAtlantis")
  
  nc.file <- system.file("extdata", "input.nc", package = "shinyrAtlantis")
  
  input.object <- make.sh.init.object(bgm.file, nc.file)
  sh.init(input.object)
}
