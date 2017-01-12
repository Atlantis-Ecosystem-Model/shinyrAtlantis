#' @title A package for visualising Atlantis data
#'
#' @description
#' A set of functions that generate objects used by shiny applications. 
#' There are two types of functions (1) make functions collect relevant data from 
#' Atlantis parameter files and convert them into list objects, and (2) shiny
#' applications that take the list objects and allow the user to view the data
#' in terms of tables and maps.
#'
#' @details
#' sh.prm displays data stored in a .prm file.
#' 
#' sh.init displays data stored in the .nc file containing initial values.
#' 
#' sh.dist generates horizontal probability distributions that can be cut-and-pasted into a .prm file.
#' @importFrom shiny a  
#' @name shinyrAtlantis
#' @docType package
NULL
