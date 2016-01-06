
#install.packages(c("DT", "devtools", "dplyr", "ggplot2", "scales", "shiny", "stringr"))
#devtools::install_github(repo = "mareframe/vat")
library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(stringr)
library(vat)
library(scales)

source("R/Distribution Tool v2.R")
bgm.file <- file.path("data", "BanzareAtlantis.bgm")

map.object <- make.map.object(bgm.file)
sh.dist(map.object)


source("R/shinySetupTool Antarctica.R")

def.all.file <- file.path("data", "paramdefns.csv")
def.grp.file <- file.path("data", "grpTemplates.csv")

# Anatarctic model files 
grp.file <- paste(wd, "AntarcticGroups.csv", sep = "")
prm.file <- paste(wd, "SO90_biol.prm", sep = "")
# generate the object of data that is read in by the shiny app (takes a few minutes)
obj.ast.Antarctic <- make.setup.object(bgm.file, grp.file, prm.file)
sh.ast(obj.ast.Antarctic) # run the shiny App
