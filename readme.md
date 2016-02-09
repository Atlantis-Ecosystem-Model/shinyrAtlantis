<!-- README.md is generated from README.Rmd. Please edit that file -->
Shiny Atlantis
--------------

Development
-----------

1.  Clone this repo in RStudio, New Project, Version Control, Git, "<https://github.com/shanerichards/shinyrAtlantis.git>"
2.  Click Tools/Project Options/Build Tools/Generate documentation with roxygen - click all options ON.
3.  Click Build & Reload in the Build Tab (or hit Ctrl-SHIFT-Enter).
4.  Make edits, build/reload/test, commit.
5.  Etc.

Usage
-----

Install these dependencies.

``` r
install.packages(c("DT", "devtools", "dplyr", "ggplot2", "scales", "shiny", "stringr", "data.table"))
devtools::install_github(repo = "mareframe/vat")
```

Install the package (from private Github).

``` r
devtools::install_git("shanearichards/shinyrAtlantis ")
```

Load packages (can fix by namespace later)

``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(shiny)
library(DT)
#> 
#> Attaching package: 'DT'
#> The following objects are masked from 'package:shiny':
#> 
#>     dataTableOutput, renderDataTable

library(ggplot2)

library(stringr)
## library(vat) 
library(scales)

library(shinyrAtlantis)
```

Spatial Distributions "sh.dist"
-------------------------------

Shiny application for generating spatial distributions with constant density (per unit area)

``` r
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
```

To launch the above app in one step, run the `spatialDistributions` example.

``` r
shinyrAtlantis::SpatialDistributionsExample()
```

Shiny PRM Run `shprm.R`
-----------------------

``` r
library(shinyrAtlantis)
library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(stringr)


def.all.file <- system.file("extdata", "paramdefns.csv", package = "shinyrAtlantis")
def.grp.file <- system.file("extdata", "grpTemplates.csv", package = "shinyrAtlantis")
bgm.file <- system.file("extdata", "BanzareAtlantis.bgm", package = "shinyrAtlantis")
grp.file <- system.file("extdata", "AntarcticGroups.csv", package = "shinyrAtlantis")
prm.file <- system.file("extdata", "SO90_biol.prm", package = "shinyrAtlantis")

obj <- make.prm.object(bgm.file, grp.file, prm.file)
sh.prm(obj) # run the shiny App
```

Shiny INIT Run `shinit.R`
-------------------------

``` r
library(shinyrAtlantis)
library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(stringr)
library(ncdf4)


bgm.file <-system.file("extdata", "BanzareAtlantis.bgm", package = "shinyrAtlantis")

nc.file <- system.file("extdata", "input.nc", package = "shinyrAtlantis")

input.object <- make.init.object(bgm.file, nc.file)
a <- sh.init(input.object)
```
