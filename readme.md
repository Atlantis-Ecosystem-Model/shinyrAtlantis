<!-- README.md is generated from README.Rmd. Please edit that file -->
Shiny Atlantis
--------------

Install these dependencies.

``` r
install.packages(c("DT", "devtools", "dplyr", "ggplot2", "scales", "shiny", "stringr"))
devtools::install_github(repo = "mareframe/vat")
```

Install the package (from local Git).

``` r
devtools::install_git("/home/shared/tools/Atlantis/ShinyAtlantis")
```

Load packages (can fix by namespace later)

``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> 
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> 
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(shiny)
library(DT)
#> 
#> Attaching package: 'DT'
#> 
#> The following objects are masked from 'package:shiny':
#> 
#>     dataTableOutput, renderDataTable

library(ggplot2)

library(stringr)
library(vat)
#> Loading required package: ncdf4
#> Loading required package: scales
#> Loading required package: plyr
#> -------------------------------------------------------------------------
#> You have loaded plyr after dplyr - this is likely to cause problems.
#> If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
#> library(plyr); library(dplyr)
#> -------------------------------------------------------------------------
#> 
#> Attaching package: 'plyr'
#> 
#> The following objects are masked from 'package:dplyr':
#> 
#>     arrange, count, desc, failwith, id, mutate, rename, summarise,
#>     summarize
#> 
#> Loading required package: tidyr
#> Loading required package: animation
#> Loading required package: gridExtra
library(scales)

library(ShinyAtlantis)
```

Distribution tool
-----------------

``` r
bgmfile <- system.file("extdata", "BanzareAtlantis.bgm", package= "ShinyAtlantis")

## namespace later
## there's a make.map.object for setup as well as distribution and they are different
mapobject <- ShinyAtlantis:::make.map.objectDistribution(bgmfile)
sh.dist(mapobject)
```

Setup tools
-----------

``` r
## these are globals (for easy shiny use, need to sort this out)
def.all.file <- system.file("extdata", "paramdefns.csv", package= "ShinyAtlantis")
def.grp.file <- system.file("extdata", "grpTemplates.csv", package = "ShinyAtlantis")

# Anatarctic model files 
grpfile <- system.file("extdata",  "AntarcticGroups.csv", package= "ShinyAtlantis")
prmfile <- system.file("extdata",  "SO90_biol.prm", package= "ShinyAtlantis")
# generate the object of data that is read in by the shiny app (takes a few minutes)
obj.ast.Antarctic <- ShinyAtlantis:::make.setup.object(bgmfile, grpfile, prmfile)
ShinyAtlantis:::sh.ast(obj.ast.Antarctic) # run the shiny App
```
