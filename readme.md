Shiny Atlantis
--------------

Development
-----------

1.  Clone this repo in RStudio, New Project, Version Control, Git, "<https://github.com/shanearichards/shinyrAtlantis.git>"
2.  Click Tools/Project Options/Build Tools/Generate documentation with roxygen - click all options ON.
3.  Click Build & Reload in the Build Tab (or hit Ctrl-SHIFT-Enter).
4.  Make edits, build/reload/test, commit.
5.  Etc.

Usage
-----

Install dependencies.

``` r
install.packages(c("shiny", 
                 "dplyr", 
                 "DT",
                 "ggplot2",
                 "ncdf4",
                 "stringr"))
```

Install the package from Github with devtools.

``` r

if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")
}
devtools::install_github("shanearichards/shinyrAtlantis")
```

Load and attach the package.

``` r
library(shinyrAtlantis)
#> Loading required package: shiny
```

**Some of the examples given below may not function correctly due to recent changes.**

Spatial Distributions "sh.dist"
-------------------------------

Shiny application for generating spatial distributions with constant density (per unit area)

``` r

library(shinyrAtlantis)
# ====================================================================
# code to choose the spatial data file (.bgm)

bgm.file <- system.file("extdata", "BanzareAtlantis.bgm", package = "shinyrAtlantis")

# ====================================================================
# code to collect the spatial data and view

map.object <- make.sh.dist.object(bgm.file)
sh.dist(map.object)
```

Shiny PRM Run `shprm.R`
-----------------------

``` r
library(shinyrAtlantis)

def.all.file <- system.file("extdata", "paramdefns.csv", package = "shinyrAtlantis")
def.grp.file <- system.file("extdata", "grpTemplates.csv", package = "shinyrAtlantis")
bgm.file <- system.file("extdata", "BanzareAtlantis.bgm", package = "shinyrAtlantis")
grp.file <- system.file("extdata", "AntarcticGroups.csv", package = "shinyrAtlantis")
prm.file <- system.file("extdata", "SO90_biol.prm", package = "shinyrAtlantis")

## prior to packaging, the scope of these file names was a bit "global"
obj <- make.sh.prm.object(bgm.file, grp.file, prm.file, def.all.file, def.grp.file)
## this def.grp.file contents should be included in object above??
sh.prm(obj, def.grp.file)  # run the shiny App
```

Shiny INIT Run `shinit.R`
-------------------------

``` r
library(shinyrAtlantis)


bgm.file <-system.file("extdata", "BanzareAtlantis.bgm", package = "shinyrAtlantis")

nc.file <- system.file("extdata", "input.nc", package = "shinyrAtlantis")

input.object <- make.sh.init.object(bgm.file, nc.file)
a <- sh.init(input.object)
```

One step launch
---------------

To launch any the above apps in one step, run the these examples.

``` r
shinyrAtlantis::SpatialDistributionsExample()
```

``` r
shinyrAtlantis::DisplayParametersExample()
```

``` r
shinyrAtlantis::DisplayInitializationExample()
```
