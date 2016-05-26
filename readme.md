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

Install dependencies *if they are not already installed*. The complete list of packages used to develop the shinyrAtlantis package are provided below. Remove the package names that are already installed. 

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

Some example Atlantis files are provided in the package so that the shiny applications can be explored straight away. Note that some of the example Atlantis files are incomplete. The shiny application allows exploration of most incomplete Atlantis files. See the examples presented below for how to use the shiny applications.

Spatial Distributions "sh.dist"
-------------------------------

**sh.dist**: A shiny application for generating spatial distributions with constant density (per unit area).

``` r
library(shinyrAtlantis)

bgm.file <- system.file("extdata", "BanzareAtlantis.bgm", package = "shinyrAtlantis")

obj <- make.sh.dist.object(bgm.file)
sh.dist(obj)
```

Shiny PRM Run `shprm.R`
-----------------------

**sh.prm**: A shiny application for exploring the data in the bioloy parameter file.

``` r
library(shinyrAtlantis)

bgm.file <- system.file("extdata", "BanzareAtlantis.bgm", package = "shinyrAtlantis")
grp.file <- system.file("extdata", "AntarcticGroups.csv", package = "shinyrAtlantis")
prm.file <- system.file("extdata", "SO90_biol.prm", package = "shinyrAtlantis")

obj <- make.sh.prm.object(bgm.file, grp.file, prm.file)
sh.prm(obj)
```

Shiny INIT Run `shinit.R`
-------------------------

**sh.int**: A shiny application for exploring the initial conditions file.

``` r
library(shinyrAtlantis)

bgm.file <-system.file("extdata", "BanzareAtlantis.bgm", package = "shinyrAtlantis")
nc.file <- system.file("extdata", "input.nc", package = "shinyrAtlantis")

obj <- make.sh.init.object(bgm.file, nc.file)
sh.init(obj)
```

Forcings "sh.forcings"
-------------------------------

**sh.forcings**: A shiny application for exploring the forcings data (time-series of salt and temperature).

``` r
library(shinyrAtlantis)

salinity.file    <- "GBR108_salt.nc"       # this file is not included in the package
temperature.file <- "GBR108_temp.nc"       # this file is not included in the package
bgm.file         <- "gbr_box_03012012.bgm" # this file is not included in the package
cum.depth <- c(0,5,10,20,50,100,200,3000)  # cumulative water layer depths

input.object <- make.sh.forcings.object(
  bgm.file         = bgm.file,
  exchange.file    = exchange.file,
  cum.depth        = cum.depth,
  temperature.file = temperature.file,
  salinity.file    = salinity.file
)

sh.forcings(input.object)
```

One step launch
---------------

Some of the above apps can be launched in one step.

``` r
shinyrAtlantis::SpatialDistributionsExample()
```

``` r
shinyrAtlantis::DisplayParametersExample()
```

``` r
shinyrAtlantis::DisplayInitializationExample()
```
