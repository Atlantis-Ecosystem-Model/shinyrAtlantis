shinyrAtlantis
--------------

A package to help users of Atlantis investigate the parameters that are spread across multiple files (.csv, .nc) when constructing their ecosystem model.

Usage in R
----------

Install dependencies *if they are not already installed*. The complete list of packages used to develop the shinyrAtlantis package are provided below. Remove the package names that are already installed.

``` r
install.packages(c("shiny", "dplyr", "DT",
                   "ggplot2", "ncdf4", "stringr"))
```

Install the package from Github with devtools.

``` r

if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")
}
devtools::install_github("https://github.com/Atlantis-Ecosystem-Model/shinyrAtlantis")
```

Load and attach the package.

``` r
library(shinyrAtlantis)
#> Loading required package: shiny
```

Some example Atlantis files are provided in the package so that the shiny applications can be explored straight away. Note that some of the example Atlantis files are incomplete. The shiny application allows exploration of most incomplete Atlantis files. See the examples presented below for how to use the shiny applications. Also see the documentation provided by typing `?shinyrAtlantis`.

The code assumes that in the group.csv file the header of the column that contains the group type is labeled **GroupType** (it is also commonly labelled InvertType, which is missleading and will cause my code to crash).

**Additional functionality to the shiny applications provided in this package, and new shiny applications, are currently under development.**

`sh.dist`
-------------------------------

**sh.dist**: A shiny application for generating spatial distributions with constant density (per unit area). Code is located in file `shdist.R`.

``` r
library(shinyrAtlantis)

bgm.file <- system.file("extdata", "BanzareAtlantis.bgm", package = "shinyrAtlantis")

obj <- make.sh.dist.object(bgm.file)
sh.dist(obj)
```

`sh.prm`
-----------------------

**sh.prm**: A shiny application for exploring the data in the bioloy parameter file. Code is located in file `shprm.R`.

``` r
library(shinyrAtlantis)

bgm.file <- system.file("extdata", "BanzareAtlantis.bgm", package = "shinyrAtlantis")
grp.file <- system.file("extdata", "AntarcticGroups.csv", package = "shinyrAtlantis")
prm.file <- system.file("extdata", "SO90_biol.prm", package = "shinyrAtlantis")

obj <- make.sh.prm.object(bgm.file, grp.file, prm.file)
sh.prm(obj)
```

`sh.init`
-------------------------

**sh.init**: A shiny application for exploring the initial conditions file. Code is located in file `shinit.R`.

``` r
library(shinyrAtlantis)

bgm.file <-system.file("extdata", "BanzareAtlantis.bgm", package = "shinyrAtlantis")
nc.file <- system.file("extdata", "input.nc", package = "shinyrAtlantis")

obj <- make.sh.init.object(bgm.file, nc.file)
sh.init(obj)
```

`sh.forcings`
-------------------------------

**sh.forcings**: A shiny application for exploring the forcings data (time-series of salt and temperature). Code is located in file `shforce.R`.

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

`make.init.nc`
-------------------------------

**make.init.nc**: An application for creating an Atlatnis initial condition file.

``` r
library(shinyrAtlantis)

## Step 1  -  Create your template files
grp.file   <- "Groups.csv"                                 # this file is not included in the package
bgm.file   <- "gbr_box_03012012.bgm"                       # this file is not included in the package
cum.depths <- c(0, 20, 50, 150, 250, 400, 650, 1000, 4300) # this file is not included in the package
csv.name   <- "GBR_init"                                   # Name of the template file
make.init.csv(grp.file, bgm.file, cum.depths, csv.name)    # Create the template files

## Step 2  - Populate the template csv files with biological and physical information realted to your Atlantis model

## Step 3  -  Create your NetCDF file from the templates
nc.file    <- "GBR.nc"               # Name of your initial condition Netcdf file.
GBR.init   <- "GBR_init.csv"         # template file with overall information
GBR.init.h <- "GBR_init_horiz.csv"   # template file with Horizontal information
make.init.nc(bgm.file, cum.depths, GBR.init, GBR.init.h, nc.file)

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

Package development
--------------------

If you wish to contribute to the development of this package then perform the following steps.

1.  Clone this repo in RStudio, New Project, Version Control, Git, "<https://github.com/Atlantis-Ecosystem-Model/shinyrAtlantis.git>"
2.  Click Tools/Project Options/Build Tools/Generate documentation with roxygen - click all options ON.
3.  Click Build & Reload in the Build Tab (or hit Ctrl-SHIFT-Enter).
4.  Make edits, build/reload/test, commit.
5.  Etc.

Author
--------------------

* **Shane A. Richards**

## License

This project is licensed under [GPL3](https://www.gnu.org/licenses/gpl-3.0.en.html)
