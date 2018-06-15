# Functions =====================================================================
# make.map.data.init: creates a data frame of box data from bgm and cummulative depths
# generate.vars.init: creates a data frame of all biological variables
# make.init.csv     : creates csv files for editing to then produce NetCDF files
# make.init.nc      : create a NetCDF file based on csv files
# get.init.nc       : generate a csv file of initial conditions from netCDF file

# ==============================================================================
# make.map.data.init
# ==============================================================================
make.map.data.init <- function(bgm.file, cum.depths){
  bgm <- readLines(bgm.file) # read in the geometry file

  numboxes <- 0
  txt.find <- "nbox"
  j <- grep(txt.find, bgm, value = FALSE)
  if (length(j) > 0) { # found text nbox
    jnew <- NULL
    for (jj in 1:length(j)) {
      # Valid row is when nbox is the first entry and second is a number
      text.split <- unlist(str_split(
        gsub(pattern = "[\t ]+", x = bgm[j[jj]], replacement = " "), " "))
      if ((text.split[1] == txt.find) &
          (str_extract(text.split[2], "[0-9.-]+") == text.split[2])) {
        jnew <- c(jnew,j[jj]) # add the row that satisfies the criteria
      }
    }
    j <- jnew # use this list of rows as they are valid
    if (length(j) == 1) { # a single row is found
      numboxes <- as.numeric(unlist(
        str_extract_all(bgm[j],"\\(?[0-9.-]+\\)?")[[1]])[1])
    }
  }

  # find the depths and areas, and identify island boxes
  box.indices <- rep(0, numboxes)
  for(i in 1:numboxes){ # box depth
    box.indices[i] <- grep(paste("box", i - 1, ".botz", sep = ""), bgm)
  }
  z.tmp <- strsplit(bgm[box.indices], "\t")
  z <- as.numeric(sapply(z.tmp,`[`,2)) # - depth of water column

  # create a data frame to store box data
  box.data <- data.frame(boxid = 0:(numboxes-1), total.depth = -z)
  # add island information
  box.data <- mutate(box.data, is.island = (total.depth <= 0.0))
  # add area information
  for(i in 1:numboxes){ # box area
    box.indices[i] <- grep(paste("box", i - 1, ".area", sep = ""), bgm)
  }
  a.tmp <- strsplit(bgm[box.indices], "\t")
  a <- as.numeric(sapply(a.tmp,`[`,2))
  box.data$area <- a
  # add total volume information
  box.data <- mutate(box.data, volume = total.depth*area)
  # allow islands to have positive volume = land volume
  box.data$volume[box.data$is.island] <- -box.data$volume[box.data$is.island]

  max.numlayers <- length(cum.depths) - 1 # maximum number of water layers

  # calculate the number of water layers
  box.numlayers <- rep(0, numboxes) # vector containing number of water layers
  for (i in 1: numboxes) {
    box.numlayers[i] <- sum(box.data$total.depth[i] > cum.depths)
  }
  box.numlayers[is.na(box.numlayers)] <- 0 # non-water boxes
  box.numlayers <- pmin(box.numlayers, max.numlayers) # bound by maximum depth
  box.data$numlayers <- box.numlayers # add the vector to box.data

  # calculate the depth of the deepest water layer (needed for volume)
  box.deepest.depth <- rep(NA, numboxes)
  max.layer.depth <- max(cum.depths)
  for (i in 1:numboxes) {
    if (box.data$numlayers[i] > 0) {
      box.deepest.depth[i] <- min(box.data$total.depth[i], max.layer.depth) -
        cum.depths[box.data$numlayers[i]]
    } else {
      box.deepest.depth[i] <- 0.0
    }
  }
  box.data$deepest.depth <- box.deepest.depth # add the vector to box.data

  # return a list of three objects: integer, data frame, data frame
  return(list(numboxes = numboxes, box.data = box.data))
}

# ==============================================================================
# generate.vars.init
# ==============================================================================
generate.vars.init <- function(grp.file, cum.depths, df.atts) {
  # read in group data from group csv file
  df.grp <- read.csv(file = grp.file, header = TRUE, stringsAsFactors = FALSE)
  # make sure GroupType column title exists
  col.titles <- names(df.grp)
  col.InvertType <- which(col.titles == "InvertType")
  if (!length(col.InvertType) == 0) {
    names(df.grp)[col.InvertType] <- "GroupType"
  }
  df.grp$GroupType <- as.character(df.grp$GroupType)
  # find epibenthos groups
  epi.grps.def <- c("SED_EP_FF", "SED_EP_OTHER", "EP_OTHER", "MOB_EP_OTHER",
    "MICROPHTYBENTHOS", "PHYTOBEN", "SEAGRASS", "CORAL")
  epi.grps <- df.grp$Name[df.grp$GroupType %in% epi.grps.def]
  df.grp <- df.grp %>% mutate(isEpiGrp = GroupType %in% epi.grps.def)

  # find cover groups. These groups need _cover in boxTracers
  cover.grps <- df.grp$Name[df.grp$IsCover == 1]

  # set up flags for groups that need multiple N values (e.g. _N1, _N2, ...)
  df.grp <- df.grp %>% mutate(multiN =
      (IsCover == 1       & (NumCohorts > 1)) |
      (GroupType == "PWN" & (NumCohorts > 1)) |
      (GroupType == "CEP" & (NumCohorts > 1)))

  # groups with nums, structural and reserve N values
  sr.grps <- c("FISH", "BIRD", "SHARK", "MAMMAL", "REPTILE", "FISH_INVERT")
  # set up flags for groups that need _Nums, _ResN, _StructN
  df.grp <- df.grp %>% mutate(needsNums = GroupType %in% sr.grps)

  # set up a flag for groups that need light adaptation
  light.adpn.grps <- c("DINOFLAG", "MICROPHTYBENTHOS", "SM_PHY",
    "MED_PHY", "LG_PHY")
  df.grp <- df.grp %>% mutate(needsLight = GroupType %in% light.adpn.grps)

  # create data frame for invert biological variables (ignores multiple stocks)
  Variable  <- NULL # variable name
  long_name <- NULL # long name
  att.index <- NULL # corresponding row of df.atts
  for (grp in 1:length(df.grp$Name)) {
    if (!df.grp$needsNums[grp]) {
      if (!df.grp$multiN[grp]) { # single group
        Variable  <- c(Variable, paste(df.grp$Name[grp], "_N", sep = ""))
        indx <- which(df.atts$name==paste(df.grp$GroupType[grp], "_N", sep = ""))
        long_name <- c(long_name, paste(df.grp$Name[grp],
          df.atts$long_name[indx], sep = " "))
        att.index <- c(att.index, indx)
      } else { # multiple groups
        for (j in 1:df.grp$NumCohorts[grp]) {
          Variable <- c(Variable, paste(df.grp$Name[grp], "_N", as.character(j),
            sep = ""))
          indx <- which(df.atts$name==paste(df.grp$GroupType[grp], "_N",
            sep = ""))
          long_name <- c(long_name, paste(df.grp$Name[grp], "cohort",
            as.character(j), df.atts$long_name[indx], sep = " "))
          att.index <- c(att.index, indx)
        }
      }
      if (df.grp$IsCover[grp]) { # single cover group
        Variable <- c(Variable, paste(df.grp$Name[grp], "_Cover", sep = ""))
        indx <- which(df.atts$name == "Cover")
        long_name <- c(long_name, paste("Percent cover by",
          df.grp$Name[grp], sep = " "))
        att.index <- c(att.index, indx)
      }
      if (df.grp$IsSiliconDep[grp]) { # single silicon group
        Variable <- c(Variable, paste(df.grp$Name[grp], "_S", sep = ""))
        indx <- which(df.atts$name == "Si3D")
        long_name <- c(long_name, paste(df.grp$Name[grp],
          "Silicon", sep = " "))
        att.index <- c(att.index, indx)
      }
      if (df.grp$needsLight[grp]) { # single light adaptation group
        Variable <- c(Variable, paste("Light_Adaptn_", df.grp$Code[grp],
          sep = ""))
        indx <- which(df.atts$name == "Light3D")
        long_name <- c(long_name, paste("Light adaption of",
          df.grp$Name[grp], sep = " "))
        att.index <- c(att.index, indx)
      }
    }
  }
  df.invert <- data.frame(Variable, long_name, att.index,
    stringsAsFactors = FALSE)

  # create data frame of vertebrate default variables (ignores multiple stocks)
  Variable  <- NULL # variable name
  long_name <- NULL # Long name
  att.index <- NULL # corresponding row of df.atts
  for (grp in 1:length(df.grp$Name)) {
    if (df.grp$needsNums[grp]) {
      Variable <- c(Variable, paste(df.grp$Name[grp], "_N", sep = ""))
      indx <- which(df.atts$name==paste(df.grp$GroupType[grp], "_N", sep = ""))
      long_name <- c(long_name, paste(df.grp$Name[grp],
        df.atts$long_name[indx], sep = " "))
      att.index <- c(att.index, indx)
      for (j in 1:df.grp$NumCohorts[grp]) {
          Variable <- c(Variable, paste(df.grp$Name[grp], as.character(j),
          "_Nums", sep = ""))
        indx <- which(df.atts$name=="Nums3D")
        long_name <- c(long_name, paste("Numbers of", df.grp$Name[grp], "cohort",
          as.character(j), sep = " "))
        att.index <- c(att.index, indx)
      }
      for (j in 1:df.grp$NumCohorts[grp]) {
        Variable <- c(Variable, paste(df.grp$Name[grp], as.character(j),
          "_StructN", sep = ""))
        indx <- which(df.atts$name=="StructN3D")
        long_name <- c(long_name, paste("Individual structural N for",
          df.grp$Name[grp], "cohort", as.character(j), sep = " "))
        att.index <- c(att.index, indx)
      }
      for (j in 1:df.grp$NumCohorts[grp]) {
        Variable <- c(Variable, paste(df.grp$Name[grp], as.character(j),
          "_ResN", sep = ""))
        indx <- which(df.atts$name=="ResN3D")
        long_name <- c(long_name, paste("Individual reserve N for",
          df.grp$Name[grp], "cohort", as.character(j), sep = " "))
        att.index <- c(att.index, indx)
      }
    }
  }

  df.vert <- data.frame(Variable, long_name, att.index, stringsAsFactors = FALSE)

  df.return <- rbind(df.invert, df.vert)
}

#' @title Function that generates two csv initial condition templates
#'
#' @description
#' Takes data from group file and box geometry file and generates two csv files that should be copied and modified.
#' The two files generated are called [csv.name]_init.csv and [csv.name]_horiz.csv.
#' Also required is the vector of cumulative depths, whos first element is 0.
#' [csv.name]_init.csv provides all the variables required in the .nc initial conditions file and their default attributes.
#' [csv.name]_horiz.csv provides box-specific values if the variable is set as customised in the [csv.name]_init file.
#' See also \code{\link[shinyrAtlantis]{make.init.nc}} for how these files are converted into a NetDF file.
#'
#' @param grp.file Atlantis group (.bgm) file that defines groups.
#' @param bgm.file Box geometry model (.bgm) file used by Atlantis that defines box boundaries and depths.
#' @param cum.depths vector of cumulative depths (starting with zero).
#' @param csv.name String that is used to identify the two output csv files.
#'
#' @return Null (always). Produces two csv files with the names \code{[csv.name]_init.csv} and \code{[csv.name]_horiz.csv}.
#'
#' @examples
#' \dontrun{
#' grp.file   <- "GBRGroups.csv"
#' bgm.file   <- "gbr_test.bgm"
#' cum.depths <- c(0,5,10,20,50,100,200,3000)
#' csv.name   <- "GBRtemplate"
#' make.init.csv(grp.file, bgm.file, cum.depths, csv.name)
#'
#' # copy GBRtemplate_init.csv to GBR_init.csv
#' # copy GBRtemplate_horiz.csv to GBR_horiz.csv
#' # edit files GBR_init.csv and GBR_horiz.csv by entering initial conditions
#'
#' init.file  <- "GBR_init.csv"
#' horiz.file <- "GBR_horiz.csv"
#' nc.file    <- "GBRtemplate.nc"
#' make.init.nc(bgm.file, cum.depths, init.file, horiz.file, nc.file)
#'
#' # view the initial conditions file
#' init.obj <- make.sh.init.object(bgm.file, nc.file)
#' sh.init(init.obj)
#' }
#' @export
#' @importFrom ncdf4 ncdim_def
make.init.csv <- function(grp.file, bgm.file, cum.depths, csv.name) {
  def.att.file <- system.file("extdata", "AttributeTemplate.csv", package = "shinyrAtlantis")
  df.atts <- read.csv(file = def.att.file, header = TRUE, stringsAsFactors = FALSE)

  numlayers <- length(cum.depths) - 1 # number of water layers
  # calculate the depths of each water layer
  layer.depth <- rep(0, numlayers)
  for (i in 1:numlayers){
    layer.depth[i] <- cum.depths[i+1] - cum.depths[i]
  }
  numsed <- 1 # default to a single sediment layer

  # extract data from the bgm file
  # numlayers, dz, nominal_dz, and volume must be calculated from this file
  map.data <- make.map.data.init(bgm.file, cum.depths)
  numboxes <- map.data$numboxes
  b.vals <- 1:numboxes
  z.vals <- 1:(numlayers + 1) # add single sediment layer
  box.data <- map.data$box.data

  # create a list of required variables
  df.return <- df.atts[df.atts$required, ]

  # add group-related variables
  grp.data <- generate.vars.init(grp.file, cum.depths, df.atts)
  num.vars <- dim(df.return)[1]
  for (i in 1:dim(grp.data)[1]) {
    num.vars <- num.vars + 1
    df.return <- rbind(df.return, df.atts[grp.data$att.index[i],])
    df.return$name[num.vars] <- grp.data$Variable[i]
    df.return$long_name[num.vars] <- grp.data$long_name[i]
  }

  write.csv(df.return, file = paste(csv.name, "_init.csv", sep = ""),
    row.names = FALSE)

  # create a template for custom horizontal distributions
  custom.vars <- df.return$name[df.return$wc.hor.pattern == "custom"]
  n.custom <- length(custom.vars)
  ma.vals <- matrix(data = 0, nrow = n.custom, ncol = numboxes)
  df.custom <- cbind(custom.vars, data.frame(ma.vals))
  names(df.custom) <- c("Variable",
    paste("box", as.character(0:(numboxes-1)), sep = ""))

  write.csv(df.custom, file = paste(csv.name, "_horiz.csv", sep = ""),
    row.names = FALSE)

  return (NULL)
}

# ==============================================================================
# make.init.nc
# ==============================================================================
#' @title Function that generates a NetCDF file of initial conditions
#'
#' @description
#' Takes data from two csv files and generates an Atlantis initial conditions NetCDF file.
#' The csv files should be generated using \code{\link[shinyrAtlantis]{make.init.csv}}.
#' \code{init.file} contains all variable names to be included in the NetCDF file and their attributes.
#' This file also contains initial conditions and their spatial distribution (e.g., surface only, uniformly distributed in the vertical) if they are not box-specific.
#' Box-specific values are specified in \code{horiz.file} and must be labelled as custom in \code{init.file}.
#'
#' @param bgm.file Box geometry model (.bgm) file used by Atlantis that defines box boundaries and depths.
#' @param cum.depths Vector of cumulative depths (starting with value zero).
#' @param init.file csv file containing all variable names and their attributes. Also includes how the values are distributed in space and the vertical.
#' @param horiz.file csv file containing box-defined values if customised flag is set for the horizontal distribution in \code{init.file}.
#' @param nc.file Name of the NetCDF file generated which contains the initial conditions. This file can be used as input to Atlantis.
#'
#' @return Null (always). Produces a NetCDF file with the name \code{nc.file}.
#'
#' @examples
#' \dontrun{
#' grp.file   <- "GBRGroups.csv"
#' bgm.file   <- "gbr_test.bgm"
#' cum.depths <- c(0,5,10,20,50,100,200,3000)
#' csv.name   <- "GBRtemplate"
#' make.init.csv(grp.file, bgm.file, cum.depths, csv.name)
#'
#' # copy GBRtemplate_init.csv to GBR_init.csv
#' # copy GBRtemplate_horiz.csv to GBR_horiz.csv
#' # edit files GBR_init.csv and GBR_horiz.csv by entering initial conditions
#'
#' init.file  <- "GBR_init.csv"
#' horiz.file <- "GBR_horiz.csv"
#' nc.file    <- "GBRtemplate.nc"
#' make.init.nc(bgm.file, cum.depths, init.file, horiz.file, nc.file)
#'
#' # view the initial conditions file
#' init.obj <- make.sh.init.object(bgm.file, nc.file)
#' sh.init(init.obj)
#' }
#' @export
make.init.nc <- function(bgm.file, cum.depths, init.file, horiz.file, nc.file, vert = NULL) {
  # nc file is created using the data stored in the following two csv files
  df.init <- df.grp <- read.csv(file = init.file, header = TRUE,
    stringsAsFactors = FALSE)
  df.horiz <- df.grp <- read.csv(file = horiz.file, header = TRUE,
    stringsAsFactors = FALSE)
    ## Transfor in double 0. for the ncfile
    df.init$b_dens <- as.double(df.init$b_dens)
    df.init$i_conc <- as.double(df.init$i_conc)
    df.init$f_conc <- as.double(df.init$f_conc)
  numlayers <- length(cum.depths) - 1 # number of water layers
  # calculate the depths of each water layer
  layer.depth <- rep(0, numlayers)
  for (i in 1:numlayers){
    layer.depth[i] <- cum.depths[i+1] - cum.depths[i]
  }
  numsed <- 1 # default to a single sediment layer
    if(!is.null(vert)){
        vert <- read.csv(vert)
    }
  # extract data from the bgm file
  # numlayers, dz, nominal_dz, and volume must be calculated from this file
  map.data <- make.map.data.init(bgm.file, cum.depths)
  numboxes <- map.data$numboxes
  b.vals <- 1:numboxes
  z.vals <- 1:(numlayers + 1) # add single sediment layer
  box.data <- map.data$box.data
  #land.box <- which(box.data$total.depth <= 0)

  # create dimensions stored in the NetCDF file
  dim1 <- ncdim_def( # create a time dimension
    name = 't',
    units = 'seconds since 1950-01-01 00:00:00 +10',
    unlim = TRUE,
    vals = as.double(0.0)
  )

  dim2 <- ncdim_def( # create a box dimension
    name = 'b',
    units = '(none)',
    vals = b.vals
  )

  dim3 <- ncdim_def( # create a depth layer dimension
    name = 'z',
    units = '(none)',
    vals = z.vals
  )

  # create a list of all variables
  vars <- NULL
  list.indx <- 1
  for (i in 1:dim(df.init)[1]) {
    var.name  <- df.init$name[i]
    var.units <- df.init$units[i]
    if (df.init$dimensions[i] == 1) {
      var.dim <- list(dim2, dim1)
    } else {
      var.dim <- list(dim3, dim2, dim1)
    }
    if(var.name == "nominal_dz"){
        var.dim <- list(dim3, dim2)
    }
    var.longname <- df.init$long_name[i]
    var.fillval  <- df.init$fill.value[i]
    vars[[list.indx]] <- ncvar_def(name = as.character(var.name),
      units = as.character(var.units), dim = var.dim,
      prec = ifelse(var.name %in% c("numlayers", "topk"), "short", "double"),
      longname = as.character(var.longname),
      missval = var.fillval)

    list.indx <- list.indx + 1
  }
  # writing to file will blow up if file already exists so make a copy
  if (file.exists(nc.file)) {
    file.remove(nc.file)
  }

  # create a NetCDF file
  outnc <- nc_create(filename = nc.file, vars = vars, force_v4 = TRUE)

  # add global attributes
  ncatt_put(nc = outnc, varid = 0, attname = 'geometry', attval = bgm.file)
  ncatt_put(nc = outnc, varid = 0, attname = 'wcnz', attval = numlayers, prec = "int")
  ncatt_put(nc = outnc, varid = 0, attname = 'sednz', attval = numsed, prec = "int")

  # add variable attributes (this can take a few minutes)
  for (i in 1:dim(df.init)[1]) {
      ## add required bmtype
    ncatt_put(nc = outnc, varid = df.init$name[i],
      attname = 'bmtype', attval = df.init$bmtype[i])

    # add the non-NA elements taken from df.atts
    for (j in 8:21) { # columns of df.atts with attributes
      if (!is.na(df.init[i,j])) {
        ncatt_put(nc = outnc, varid = df.init$name[i],
          attname = names(df.init)[j],
          attval = df.init[i,j])
      }
    }
  }
  nc_close(outnc)
  outnc <- nc_open(nc.file, write=TRUE) # open .nc file
  # create data based on the bgm file: volume, dz, nominal_dz, numlayers
  # add volume data (not quite matching Gladstone data but close - projection?)
  ma.volume <- matrix(data = 0, nrow = numlayers+1, ncol = numboxes)
  for (i in 1:numboxes) {
    if (box.data$is.island[i]) {
      # no water column volumes just a sediment layer volume
      ma.volume[numlayers + 1,i] <- box.data$volume[i]
    } else {
      # add sediment layer volume (assume depth = 1m)
      ma.volume[numlayers + 1,i] <- box.data$area[i]
      # add water column volumes
      if (box.data$numlayers[i] > 1) { # some full layers present
        for (j in 1:(box.data$numlayers[i] - 1)) {
          # j=1 = surface layer, j=numlayers = just above sediment
          ma.volume[box.data$numlayers[i] - j + 1,i] <-
            box.data$area[i]*layer.depth[j]
        }
      }
      # add the incomplete water layer just above the sediment
      ma.volume[1,i] <- box.data$area[i]*box.data$deepest.depth[i]
    }
  }
  ncvar_put(outnc, varid = "volume", vals = ma.volume)
    ##browser()
    ## add depth data
    ma.depth  <- matrix(data = 0, nrow = numlayers+1, ncol = numboxes)
    nom.depth <- matrix(data = 0, nrow = numlayers+1, ncol = numboxes) ## nominal depth
  for (i in 1:numboxes) {
    if (box.data$is.island[i]) {
      # no water column volumes just a sediment layer depth
      ma.depth[numlayers + 1,i] <- box.data$total.depth[i]
    } else {
      # add sediment layer depth (assumed to be depth = 1m)
      ma.depth[numlayers + 1,i] <- 1.0
      # add water column depths
      if (box.data$numlayers[i] > 1) { # some full layers present
        for (j in 1:(box.data$numlayers[i] - 1)) {
          # j=1 = surface layer, j=numlayers = just above sediment
          ma.depth[box.data$numlayers[i] - j + 1,i] <- layer.depth[j]
        }
      }
      # add the incomplete water layer just above the sediment
      ma.depth[1,i] <- box.data$deepest.depth[i]
      nom.depth[, i] <- ma.depth[, i]
        if(box.data$total.depth[i] > cum.depths[numlayers + 1]) {
            nom.depth[1, i] <- nom.depth[1, i] + (box.data$total.depth[i] - cum.depths[numlayers + 1])
        }
    }
  }
  ncvar_put(outnc, varid = "nominal_dz", vals = nom.depth)
  ncvar_put(outnc, varid = "dz", vals = ma.depth)

  # add numlayers data (calculated in box.data)
  ncvar_put(outnc, varid = "numlayers", vals = box.data$numlayers)
  ## information only by layer
    by.layer <- ifelse(nom.depth >= 1, 1, 0)

  # add data to required variables based on df.atts
  for (idx in 5:dim(df.init)[1]) { # four variables have already been calculated
    if (df.init$dimensions[idx] == 1) {
      # add the default value throughout (only for numlayers > 0?)
      var.data <- rep(df.init$wc.hor.scalar[idx], numboxes)
      # overwrite default value if custom
      if (df.init$wc.hor.pattern[idx] == "custom") {
        j <- which(df.horiz$Variable == df.init$name[idx])
        var.data <- df.horiz[j,2:(numboxes+1)]
      }
      ncvar_put(outnc, varid = df.init$name[idx], vals = var.data)
    } else {
      #var.data <- matrix(data = 0, nrow = numlayers+1, ncol = numboxes)
      var.data <- matrix(data = 1e30, nrow = numlayers+1, ncol = numboxes)
      hor.data <- rep(df.init$wc.hor.scalar[idx], numboxes) # default values
      # replace default values with custom values if provided
      if (df.init$wc.hor.pattern[idx] == "custom") {
        j <- which(df.horiz$Variable == df.init$name[idx])
        for (i in 1:numboxes) {
          hor.data[i] <- df.horiz[j,i+1]
        }
      }

      for (i in 1:numboxes) {
        if (box.data$numlayers[i] >= 1) { # some full layers present in box
          # add sediment default value
          var.data[numlayers + 1,i] <- as.double(df.init$sediment[idx])
          # j=1 = surface layer, j=numlayers = just above sediment
            if (df.init$wc.ver.pattern[idx] == "uniform") {
                for (j in 1:box.data$numlayers[i]) {
                    var.data[box.data$numlayers[i] - j + 1,i] <- hor.data[i]
                }
            } else if (df.init$wc.ver.pattern[idx] == "bottom") {
                for (j in 1:box.data$numlayers[i]) {
                    var.data[box.data$numlayers[i] - j + 1,i] <- 0.0
                }
                var.data[1,i] <- hor.data[i]
          } else if (df.init$wc.ver.pattern[idx] == "surface") {
              for (j in 1:box.data$numlayers[i]) {
                  var.data[box.data$numlayers[i] - j + 1,i] <- 0.0
              }
              var.data[box.data$numlayers[i],i] <- hor.data[i]
          } else if (df.init$wc.ver.pattern[idx] == "sediment"){
              for (j in 1:box.data$numlayers[i]) {
                  var.data[box.data$numlayers[i] - j + 1,i] <- 0.0
              }
              var.data[numlayers + 1, i] <- hor.data[i]
          } else if(df.init$wc.ver.pattern[idx] == "custom"){
              ver.d <- vert[, which(names(vert) %in%  df.init$name[idx])]
              for (j in 1:box.data$numlayers[i]) {
                  var.data[box.data$numlayers[i] - j + 1, i] <- hor.data[i]  * ver.d[j]
              }
          }
        }
      }
        var.data <- var.data * by.layer
        ncvar_put(outnc, varid = as.character(df.init$name[idx]), vals = var.data)
    }
  }

  nc_close(outnc)

  return (NULL)
}

# ==============================================================================
# get.init.nc : read the initial conditions of all variables in a NetCDF
#               file into a csv file.
# ==============================================================================
#' @title Function that generates a csv file of NetCDF values
#'
#' @description
#' Takes data from a NetCDF file and generates a csv file containing the box-specific values.
#' If the variable is distributed in the vertical then the value on the bottom water layer is returned.
#' This function is useful when collecting data from Atlantis models in order to parameterise new Atlantis models.
#' Incorrect output may be produced if the number of boxes equals the number of time steps or water layers.
#'
#' @param nc.file Name of the NetCDF file containing the initial conditions.
#' @param output.file Name of the csv file where data is written.
#'
#' @return Null (always). Produces a csv file with the name \code{ouput.file}.
#'
#' @examples
#' \dontrun{
#' nc.file <- "~/Atlantis/RunFiles/SEAP/params/initSEAPaquacult_pH.nc"
#' output.file <- "oldData.csv" # where to write the data
#'
#' get.init.nc(nc.file, output.file) # extract data from the NetCDF file
#' }
#' @export
get.init.nc <- function(nc.file, output.file) {
  nc.out <- nc_open(nc.file)       # open the NetCDF file
  n.vars <- nc.out$nvars           # number of variables in the NetCDF file
  var.names.all <- rep(NA, n.vars) # variable names
  for (i in 1:n.vars) { # find all variable names
    var.names.all[i] <- nc.out$var[[i]]$name # add variable name
  }

  max.cols <- 0 # maximum number of data per variable
  numboxes <- max(dim(ncvar_get(nc.out, "reef"))) # should be present

  numvars <- length(var.names.all)
  # first, store all initial data into a matrix
  m.data <- matrix(data = NA, nrow = numvars, ncol = numboxes)
  for (i in 1:length(var.names.all)) {
    data.all <- ncvar_get(nc.out, var.names.all[i]) # get the data
    indx <- which(dim(data.all) == numboxes) # which index has the data?
    data.out <- rep(0, numboxes) # reset vector where data is read in
    if (length(dim(data.all)) == 1) { # 1-D array
      data.out <- data.all
    } else if (length(dim(data.all)) == 2) { # 2-D array, data in indx
      if (indx == 1) {
        data.out <- data.all[ ,1]
      } else {
        data.out <- data.all[1, ]
      }
    } else if (length(dim(data.all)) == 3) { # 3-D array, data in indx
      if (indx == 1) {
        data.out <- data.all[ ,1,1]
      } else if (indx == 2) {
        data.out <- data.all[1, ,1]
      } else {
        data.out <- data.all[1,1, ]
      }
    }

    m.data[i,1:numboxes] <- data.out[1:numboxes] # transfer data to matrix
  }
  df.out <- data.frame(Variable = var.names.all, m.data) # make a data frame
  names(df.out) <- c("Variable",
    paste("box", as.character(0:(numboxes-1)), sep = "")) # name the columns
  write.csv(df.out, output.file) # write all the data to a csv file

  return(NULL)
}
