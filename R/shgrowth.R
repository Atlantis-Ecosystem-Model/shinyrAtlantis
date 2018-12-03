#' @title Shiny application for viewing Atlantis feeding and growth data
#'
#' @description
#' Takes data from an Atlantis group file (.csv), biology parameter file (.prm),
#' and initial conditions file (.nc) and produces visualisations of the vertebrate feeding data in the form of a shiny application.
#'
#' @param grp.file Atlantis group file (.csv).
#' @param prm.file Atlantis biological parameter file (.prm).
#' @param nc.file Initial conditions NetCDF file (.nc).
#'
#' @return Object of class 'shiny.appobj' see \code{\link[shiny]{shinyApp}}.
#'
#' @examples
#' \dontrun{
#' bgm.file <- "VMPA_setas.bgm"
#' nc.file  <- "INIT_VMPA_Jan2015.nc"
#' sh.init(grp.file, prm.file, nc.file)
#' }
#' @export
sh.feeding <- function(grp.file, prm.file, nc.file){

  # create HTML text for viewing on help tab
  txtHelp <- "<p>This shiny application displays some important feeding and growth parameters for vertebrate predators.</p>"
  txtHelp <- paste(txtHelp, "<p>Predictions are based on the assumption that in Atlantis predator growth is governed by the <b>type II functional response</b>.</p>")
  txtHelp <- paste(txtHelp, "<p>Predictions also assume that fish respiration has been turned off; <b>flagresp = 0</b>.</p>")
  txtHelp <- paste(txtHelp, "<p>Predicted growth curves assume that all cohorts are capable of spawning provided animals are above the minimal mass depicted in the <i>Weight tab</i>.</p>")
  txtHelp <- paste(txtHelp, "<p>Inclusion of invertebrates may occur in the future.</p>")

  dfs <- GenerateFeedingData(grp.file, prm.file, nc.file)
  df.Weight <- dfs$df.Group %>% select(Predator, Cohort, Reserve:Spawn) %>%
    gather(Type, Weight, Reserve:Spawn)
  df.Clearance <- dfs$df.Group %>% select(Predator, Cohort, Clearance)
  df.Growth <- dfs$df.Group %>% select(Predator, Cohort, mum, Require) %>%
    gather(Type, Rate, mum:Require)

  shinyApp(

    # USER INPUT FUNCTION

    ui = navbarPage(
      title = "Atlantis growth viewer",
      # Weight
      tabPanel("Weight",
        fluidPage(
          h5("Initial weight distribution: total (blue), reserve (red). Weight allocated to spawning is indicated in green."),
          plotOutput("plotWeight", height = "575px")
        )
      ),
      # Clearance
      tabPanel("Clearance",
        fluidPage(
          h5("Clearance rates provided in the biological parameter file."),
          plotOutput("plotClearance", height = "550px")
        )
      ),
      # Growth
      tabPanel("Growth rates",
        fluidPage(
          h5("Net growth rate needed to maintain initial condition mass (includes cost of spawning), and the maximum growth rate (mum)."),
          plotOutput("plotGrowth", height = "550px")
        )
      ),
      # Growth
      tabPanel("Growth curves",
        sidebarPanel(
          selectInput(inputId = 'SpGroup', label = 'Predator group',
            choices = dfs$df.grp$Name),
          htmlOutput("txtE"),
          width = 3
        ),
        mainPanel(
          fluidPage(
            h5("Predicted growth curves when feeding solely on each prey type, for a range of prey densities [mg N m-3]. Includes the cost of spawning."),
            plotOutput("plotPredict", height = "575px")
          )
        )
      ),
      tabPanel("Help",
        fluidPage(
          HTML(txtHelp)
        )
      ),
      tabPanel(actionButton("exitButton", "Exit"))
    ),

    # SERVER FUNCTION

    server = function(input, output, session) {
      values <- reactiveValues()
      values$txtE <- ""

      # register change in predator box
      observeEvent(input$SpGroup, {
        xxx <- input$SpGroup

        dfs.pred <- make.Predictions(dfs, xxx)
        levels(dfs.pred$df.plot$PreyType)[levels(dfs.pred$df.plot$PreyType)=="ELive"]  <- "Live"
        levels(dfs.pred$df.plot$PreyType)[levels(dfs.pred$df.plot$PreyType)=="EPlant"]  <- "Plant"
        levels(dfs.pred$df.plot$PreyType)[levels(dfs.pred$df.plot$PreyType)=="EDL"]  <- "Labile detritus"
        levels(dfs.pred$df.plot$PreyType)[levels(dfs.pred$df.plot$PreyType)=="EDR"]  <- "Refractory detritus"

        i <- which(dfs$df.grp$Name == xxx)
        values$txtE <- paste("<b>Assimilation fractions</b>",
          "<br>Labile: ", as.character(dfs$df.grp$EDL[i]),
          "<br>Refractory: ", as.character(dfs$df.grp$EDR[i]),
          "<br>Live: ", as.character(dfs$df.grp$ELive[i]),
          "<br>Plant: ", as.character(dfs$df.grp$EPlant[i]),
          "</p>", sep = "")

        output$plotPredict <- renderPlot({
          ggplot(dfs.pred$df.plot, aes(x = Cohort, y = Weight, color = NType)) +
            geom_line() +
            geom_point(data = dfs.pred$df.init) +
            xlab("Cohort") + ylab("Weight [mg N]") +
            labs(color = "Nitrogen\nsource") +
            facet_grid(NAvailable ~ PreyType, scales = "free_y") +
            theme_bw()
        })
      })

      output$txtE <- renderUI({
        HTML(values$txtE)
      })

      output$plotWeight <- renderPlot({
        ggplot(data = filter(df.Weight, Type != "Structural"),
            aes(x = Cohort, y = Weight, color = Type)) +
          geom_point() + geom_line() +
          facet_wrap( ~ Predator, scales = "free_y") + ylim(0,NA) +
          xlab("Cohort") + ylab("Weight [mg N]") +
          theme_bw()
      })

      output$plotClearance <- renderPlot({
        ggplot(data = df.Clearance, aes(x = Cohort, y = Clearance)) +
          geom_point() + geom_line() +
          facet_wrap( ~ Predator, scales = "free_y") + ylim(0,NA) +
          xlab("Cohort") + ylab("Clearance rate [m3 d-1]") +
          theme_bw()
      })

      output$plotGrowth <- renderPlot({
        ggplot(data = df.Growth, aes(x = Cohort, y = Rate, color = Type)) +
          geom_point() + geom_line() +
          facet_wrap( ~ Predator, scales = "free_y") +
          xlab("Cohort") + ylab("Growth rate [mg N d-1]") + # scale_y_log10() +
          theme_bw()
      })

      observeEvent(input$exitButton, {
        stopApp()
      })
    }
  )
}

GenerateFeedingData <- function(grp.file, prm.file, nc.file) {
  # == Group .csv file
  # read in group data from group .csv file
  df.grp <- read.csv(file = grp.file, header = TRUE,
    stringsAsFactors = FALSE)
  # make sure GroupType column title exists
  col.titles <- names(df.grp)
  col.InvertType <- which(col.titles == "InvertType")
  if (!length(col.InvertType) == 0) {
    names(df.grp)[col.InvertType] <- "GroupType"
  }

  # find vertebrate groups
  vert.grps <- c("FISH", "BIRD", "SHARK", "MAMMAL", "REPTILE")
  # set up flags for groups that need _Nums, _ResN, _StructN
  df.grp <- df.grp %>% mutate(isVertebrate = GroupType %in% vert.grps)
  df.grp <- filter(df.grp, isVertebrate) # only keep vertebrates
  numPredators <- dim(df.grp)[1] # number of predators
  # add additional columns that store species-specific parameters
  df.grp$NumCohortsPrm <- rep(NA, numPredators) # Num cohorts (a check)
  df.grp$KA            <- rep(NA, numPredators) # respiration param
  df.grp$KB            <- rep(NA, numPredators) # respiration param
  df.grp$ELive         <- rep(NA, numPredators) # assimilation efficiency
  df.grp$EPlant        <- rep(NA, numPredators) # assimilation efficiency
  df.grp$EDL           <- rep(NA, numPredators) # assimilation efficiency
  df.grp$EDR           <- rep(NA, numPredators) # assimilation efficiency
  df.grp$KSPA          <- rep(NA, numPredators) # spawning parameter
  df.grp$FSP           <- rep(NA, numPredators) # spawning parameter
  df.grp$pR            <- rep(NA, numPredators) # reserve/structural allocation

  # == Parameter file
  prm <- readLines(prm.file) # read in the biological parameter file

  # read in nonconsumer parameters
  to.find <- c("X_RS", "X_CN", "flagresp", "Kthresh2",
    "Ktmp_fish", "Ktmp_shark", "Ktmp_bird", "Ktmp_mammal", "Ktmp_reptile",
    "KST_fish", "KST_shark", "KST_bird", "KST_mammal", "KST_reptile")
  prm.vals <- rep(NA, length(to.find))
  i <- 0
  for (txt.find in to.find) {
    i <- i+1
    txt.find <- to.find[i]
    j <- grep(pattern = txt.find, x = prm, value = FALSE) # file row(s)
    if (length(j) > 0) { # found the parameter
      jnew <- NULL
      for (jj in 1:length(j)) {
        # Valid row is when txt.find is first entry and second is number
        text.split <- unlist(str_split(
          gsub(pattern = "[ ]+", x = prm[j[jj]], replacement = " "), " "))
        if (text.split[1] == txt.find) {
          jnew <- c(jnew,j[jj]) # add the row that satisfies the criteria
        }
      }
      j <- jnew # use this list of rows as they are valid
      if (length(j) == 1) { # a single row is found so valid
        # get parameter value (1 after nums)
        prm.vals[i] <- as.numeric(unlist(str_split(prm[j],"[\t ]+"))[2])
      }
    }
  }
  # store these parameters in a data frame
  df.prms <- data.frame(param = to.find, value = prm.vals)

  # read in consumer group parameters

  maxCohorts <- max(df.grp$NumCohorts) # upper size of matrices
  Rate.C <- matrix(data = NA, nrow = numPredators, ncol = maxCohorts)
  Rate.mum <- matrix(data = NA, nrow = numPredators, ncol = maxCohorts)
  Pr.FSPB <- matrix(data = NA, nrow = numPredators, ncol = maxCohorts)

  i <- 0 # predator index
    for (xxx in df.grp$Code) { # xxx = predator
        i <- i + 1 # new predator

    # KSPA: Find spawning parameter for xxx
    txt.find <- paste("KSPA_", xxx, sep = "")
    j <- grep(pattern = txt.find, x = prm, value = FALSE) # file row(s)
    if (length(j) > 0) { # found the parameter
      jnew <- NULL
      for (jj in 1:length(j)) {
        # Valid row is when txt.find is first entry and second is number
        text.split <- unlist(str_split(
          gsub(pattern = "[ \t]+", x = prm[j[jj]], replacement = " "), " "))
        if (text.split[1] == txt.find) {
          jnew <- c(jnew,j[jj]) # add the row that satisfies the criteria
        }
      }
      j <- jnew # use this list of rows as they are valid
      if (length(j) == 1) { # a single row is found so valid
        # get parameter value (1 after nums)
        p.val <- as.numeric(unlist(str_split(prm[j],"[\t ]+"))[2])
        df.grp$KSPA[i] <- p.val
      }
    }

    # pR: Find reserve/structural scaling parameter for xxx
    txt.find <- paste("pR_", xxx, sep = "")
    j <- grep(pattern = txt.find, x = prm, value = FALSE) # file row(s)
    if (length(j) > 0) { # found the parameter
      jnew <- NULL
      for (jj in 1:length(j)) {
        # Valid row is when txt.find is first entry and second is number
        text.split <- unlist(str_split(
          gsub(pattern = "[ \t]+", x = prm[j[jj]], replacement = " "), " "))
        if (text.split[1] == txt.find) {
          jnew <- c(jnew,j[jj]) # add the row that satisfies the criteria
        }
      }
      j <- jnew # use this list of rows as they are valid
      if (length(j) == 1) { # a single row is found so valid
        # get parameter value (1 after nums)
        p.val <- as.numeric(unlist(str_split(prm[j],"[\t ]+"))[2])
        df.grp$pR[i] <- p.val
      }
    }

    # FSP: Find spawning parameter for xxx
    txt.find <- paste("FSP_", xxx, sep = "")
    j <- grep(pattern = txt.find, x = prm, value = FALSE) # file row(s)
    if (length(j) > 0) { # found the parameter
      jnew <- NULL
      for (jj in 1:length(j)) {
        # Valid row is when txt.find is first entry and second is number
        text.split <- unlist(str_split(
          gsub(pattern = "[ \t]+", x = prm[j[jj]], replacement = " "), " "))
        if (text.split[1] == txt.find) {
          jnew <- c(jnew,j[jj]) # add the row that satisfies the criteria
        }
      }
      j <- jnew # use this list of rows as they are valid
      if (length(j) == 1) { # a single row is found so valid
        # get parameter value (1 after nums)
        p.val <- as.numeric(unlist(str_split(prm[j],"[\t ]+"))[2])
        df.grp$FSP[i] <- p.val
      }
    }

    # FSPB: Find spawning parameters for xxx
    txt.find <- paste("FSPB_", xxx, sep = "")
    j <- grep(pattern = txt.find, x = prm, value = FALSE) # file row(s)
    if (length(j) > 0) { # found the parameter
      jnew <- NULL
      for (jj in 1:length(j)) {
        # Valid row is when txt.find is first entry and second is number
        text.split <- unlist(str_split(
          gsub(pattern = "[ \t]+", x = prm[j[jj]], replacement = " "), " "))
        if (text.split[1] == txt.find) {
          jnew <- c(jnew,j[jj]) # add the row that satisfies the criteria
        }
      }
      j <- jnew # use this list of rows as they are valid
      if (length(j) == 1) { # a single row is found so valid
        # Clearance data is on the next row
        FSPB.vals <- na.omit(as.numeric(unlist(strsplit(prm[j+1],
          "[^0-9.]+"))))
        Pr.FSPB[i, 1:length(FSPB.vals)] <- FSPB.vals
      }
    }

    # Find clearance rates for xxx
    txt.find <- paste("C_", xxx, sep = "")
    j <- grep(pattern = txt.find, x = prm, value = FALSE) # file row(s)
    if (length(j) > 0) { # found the parameter
      jnew <- NULL
      for (jj in 1:length(j)) {
        # Valid row is when txt.find is first entry and second is number
        text.split <- unlist(str_split(
          gsub(pattern = "[ \t]+", x = prm[j[jj]], replacement = " "), " "))
        if (text.split[1] == txt.find) {
          jnew <- c(jnew,j[jj]) # add the row that satisfies the criteria
        }
      }
      j <- jnew # use this list of rows as they are valid
      if (length(j) == 1) { # a single row is found so valid
        # get parameter value (1 after nums)
        p.val <- as.numeric(unlist(str_split(prm[j],"[\t ]+"))[2])
        df.grp$NumCohortsPrm[i] <- p.val
                                        # Clearance data is on the next row
          C.vals <- na.omit(as.numeric(unlist(strsplit(prm[j+1], "[[:space:]]+"))))
          Rate.C[i, 1:length(C.vals)] <- C.vals
      }
    }

    # Find respiration constant for xxx
    txt.find <- paste("KA_", xxx, sep = "")
    j <- grep(pattern = txt.find, x = prm, value = FALSE) # file row(s)
    if (length(j) > 0) { # found the parameter
      jnew <- NULL
      for (jj in 1:length(j)) {
        # Valid row is when txt.find is first entry and second is number
        text.split <- unlist(str_split(
          gsub(pattern = "[ \t]+", x = prm[j[jj]], replacement = " "), " "))
        if (text.split[1] == txt.find) {
          jnew <- c(jnew,j[jj]) # add the row that satisfies the criteria
        }
      }
      j <- jnew # use this list of rows as they are valid
      if (length(j) == 1) { # a single row is found so valid
        # Clearance data is on the next row
        p.val <- as.numeric(unlist(str_split(prm[j],"[\t ]+"))[2])
        df.grp$KA[i] <- p.val
      }
    }

    # Find respiration constant for xxx
    txt.find <- paste("KB_", xxx, sep = "")
    j <- grep(pattern = txt.find, x = prm, value = FALSE) # file row(s)
    if (length(j) > 0) { # found the parameter
      jnew <- NULL
      for (jj in 1:length(j)) {
        # Valid row is when txt.find is first entry and second is number
        text.split <- unlist(str_split(
          gsub(pattern = "[ \t]+", x = prm[j[jj]], replacement = " "), " "))
        if (text.split[1] == txt.find) {
          jnew <- c(jnew,j[jj]) # add the row that satisfies the criteria
        }
      }
      j <- jnew # use this list of rows as they are valid
      if (length(j) == 1) { # a single row is found so valid
        # Clearance data is on the next row
        p.val <- as.numeric(unlist(str_split(prm[j],"[\t ]+"))[2])
        df.grp$KB[i] <- p.val
      }
    }
    # Find maximum growth rates for xxx
    txt.find <- paste("mum_", xxx, sep = "")
    j <- grep(pattern = txt.find, x = prm, value = FALSE) # file row(s)
    if (length(j) > 0) { # found the parameter
      jnew <- NULL
      for (jj in 1:length(j)) {
        # Valid row is when txt.find is first entry and second is number
        text.split <- unlist(str_split(
          gsub(pattern = "[ \t]+", x = prm[j[jj]], replacement = " "), " "))
        if (text.split[1] == txt.find) {
          jnew <- c(jnew,j[jj]) # add the row that satisfies the criteria
        }
      }
      j <- jnew # use this list of rows as they are valid
      if (length(j) == 1) { # a single row is found so valid
        # Clearance data is on the next row
          #mum.vals <- na.omit(as.numeric(unlist(strsplit(prm[j+1], "[^0-9.]+"))))
          mum.vals <- na.omit(as.numeric(unlist(strsplit(prm[j+1], "[[:space:]]+"))))
        Rate.mum[i, 1:length(mum.vals)] <- mum.vals
      }
    }

    # Find live prey efficiency for xxx
    txt.find <- paste("E_", xxx, sep = "")
    j <- grep(pattern = txt.find, x = prm, value = FALSE) # file row(s)
    if (length(j) > 0) { # found the parameter
      jnew <- NULL
      for (jj in 1:length(j)) {
        # Valid row is when txt.find is first entry and second is number
        text.split <- unlist(str_split(
          gsub(pattern = "[ \t]+", x = prm[j[jj]], replacement = " "), " "))
        if (text.split[1] == txt.find) {
          jnew <- c(jnew,j[jj]) # add the row that satisfies the criteria
        }
      }
      j <- jnew # use this list of rows as they are valid
      if (length(j) == 1) { # a single row is found so valid
        # get parameter value (1 after nums)
        p.val <- as.numeric(unlist(str_split(prm[j],"[\t ]+"))[2])
        df.grp$ELive[i] <- p.val
      }
    }

    # Find plant prey efficiency for xxx
    txt.find <- paste("EPlant_", xxx, sep = "")
    j <- grep(pattern = txt.find, x = prm, value = FALSE) # file row(s)
    if (length(j) > 0) { # found the parameter
      jnew <- NULL
      for (jj in 1:length(j)) {
        # Valid row is when txt.find is first entry and second is number
        text.split <- unlist(str_split(
          gsub(pattern = "[ \t]+", x = prm[j[jj]], replacement = " "), " "))
        if (text.split[1] == txt.find) {
          jnew <- c(jnew,j[jj]) # add the row that satisfies the criteria
        }
      }
      j <- jnew # use this list of rows as they are valid
      if (length(j) == 1) { # a single row is found so valid
        # get parameter value (1 after nums)
        p.val <- as.numeric(unlist(str_split(prm[j],"[\t ]+"))[2])
        df.grp$EPlant[i] <- p.val
      }
    }

    # Find labile detritus prey efficiency for xxx
    txt.find <- paste("EDL_", xxx, sep = "")
    j <- grep(pattern = txt.find, x = prm, value = FALSE) # file row(s)
    if (length(j) > 0) { # found the parameter
      jnew <- NULL
      for (jj in 1:length(j)) {
        # Valid row is when txt.find is first entry and second is number
        text.split <- unlist(str_split(
          gsub(pattern = "[ \t]+", x = prm[j[jj]], replacement = " "), " "))
        if (text.split[1] == txt.find) {
          jnew <- c(jnew,j[jj]) # add the row that satisfies the criteria
        }
      }
      j <- jnew # use this list of rows as they are valid
      if (length(j) == 1) { # a single row is found so valid
        # get parameter value (1 after nums)
        p.val <- as.numeric(unlist(str_split(prm[j],"[\t ]+"))[2])
        df.grp$EDL[i] <- p.val
      }
    }

    # Find refractory detritus prey efficiency for xxx
    txt.find <- paste("EDR_", xxx, sep = "")
    j <- grep(pattern = txt.find, x = prm, value = FALSE) # file row(s)
    if (length(j) > 0) { # found the parameter
      jnew <- NULL
      for (jj in 1:length(j)) {
        # Valid row is when txt.find is first entry and second is number
        text.split <- unlist(str_split(
          gsub(pattern = "[ \t]+", x = prm[j[jj]], replacement = " "), " "))
        if (text.split[1] == txt.find) {
          jnew <- c(jnew,j[jj]) # add the row that satisfies the criteria
        }
      }
      j <- jnew # use this list of rows as they are valid
      if (length(j) == 1) { # a single row is found so valid
        # get parameter value (1 after nums)
        p.val <- as.numeric(unlist(str_split(prm[j],"[\t ]+"))[2])
        df.grp$EDR[i] <- p.val
      }
    }
  }

  df.grp <- mutate(df.grp, minSpawnWt = KSPA/FSP) # minimum spawning weight

  # == NetCDF file
  N.Struct <- matrix(data = NA, nrow = numPredators, ncol = maxCohorts)
  N.Res <- matrix(data = NA, nrow = numPredators, ncol = maxCohorts)

  nc.out <- nc_open(nc.file) # open .nc file
  i <- 0
  for (xxx in df.grp$Name) { # look for each Code
    i <- i + 1 # xxx index

    for (j in 1:df.grp$NumCohorts[i]) {
      StructN <- ncvar_get(nc.out, paste(xxx, as.character(j), "_StructN",
        sep = "")) # get all variable data
      if (length(dim(StructN)) == 3) {
        StructN <- StructN[ , ,1] # remove time variable
      }
      if (prod(is.na(StructN))) { # all are NA so use fillValue
        N.Struct[i,j] <- ncatt_get(nc.out, varid = paste(xxx,
          as.character(j), "_StructN", sep = ""),
          attname = "_FillValue")$value
      } else { # not all are NA
        N.Struct[i,j] <- max(StructN, na.rm=TRUE)
      }

      ResN <- ncvar_get(nc.out, paste(xxx, as.character(j), "_ResN",
        sep = "")) # get all variable data
      if (length(dim(ResN)) == 3) {
        ResN <- ResN[ , ,1] # remove time variable
      }
      if (prod(is.na(ResN))) { # all are NA so use fillValue
        N.Res[i,j] <- ncatt_get(nc.out, varid = paste(xxx, as.character(j),
          "_ResN", sep = ""),
          attname = "_FillValue")$value
      } else { # not all are NA
        N.Res[i,j] <- max(ResN, na.rm=TRUE)
      }
    }
  }
  nc_close(nc.out)

  X_RS <- df.prms$value[which(df.prms$param == "X_RS")] # usually 2.65

  Predator <- NULL
  PreyGroup <- NULL
  Cohort <- NULL

  Predator.G <- NULL
  Cohort.G   <- NULL

  mum <- NULL
  Require <- NULL
  Clearance  <- NULL

  Frac.Spawn  <- NULL
  Spawn     <- NULL

  NStruct <- NULL
  NRes    <- NULL
  NTotal  <- NULL

  W <- N.Struct + N.Res # Total nitrogen weight
  i <- 0
  # calculate required available biomass for each of the four groups
  for (xxx in df.grp$Name) { # look for each Code (Predator)
    i <- i + 1 # xxx index
    # Live Prey
    # for (j in 1:(df.grp$NumCohorts[i]-1)) {
    for (j in 1:df.grp$NumCohorts[i]) { # cohort
      Predator  <- c(Predator, xxx)
      Cohort    <- c(Cohort, j)

      C    <- Rate.C[i,j]     # clearance rate [mg N d-1]
      G    <- Rate.mum[i,j]   # maximum growth rate [mg N d-1]
      FSPB <- Pr.FSPB[i,j]    # spawning fraction
      E    <- df.grp$ELive[i] # uptake efficiency
      if (is.null(df.grp$NumAgeClassSize)) { # add column and set to 1
        df.grp <- mutate(df.grp, NumAgeClassSize = 1.0)
      }
      tau  <- 365*df.grp$NumAgeClassSize[i] # days per cohort

      # Calculate some growth
      Predator.G <- c(Predator.G, xxx) # predator
      Cohort.G   <- c(Cohort.G, j) # cohort
      mum        <- c(mum, G) # maximum growth rate [mg N d-1]
      Clearance  <- c(Clearance, C) # clearance rate [m3 d-1]

      if (j < df.grp$NumCohorts[i]) {
        dW  <- W[i,j+1] - W[i,j]
      } else {
        dW <- 0
      }
      # TO DO: calculate spawning cost Nr and incorporate into Require
      FSP     <- df.grp$FSP[i]
      KSPA    <- df.grp$KSPA[i]
      WIdeal  <- (1.0 + X_RS) * N.Struct[i,j] # ideal weight
      LowWghtCost <- min(0, WIdeal - W[i,j]) # underweight loss in Res N
      spwn <- max(0.0, FSP*(WIdeal - KSPA/FSP) - LowWghtCost) # mean reserve loss per spawner
      Spawn <- c(Spawn, spwn)

      # calculate daily growth requirement with spawning fraction
      Require <- c(Require, (dW + FSPB*spwn)/tau) # required daily growth [mg N d-1]

      Frac.Spawn <- c(Frac.Spawn, FSPB) # fraction spawning

      NStruct    <- c(NStruct, N.Struct[i,j]) # stuctural nitrogen [mg N]
      NRes       <- c(NRes, N.Res[i,j]) # stuctural nitrogen [mg N]
      NTotal     <- c(NTotal, W[i,j]) # stuctural nitrogen [mg N]
    }
  }

  df.Group <- data.frame(Predator = Predator.G, Cohort = Cohort.G,
    mum = mum, Clearance = Clearance, Require = Require,
    Frac.Spawn = Frac.Spawn, Reserve = NRes, Structural = NStruct,
    WghtTotal = NTotal, Spawn = Spawn)
    return(list(df.grp = df.grp, df.Group = df.Group, df.prms = df.prms))
}

make.Predictions <- function(dfs, xxx) {
  # xxx is the predator code

  # create assimilation efficiencies data frame
  df.E <- tidyr::gather(select(dfs$df.grp, Name, ELive:EDR),
    PreyType, Fraction, ELive:EDR)

  # get the species-specific data
  Tau  <- dfs$df.grp$NumAgeClassSize[which(dfs$df.grp$Name == xxx)]
  FSP  <- dfs$df.grp$FSP[which(dfs$df.grp$Name == xxx)]
  KSPA <- dfs$df.grp$KSPA[which(dfs$df.grp$Name == xxx)]
  df.R <- filter(dfs$df.Group, Predator == xxx) %>%
    select(Cohort, Reserve)
  df.S <- filter(dfs$df.Group, Predator == xxx) %>%
    select(Cohort, Structural)

  df.Group <- filter(dfs$df.Group, Predator == xxx)

  Cohorts <- dfs$df.grp$NumCohorts[which(dfs$df.grp$Name == xxx)]
  df.Group$ReserveEnd <- rep(0, Cohorts)
  df.Group$StructuralEnd <- rep(0, Cohorts)

  X_RS <- dfs$df.prm$value[which(dfs$df.prm$param == "X_RS")] # usually 2.65
  Cohort <- NULL
  PreyType <- NULL
  NAvailable <- NULL
  Reserve <- NULL
  Structural <- NULL

  # consider all four prey types
  for (yyy in c("ELive", "EPlant", "EDL", "EDR")) {
    E <- filter(df.E, PreyType == yyy, Name == xxx)$Fraction[1]
    # consider three nitrogen densities
    for (N.Available in c(0.01, 0.05, 0.1, 0.5, 1.0, 5.0, 10.0, 50.0)) {
      NR <- df.R$Reserve[1] # initial reserve weight
      NS <- df.S$Structural[1] # initial structural weight

      Cohort <- c(Cohort, 1)
      PreyType <- c(PreyType, yyy)
      NAvailable <- c(NAvailable, N.Available)
      Reserve <- c(Reserve, NR)
      Structural <- c(Structural, NS)

      for (chrt in 1:(Cohorts-1)) {
        B <- N.Available # available nitrogen [mg N m-3]
        C <- df.Group$Clearance[chrt] # clearance rate [m3 d-1]
        G <- df.Group$mum[chrt] # maximum growth rate  [mg N d-1]
        fs <- df.Group$Frac.Spawn[chrt] # spawning fraction [1]
        for (ageclass in 1:Tau) {
          # == Spawn
          # calculate reserve nitrogen used for spawning for each cohort, Nr
          WIdeal <- (1 + X_RS)*NS # ideal weight
          Nr <- max(FSP*WIdeal - KSPA,0) # reserve nitrogen for spawning
          LowWghtCost <- max(0, WIdeal - NR - NS) # underweight loss in Res N
          Nr <- fs*max(Nr - LowWghtCost, 0.0) # mean reserve loss per spawner
          # annual gain in nitrogen though eating
          Ngain <- 365*C*E*B/(1.0 + C*E*B/G)
          # Remove mean nitrogen loss through spawning as it needs to be replenished before growth
          Ngain <- max(Ngain - Nr, 0.0) # cannot lose weight over the year
          # update total N pool
          NTotal <- NR + NS + Ngain
          # redistribute R and S for ideal weighted animal
          NR <- NTotal * X_RS / (1.0 + X_RS)
          NS <- NTotal * 1.0 / (1.0 + X_RS)
        }
        # store final cohort nitrogen values
        Cohort <- c(Cohort, chrt+1)
        PreyType <- c(PreyType, yyy)
        NAvailable <- c(NAvailable, N.Available)
        Reserve <- c(Reserve, NR)
        Structural <- c(Structural, NS)
      }
    }
  }

  df.Predict <- data.frame(Cohort = Cohort, PreyType = PreyType,
    NAvailable = NAvailable, Reserve = Reserve, Structural = Structural)

  df.plot <- tidyr::gather(df.Predict, NType, Weight, Reserve:Structural)

  df.init <- dfs$df.Group %>% filter(Predator == xxx) %>%
    select(Cohort, Reserve, Structural) %>%
    gather(NType, Weight, Reserve:Structural)

  return(list(df.plot = df.plot, df.init = df.init))
}
