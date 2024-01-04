# shforce.R
# 02/05/2016

#' @title Shiny application for viewing Atlantis forcings data
#'
#' @description
#'A shiny application that displays water exchanges, salinity and temperature data
#' from the netCDF files used as forcings for an Atlantis run.
#'
#' To run this application a list object must first be generated using
#' \code{\link[shinyrAtlantis]{make.sh.forcings.object}} (see Examples).
#'
#' The \emph{Connections} tab displays the number of neighbouring water layers that each
#' water layer in a chosen box exchanges water. Use this tab to check that all water layers in
#' boxes exchange with at least one other water layer.
#'
#' The \emph{Exchanges} tab allows the user to display the exchanges read into Atlantis
#' (Raw) and all exchanges associated with a box (All). Atlantis only reads in a
#' single exchange value associated with a pair of box layers, however this exchange
#' influences flow for both box layers. Use the (All) tab to see all flows
#' associated with a box. An option is available to plot either
#' raw exchange values or their sign, and whether box numbering is displayed.
#' Exchanges are also presented in a table.
#'
#' The \emph{Horizontal flows} tab provides visualisation of the horizontal flow field at
#' each water layer. This flow is \emph{approximate} as it is calculated
#' using only exchanges at a single layer, however some horizontal flow may occur
#' from horizontal exchanges at the other layers. The length of the arrow only provides
#' a guide to flow speed.
#'
#' The \emph{Vertical flows} tab provides net vertical fluxes for each water layer in
#' a box. An option is available to plot either
#' raw exchange values or their sign, and whether box numbering is displayed.
#' Exchanges (per unit area per time step) are also presented in a table.
#'
#' The \emph{Time-series} tab allows the user to examine all water exchanges from a
#' specified water layer. As vertical exchanges are often much greater than horizontal
#' exchanges an option is available to suppress plotting of vertical exchanges.
#' This plot can take some time to generate so a checkbox is provided to
#' enable the plot.
#'
#' The \emph{Temperature} and \emph{Salinity} tabs provide spatial and time-series
#' plots. Tabular versions of the data are also presented.
#'
#' @param input.object An R list object generated from \code{\link[shinyrAtlantis]{make.sh.forcings.object}}.
#'
#' @return An object of class 'shiny.appobj' see \code{\link[shiny]{shinyApp}}.
#'
#' @examples
#' \dontrun{
#' exchange.file    <- "GBR108_hydro.nc"
#' salinity.file    <- "GBR108_salt.nc"
#' temperature.file <- "GBR108_temp.nc"
#' bgm.file         <- "gbr_box_03012012.bgm"
#' cum.depth <- c(0,5,10,20,50,100,200,3000) # cumulative water layer depths
#'
#' input.object <- make.sh.forcings.object(
#'   bgm.file         = bgm.file,
#'   exchange.file    = exchange.file,
#'   cum.depth        = cum.depth,
#'   temperature.file = temperature.file,
#'   salinity.file    = salinity.file
#' )
#' sh.forcings(input.object)
#' }
#' @export
#' @importFrom ggplot2 facet_wrap geom_point geom_segment geom_line annotate scale_fill_gradient2 scale_color_manual arrow element_text
#' @importFrom dplyr left_join select
#' @importFrom grid unit
sh.forcings <- function(input.object){
  # create HTML text for viewing on help tab
  txtHelp <- "<h3>Summary</h3>"
  txtHelp <- paste(txtHelp, "<p>This program displays data from the .nc files used to provide the time-series of forcings for an <b>Atlantis</b> run.</p>")
  txtHelp <- paste(txtHelp, "<h3>Details</h3>")
  txtHelp <- paste(txtHelp, "<p>Plots have a zoom feature. Draw a box and double click to zoom into the box. Double click to reset zoom.</p>")
  txtHelp <- paste(txtHelp, "<p>There are two numberings of layers. Atlantis assumes layer 0 is immediately above the sediment, which means that Atlantis layers do not correspond between boxes that differ in the number of water layers. For plotting the surface layer is always set to 1 and layers correspond with depth from the surface.</p>")
  txtHelp <- paste(txtHelp, "<p>For the exchange plots gold cells depict the focal box and focal layer provided by the user.</p>")
  txtHelp <- paste(txtHelp, "<p>For the connections plot gold cells depict box layers that are not linked to any other boxes. Check that these boxes are not isolated throughout the time series.</p>")
  txtHelp <- paste(txtHelp, "<p>Plotting the time-series can be slow so it is initially disabled. It is best to disable plotting the time series when investigating other aspects of the exchange data.</p>")
  txtHelp <- paste(txtHelp, "<p>The exhange tabs allow plotting of the raw exchange values or the sign of the exchanges. Plotting the signs rather than the raw values helps to show which layers are connected.</p>")
  txtHelp <- paste(txtHelp, "<p>Note that the 2-D flows are approximate and should only be used as a guide of flow direction.</p>")

  dt = input.object$t[2]-input.object$t[1] # time step

  # extract elements from input parameter
  numboxes    <- input.object$numboxes
  numlayers   <- input.object$numlayers
  numdests    <- input.object$numdests
  numtimes    <- input.object$numtimes
  time        <- input.object$t
  box.data    <- input.object$box.data
  box.data$numlayers[is.na(box.data$numlayers)] <- 0
  cum.depth   <- input.object$cum.depth

  # create side bar text describing depth layers
  txt.depths <- "<h5>Layer depths</h5><p>"
  for (i in 2:length(cum.depth)) {
    txt.depths <- paste(txt.depths, as.character(i-1), ": ",
      as.character(cum.depth[i]), "m<br>", sep = "")
  }
  txt.depths <- paste(txt.depths, "</p>", sep = "")

  # create useful global arrays
  exchange.links <- array(0, dim = c(numboxes, numlayers,
    numboxes, numlayers))
  # exchange.value: [i1,j1,i2,j2] = net flow from (i1,j1) to (i2,j2)
  exchange.value <- array(NA, dim = c(numboxes, numlayers, numboxes, numlayers))
  exchange.TS <- array(NA, dim = c(numtimes, numboxes, numlayers))
  vert.dir <- array(NA, dim = c(numboxes, numlayers))

  # calculate the depths of each water layer
  layer.depth <- rep(0, numlayers)
  for (i in 1:(length(cum.depth)-1)){
    layer.depth[i] <- cum.depth[i+1] - cum.depth[i]
  }

  # create a mapping from plotting layer to Atalantis layer
  plot.to.Atlantis <- array(0, dim = c(numboxes, numlayers))
  for (i in 1:numboxes) {
    for (j in 1:numlayers) {
      plot.to.Atlantis[i,j] <- box.data$numlayers[i]-j+1
      if (plot.to.Atlantis[i,j] < 1) {
        plot.to.Atlantis[i,j] <- numlayers + plot.to.Atlantis[i,j]
      }
    }
  }

  # estimate total box face lengths that connect pairs of boxes
  min.length <- array(0, dim = c(numboxes, numboxes))
  for (i in 1:numboxes) {
    for (j in 1:numboxes) {
       min.length[i,j] <- min(sqrt(box.data$area[i]), sqrt(box.data$area[j]))
    }
  }

  plotHeight3D <- paste(as.character(250 * ((numlayers - 1) %/% 3 + 1)),
    "px", sep = "") # calculate a reasonable overall plot size for 3D plots

  shinyApp(

    # USER INPUT FUNCTION

    ui = navbarPage(
      title = "Atlantis forcings viewer",
      # -- Connections --
      tabPanel("Connections",
        sidebarLayout(
          sidebarPanel(width = 2,
            numericInput(inputId = "NI.Connections", label = "Time (days)",
              min = dt, max = max(input.object$t),
              value = input.object$t[1], step = dt),
            checkboxInput(inputId = "CI.BoxIDConnections", label = "Show box IDs",
              value = FALSE)
          ),
          mainPanel(
            plotOutput("plotConnections",
              height = plotHeight3D,
              dblclick = "plotConnections_dblclick",
              brush = brushOpts(
                id = "plotConnections_brush",
                resetOnNew = TRUE
              )
            ),
            HTML("<p>Number of box layers connected with the focal box layer. Plot layer 1 is the surface layer. Gold cells do not exchange water with any other boxes. Grey cells do not contain water.</p>"),
            hr(),
            DT::dataTableOutput("table.Connections")
          )
        )
      ),
      # -- Exchanges (raw values) --
      navbarMenu("Exchanges",
        tabPanel("Raw",
          sidebarLayout(
            sidebarPanel(width = 2,
              selectInput(inputId = "SI.Box", label = "Focal box",
                choices = 0:(numboxes-1)),
              selectInput(inputId = "SI.Level", label = "Plotted layer\n(surface = layer 1)",
                choices = 1:box.data$numlayers[1]),
              numericInput(inputId = "NI.Time", label = "Time (days)",
                min = min(input.object$t), max = max(input.object$t),
                value = input.object$t[1], step = dt),
              checkboxInput(inputId = "CI.Raw", label = "Plot raw values",
                value = FALSE),
              checkboxInput(inputId = "CI.BoxID", label = "Show box IDs",
                value = FALSE)
            ),
            mainPanel(
              fluidRow(
                plotOutput("plotExchange",
                  height = plotHeight3D,
                  dblclick = "plotExchange_dblclick",
                  brush = brushOpts(
                    id = "plotExchange_brush",
                    resetOnNew = TRUE
                  )
                )
              ),
              HTML("<p>Positive values indicate flow from the focal (gold) cell to the destination cell, and vice-versa. Numbers in the panel headers indicate the plotted depth layer (layer 1 is the surface layer).</p>"),
              hr(),
              DT::dataTableOutput("table.Exchange")
            )
          )
        ),
        # -- Exchanges (all values) --
        tabPanel("All",
          sidebarLayout(
            sidebarPanel(width = 2,
              selectInput(inputId = "SI.Box.all", label = "Focal box",
                choices = 0:(numboxes-1)),
              selectInput(inputId = "SI.Level.all", label = "Plotted layer\n(surface = layer 1)",
                choices = 1:box.data$numlayers[1]),
              numericInput(inputId = "NI.Time.all", label = "Time (days)",
                min = min(input.object$t), max = max(input.object$t),
                value = input.object$t[1], step = dt),
              checkboxInput(inputId = "CI.Raw.all", label = "Plot raw values",
                value = FALSE),
              checkboxInput(inputId = "CI.BoxID.all", label = "Show box IDs",
                value = FALSE)
            ),
            mainPanel(
              plotOutput("plotExchange.all",
                height = plotHeight3D,
                dblclick = "plotExchange.all_dblclick",
                brush = brushOpts(
                  id = "plotExchange.all_brush",
                  resetOnNew = TRUE
                )
              ),
              HTML("<p>Positive values indicate flow from the focal (gold) cell to the destination cell, and vice-versa. Flow is from red to gold to blue.</p><p>Numbers in the panel headers indicate the plotted depth layer (layer 1 is the surface layer).</p>"),
              hr(),
              DT::dataTableOutput("table.Exchange.all")
            )
          )
        )
      ),
      # -- Horizontal flows --
      tabPanel("Horizontal flows",
        sidebarLayout(
          sidebarPanel(width = 2,
            numericInput(inputId = "NI.Flows", label = "Time (days)",
              min = dt, max = max(input.object$t),
              value = input.object$t[1], step = dt),
            checkboxInput(inputId = "CI.BoxIDFlows", label = "Show box IDs",
              value = FALSE)
          ),
          mainPanel(
            HTML("<p><b>Please be patient as these plots may take some time to generate.</b></p>"),
            plotOutput("plotFlows",
              height = plotHeight3D,
              dblclick = "plotFlows_dblclick",
              brush = brushOpts(
                id = "plotFlows_brush",
                resetOnNew = TRUE
              )
            ),
            HTML("<p>Directions of horizontal flow are only approximate. Layer 1 is the surface layer.</p><p>Flows in and out of the deepest layer of box 0 are ignored as it is a dummy location.</p>")
          )
        )
      ),
      # -- Vertical flows --
      tabPanel("Vertical flows",
        sidebarLayout(
          sidebarPanel(width = 2,
            numericInput(inputId = "NI.Vert", label = "Time (days)",
              min = dt, max = max(input.object$t),
              value = input.object$t[1], step = dt),
            checkboxInput(inputId = "CI.Vert", label = "Plot raw values",
              value = FALSE),
            checkboxInput(inputId = "CI.BoxIDVert", label = "Show box IDs",
              value = FALSE)
          ),
          mainPanel(
            plotOutput("plotVert",
              height = plotHeight3D,
              dblclick = "plotVert_dblclick",
              brush = brushOpts(
                id = "plotVert_brush",
                resetOnNew = TRUE
              )
            ),
            HTML("<p>Layer 1 is the surface layer. Positive values indicate upward flow.</p>"),
            hr(),
            DT::dataTableOutput("table.Vert")
          )
        )
      ),
      # -- Time Series --
      tabPanel("Time-series",
        sidebarLayout(
          sidebarPanel(width = 2,
            checkboxInput(inputId = "CI.Enable", label = "Enable plotting",
              value = FALSE),
            selectInput(
              inputId = "SI.BoxTS",
              label   = "Focal box",
              choices = 0:(input.object$numboxes-1)
            ),
            selectInput(
              inputId = "SI.LevelTS",
              label = "Plotted layer\n(surface = layer 1)",
              choices = 1:box.data$numlayers[1]
            ),
            checkboxInput(
              inputId = "CI.ExcludeVert",
              label = "Exclude vertical",
              value = FALSE
            )
          ),
          mainPanel(
            HTML("<p><b>Please be patient as this plot may take some time to generate.</b></p>"),
            plotOutput("plotTimeSeries",
              height = "450px",
              dblclick = "plotTimeSeries_dblclick",
              brush = brushOpts(
                id = "plotTimeSeries_brush",
                resetOnNew = TRUE
              )
            ),
            HTML("<p>Positive values indicate that exchange is out of the focal box. Layer 1 is the surface layer. Horizontal exchanges can often be resolved by removing the vertical exchanges from the plot.</p>")
          )
        )
      ),
      # -- Temperature --
      navbarMenu("Temperature",
        tabPanel("Spatial",
          sidebarLayout(
            sidebarPanel(width = 2,
              numericInput(inputId = "NI.Temp", label = "Time (days)",
                min = dt, max = max(input.object$t),
                value = input.object$t[1], step = dt),
              checkboxInput(inputId = "CI.BoxIDTemp", label = "Show box IDs",
                value = FALSE)
            ),
            mainPanel(
              plotOutput("plotTemp",
                height = plotHeight3D,
                dblclick = "plotTemp_dblclick",
                brush = brushOpts(
                  id = "plotTemp_brush",
                  resetOnNew = TRUE
                )
              ),
              hr(),
              DT::dataTableOutput("table.Temp")
            )
          )
        ),
        tabPanel("Temporal",
          sidebarLayout(
            sidebarPanel(width = 2,
              selectInput(inputId = "SI.TempTS", label = "Focal box",
                choices = 0:(numboxes-1), selected = 0),
              h5("Box attributes"),
              htmlOutput("txtTempAtt")
            ),
            mainPanel(
              plotOutput("plotTempTS",
                height = "450px",
                dblclick = "plotTempTS_dblclick",
                brush = brushOpts(
                  id = "plotTempTS_brush",
                  resetOnNew = TRUE
                )
              ),
              HTML("<p>Layer 1 is the surface layer. Bottom water layer and sediment layer may have the same temperature.</p>"),
              hr(),
              DT::dataTableOutput("table.TempTS")
            )
          )
        )
      ),
      # -- Salinity --
      navbarMenu("Salinity",
        tabPanel("Spatial",
          sidebarLayout(
            sidebarPanel(width = 2,
              numericInput(inputId = "NI.Salt", label = "Time (days)",
                min = dt, max = max(input.object$t),
                value = input.object$t[1], step = dt),
              checkboxInput(inputId = "CI.BoxIDSalt", label = "Show box IDs",
                value = FALSE)
            ),
            mainPanel(
              plotOutput("plotSalt",
                height = plotHeight3D,
                dblclick = "plotSalt_dblclick",
                brush = brushOpts(
                  id = "plotSalt_brush",
                  resetOnNew = TRUE
                )
              ),
              hr(),
              DT::dataTableOutput("table.Salt")
            )
          )
        ),
        tabPanel("Temporal",
          sidebarLayout(
            sidebarPanel(width = 2,
              selectInput(inputId = "SI.SaltTS", label = "Focal box",
                choices = 0:(numboxes-1), selected = 0),
              h5("Box attributes"),
              htmlOutput("txtSaltAtt")
            ),
            mainPanel(
              plotOutput("plotSaltTS",
                height = "450px",
                dblclick = "plotSaltTS_dblclick",
                brush = brushOpts(
                  id = "plotSaltTS_brush",
                  resetOnNew = TRUE
                )
              ),
              HTML("<p>Layer 1 is the surface layer. Bottom water layer and sediment layer may have the same salinity.</p>"),
              hr(),
              DT::dataTableOutput("table.SaltTS")
            )
          )
        )
      ),
      # -- Help --
      tabPanel("Help",
        fluidPage(
          HTML(txtHelp)
        )
      ),
      # -- Exit --
      tabPanel(
        actionButton("exitButton", "Exit")
      )
    ),

    # SERVER FUNCTION

    server = function(input, output, session) {
      values <- reactiveValues()
      values$TempAtt <- ""
      values$SaltAtt <- ""

      # reactive variables used to set plot ranges
      rangesExchange     <- reactiveValues(x = NULL, y = NULL)
      rangesExchange.all <- reactiveValues(x = NULL, y = NULL)
      rangesTimeSeries   <- reactiveValues(x = NULL, y = NULL)
      rangesConnections  <- reactiveValues(x = NULL, y = NULL)
      rangesFlows        <- reactiveValues(x = NULL, y = NULL)
      rangesVert         <- reactiveValues(x = NULL, y = NULL)
      rangesTemp         <- reactiveValues(x = NULL, y = NULL)
      rangesSalt         <- reactiveValues(x = NULL, y = NULL)

      # -- Update sub-selections
      observe({
        i <- as.integer(input$SI.Box)
        updateSelectInput(session, "SI.Level",
          choices = 1:box.data$numlayers[i+1])
      })

      observe({
        i <- as.integer(input$SI.Box.all)
        updateSelectInput(session, "SI.Level.all",
          choices = 1:box.data$numlayers[i+1])
      })

      observe({
        i <- as.integer(input$SI.BoxTS)
        updateSelectInput(session, "SI.LevelTS",
          choices = 1:box.data$numlayers[i+1])
      })

      # -- Validate user entries
      observe({
        if (input$NI.Time < min(input.object$t) |
            input$NI.Time > max(input.object$t)) {
          updateNumericInput(session, "NI.Time",
            value = input.object$t[1])
        }
      })

      observe({
        if (input$NI.Time.all < min(input.object$t) |
            input$NI.Time.all > max(input.object$t)) {
          updateNumericInput(session, "NI.Time.all",
            value = input.object$t[1])
        }
      })

      # -- register brush events for zooming in and out
      observeEvent(input$plotExchange_dblclick, {
        brush <- input$plotExchange_brush
        if (!is.null(brush)) {
          rangesExchange$x <- c(brush$xmin, brush$xmax)
          rangesExchange$y <- c(brush$ymin, brush$ymax)
        } else {
          rangesExchange$x <- NULL
          rangesExchange$y <- NULL
        }
      })

      observeEvent(input$plotExchange.all_dblclick, {
        brush <- input$plotExchange.all_brush
        if (!is.null(brush)) {
          rangesExchange.all$x <- c(brush$xmin, brush$xmax)
          rangesExchange.all$y <- c(brush$ymin, brush$ymax)
        } else {
          rangesExchange.all$x <- NULL
          rangesExchange.all$y <- NULL
        }
      })

      observeEvent(input$plotConnections_dblclick, {
        brush <- input$plotConnections_brush
        if (!is.null(brush)) {
          rangesConnections$x <- c(brush$xmin, brush$xmax)
          rangesConnections$y <- c(brush$ymin, brush$ymax)
        } else {
          rangesConnections$x <- NULL
          rangesConnections$y <- NULL
        }
      })

      observeEvent(input$plotFlows_dblclick, {
        brush <- input$plotFlows_brush
        if (!is.null(brush)) {
          rangesFlows$x <- c(brush$xmin, brush$xmax)
          rangesFlows$y <- c(brush$ymin, brush$ymax)
        } else {
          rangesFlows$x <- NULL
          rangesFlows$y <- NULL
        }
      })

      observeEvent(input$plotVert_dblclick, {
        brush <- input$plotVert_brush
        if (!is.null(brush)) {
          rangesVert$x <- c(brush$xmin, brush$xmax)
          rangesVert$y <- c(brush$ymin, brush$ymax)
        } else {
          rangesVert$x <- NULL
          rangesVert$y <- NULL
        }
      })

      observeEvent(input$plotTimeSeries_dblclick, {
        brush <- input$plotTimeSeries_brush
        if (!is.null(brush)) {
          rangesTimeSeries$x <- c(brush$xmin, brush$xmax)
          rangesTimeSeries$y <- c(brush$ymin, brush$ymax)
        } else {
          rangesTimeSeries$x <- NULL
          rangesTimeSeries$y <- NULL
        }
      })

      observeEvent(input$plotTemp_dblclick, {
        brush <- input$plotTemp_brush
        if (!is.null(brush)) {
          rangesTemp$x <- c(brush$xmin, brush$xmax)
          rangesTemp$y <- c(brush$ymin, brush$ymax)
        } else {
          rangesTemp$x <- NULL
          rangesTemp$y <- NULL
        }
      })

      observeEvent(input$plotSalt_dblclick, {
        brush <- input$plotSalt_brush
        if (!is.null(brush)) {
          rangesSalt$x <- c(brush$xmin, brush$xmax)
          rangesSalt$y <- c(brush$ymin, brush$ymax)
        } else {
          rangesSalt$x <- NULL
          rangesSalt$y <- NULL
        }
      })

      # register change in box
      observeEvent(input$SI.TempTS, {
        focal.box <- as.integer(input$SI.TempTS)
        values$TempAtt <- paste("<p>Water layers: ",
          as.character(box.data$numlayers[focal.box+1]), "</p><p>",
          "Depth: ", as.character(-box.data$z[focal.box+1]), "m</p>", txt.depths,
          sep = "")
      })

      observeEvent(input$SI.SaltTS, {
        focal.box <- as.integer(input$SI.SaltTS)
        values$SaltAtt <- paste("<p>Water layers: ",
          as.character(box.data$numlayers[focal.box+1]), "</p><p>",
          "Depth: ", as.character(-box.data$z[focal.box+1]), "m</p>", txt.depths,
          sep = "")
      })

      # display box attributes for temperature time series
      output$txtTempAtt <- renderUI({
        HTML(values$TempAtt)
      })

      # display box attributes for temperature time series
      output$txtSaltAtt <- renderUI({
        HTML(values$SaltAtt)
      })

      # -- Connections -- Plot
      output$plotConnections <- renderPlot({
        # calculate the time index of the data matrices
        t <- round((input$NI.Connections - input.object$t[1]) / dt, digits = 0) + 1

        exchange.links[ , , , ] <- 0

        for (i in 1:numboxes) {
          for (j in 1:numlayers) {
            for (k in 1:numdests) {
              if (!is.na(input.object$exchange[k,j,i,t])) {
                dest.i <- input.object$dest.box[k,j,i,t] + 1
                dest.j <- input.object$dest.layer[k,j,i,t] + 1
                if (!is.na(dest.i) & !is.na(dest.j)) {
                  if (!((i == dest.i) & (j == dest.j))) { # only consider adjacent boxes
                    exchange.links[i,j,dest.i,dest.j] <- exchange.links[i,j,dest.i,dest.j] + 1
                    exchange.links[dest.i,dest.j,i,j] <- exchange.links[dest.i,dest.j,i,j] + 1
                  }
                }
              }
            }
          }
        }

        total.links <- array(0, dim = c(numboxes, numlayers))
        for (i in 1:numboxes) {
          for (j in 1:numlayers) { # surface to bottom
            total.links[i,j] <- sum(exchange.links[i,j, , ], na.rm = TRUE)
          }
        }

        df <- data.frame(boxid = 0:(numboxes-1), numlayers = box.data$numlayers,
          total.links)
        layer.names <- rep("x", numlayers)
        for (i in 1:numlayers) {
          layer.names[i] <- paste("l", as.character(i-1), sep = "")
        }

        names(df)[3:(numlayers+2)] <- layer.names
        tmp <- numlayers+2
        df <- tidyr::gather(df, layer, links, 3:tmp)
        df$layer <- sort(rep(0:(numlayers-1), numboxes))

        df$dest.plot.layer <- df$numlayers - df$layer
        df$dest.plot.layer[df$dest.plot.layer < 1] <-
          numlayers + df$dest.plot.layer[df$dest.plot.layer < 1]
        df$links[df$layer >= df$numlayers] <- NA

        df.plot <- dplyr::left_join(input.object$map.vertices, df, by = "boxid")
        df.unconnected <- df.plot %>% dplyr::filter(is.na(links))


        gg <- ggplot(data = df.plot,
          aes(x = x, y = y, group = boxid, fill = links)) +
          geom_polygon(colour = "grey25", size = 0.25, na.rm = TRUE) +
          scale_fill_gradient(low = "white", high = "#de2d26", na.value="grey", limits = c(0,max(df.plot$links))) +
          labs(fill = "Connecting\nbox\nlayers") +
          facet_wrap( ~ dest.plot.layer, ncol = 3) +
          theme_bw() + xlab("") + ylab("") +
          theme(plot.background = element_blank()) +
          scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL) +
          coord_cartesian(xlim = rangesConnections$x, ylim = rangesConnections$y)
        gg <- gg + geom_polygon(data = df.unconnected, mapping = aes(x = x, y = y, group = boxid), inherit.aes = FALSE, fill = "gold", size = 0.25, na.rm = TRUE)
        if (input$CI.BoxIDConnections) { # add box id
          gg <- gg + geom_text(data = input.object$box.data, mapping = aes(x = x.in, y = y.in, label = boxid), size = 2.5, inherit.aes = FALSE)
        }
        gg
      })

      # -- Connections -- Table
      output$table.Connections <- DT::renderDataTable({
        # calculate the time index of the data matrices
        t <- round((input$NI.Connections - input.object$t[1]) / dt, digits = 0) + 1

        exchange.links[ , , , ] <- 0

        for (i in 1:numboxes) {
          for (j in 1:numlayers) {
            for (k in 1:numdests) {
              if (!is.na(input.object$exchange[k,j,i,t])) {
                dest.i <- input.object$dest.box[k,j,i,t] + 1
                dest.j <- input.object$dest.layer[k,j,i,t] + 1
                if (!is.na(dest.i) & !is.na(dest.j)) {
                  if (!((i == dest.i) & (j == dest.j))) { # only consider adjacent boxes
                    exchange.links[i,j,dest.i,dest.j] <- exchange.links[i,j,dest.i,dest.j] + 1
                    exchange.links[dest.i,dest.j,i,j] <- exchange.links[dest.i,dest.j,i,j] + 1
                  }
                }
              }
            }
          }
        }

        total.links <- array(0, dim = c(numboxes, numlayers))
        for (i in 1:numboxes) {
          for (j in 1:numlayers) { # surface to bottom
            total.links[i,j] <- sum(exchange.links[i,j, , ], na.rm = TRUE)
          }
        }

        df <- data.frame(boxid = 0:(numboxes-1), numlayers = box.data$numlayers,
          total.links)
        layer.names <- rep("x", numlayers)
        for (i in 1:numlayers) {
          layer.names[i] <- paste("l", as.character(i-1), sep = "")
        }

        names(df)[3:(numlayers+2)] <- layer.names
        tmp <- numlayers+2
        df <- tidyr::gather(df, layer, links, 3:tmp)
        df$layer <- sort(rep(0:(numlayers-1), numboxes))

        df$dest.plot.layer <- df$numlayers - df$layer
        df$dest.plot.layer[df$dest.plot.layer < 1] <-
          numlayers + df$dest.plot.layer[df$dest.plot.layer < 1]
        df$links[df$layer >= df$numlayers] <- NA
        df <- df %>% dplyr::filter(!is.na(links))
        df <- df[c(1,2,3,5,4)] # reorder columns

        DT::datatable(df, rownames = FALSE,
          colnames = c('box', 'water layers', 'layer (Atlantis)', 'layer (plotting)', 'number of links'),
          caption = "Number of linked box layers excluding self exchanges.")
      })

      # -- Exchange -- Plot
      output$plotExchange <- renderPlot({
        focal.box   <- as.integer(input$SI.Box) # 1:numboxes
        focal.plot.layer <- as.integer(input$SI.Level) # 1 is surface
        # convert plotting layer to Atlantis layer
        focal.Atlantis.layer <- box.data$numlayers[focal.box+1] -
          focal.plot.layer # 0 is the layer above the sediment
        t <- round(as.integer((input$NI.Time - time[1]) / dt), digits = 0) + 1

        tmp.exchange <- input.object$exchange[ , focal.Atlantis.layer+1,
          focal.box+1, t]
        keep <- which(!is.na(tmp.exchange))
        tmp.exchange <- tmp.exchange[keep]

        if (!input$CI.Raw) {
          tmp.exchange <- sign(tmp.exchange) # remove magnitude
        }

        tmp.dest.box <- input.object$dest.box[ , focal.Atlantis.layer+1,
          focal.box+1, t][keep]

        tmp.dest.layer <- input.object$dest.layer[ , focal.Atlantis.layer+1,
          focal.box+1, t][keep]

        tmp.numlayers <- box.data$numlayers[keep]

        boxid <- rep(0:(numboxes-1), numlayers)
        layer <- sort(rep(1:numlayers, numboxes)) - 1
        exch <- rep(0, length(boxid))

        df <- data.frame(boxid, numlayers = rep(box.data$numlayers, numlayers),
          dest.Atlantis.layer = layer, exch)

        for (i in 1:length(tmp.exchange)) {
          indx <- tmp.dest.layer[i]*numboxes + tmp.dest.box[i] + 1
          df$exch[indx] <- tmp.exchange[i]
        }
        df$dest.plot.layer <- df$numlayers - df$dest.Atlantis.layer
        df$dest.plot.layer[df$dest.plot.layer < 1] <-
          numlayers +
          df$dest.plot.layer[df$dest.plot.layer < 1]
        df$exch[df$dest.plot.layer > df$numlayers] <- NA

        df.plot <- dplyr::left_join(input.object$map.vertices, df, by = "boxid")
        df.plot$boxid <- factor(df.plot$boxid)

        df.focal <- dplyr::filter(df.plot,
            boxid == focal.box & dest.plot.layer == focal.plot.layer) %>%
          select(boxid, dest.plot.layer, x, y)

        gg <- ggplot(data = df.plot,
          aes(x = x, y = y, group = boxid, fill = exch)) +
          geom_polygon(colour = "black", size = 0.25, na.rm = TRUE) +
          scale_fill_gradient2(low = "#e6550d", high = "#3182bd", mid = "white", midpoint=0, na.value="grey75") +
          labs(fill = "value") +
          facet_wrap( ~ dest.plot.layer, ncol = 3) +
          theme_bw() + xlab("") + ylab("") +
          coord_cartesian(xlim = rangesExchange$x, ylim = rangesExchange$y) +
          theme(plot.background = element_blank()) +
          scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL)
        gg <- gg + geom_polygon(data = df.focal, mapping = aes(x = x, y = y, group = boxid), inherit.aes = FALSE, fill = "gold", size = 0.25, na.rm = TRUE)
        if (input$CI.BoxID) { # add box ids
          gg <- gg + geom_text(data = box.data, mapping = aes(x = x.in, y = y.in, label = boxid), size = 2.5)
        }
        gg
      })

      # -- Exchange -- Table
      output$table.Exchange <- DT::renderDataTable({
        focal.box   <- as.integer(input$SI.Box) # 1:numboxes
        focal.plot.layer <- as.integer(input$SI.Level) # 1 is surface
        # convert plotting layer to Atlantis layer
        focal.Atlantis.layer <- box.data$numlayers[focal.box+1] -
          focal.plot.layer # 0 is the layer above the sediment
        t <- round(as.integer((input$NI.Time - time[1]) / dt), digits = 0) + 1

        tmp.exchange <- input.object$exchange[ , focal.Atlantis.layer+1,
          focal.box+1, t]
        keep <- which(!is.na(tmp.exchange))
        tmp.exchange <- tmp.exchange[keep]

        tmp.dest.box <- input.object$dest.box[ , focal.Atlantis.layer+1,
          focal.box+1, t][keep]

        tmp.dest.layer <- input.object$dest.layer[ , focal.Atlantis.layer+1,
          focal.box+1, t][keep]

        tmp.numlayers <- box.data$numlayers[tmp.dest.box+1]

        df.Exchange <- data.frame(dest.box = tmp.dest.box,
          numlayers = tmp.numlayers,
          dest.Atlantis.layer = tmp.dest.layer,
          dest.plot.layer = tmp.numlayers - tmp.dest.layer,
          Exchange = tmp.exchange)

        DT::datatable(df.Exchange, rownames = FALSE,
          colnames = c('box', 'water layers', 'layer (Atlantis)', 'layer (plotting)',
            'volume exchanged (m^3)'))
      })

      # -- Exchange (all) -- Plot
      output$plotExchange.all <- renderPlot({
        focal.box <- as.integer(input$SI.Box.all) # 1:numboxes
        focal.plot.layer <- as.integer(input$SI.Level.all) # 1 is surface
        # convert plotting layer to Atlantis layer
        focal.Atlantis.layer <- box.data$numlayers[focal.box+1] -
          focal.plot.layer # 0 is the layer above the sediment
        t <- round(as.integer((input$NI.Time.all - time[1]) / dt), digits = 0) + 1

        exchange.value[ , , , ] <- 0.0

        for (i in 1:numboxes) {
          for (j in 1:numlayers) {
            for (k in 1:numdests) {
              if (!is.na(input.object$exchange[k,j,i,t])) {
                dest.i <- input.object$dest.box[k,j,i,t] + 1
                dest.j <- input.object$dest.layer[k,j,i,t] + 1
                if (!is.na(dest.i) & !is.na(dest.j)) {
                  exchange.value[i,j,dest.i,dest.j] <-
                    input.object$exchange[k,j,i,t] # from i to dest
                  exchange.value[dest.i,dest.j,i,j] <-
                    -input.object$exchange[k,j,i,t] # from dest to i
                }
              }
            }
          }
        }

        if (!input$CI.Raw.all) {
          exchange.value <- sign(exchange.value) # remove magnitude
        }

        boxid <- rep(0:(numboxes-1), numlayers)
        layer <- sort(rep(1:numlayers, numboxes)) - 1
        exch <- rep(0, length(boxid))

        df <- data.frame(boxid, numlayers = box.data$numlayers, layer,
          dest.Atlantis.layer = layer, exch)
        for (i in 1:dim(exchange.value)[3]) { # destination box + 1
          for (j in 1:dim(exchange.value)[4]) { # destination layer + 1
            if (!is.na(exchange.value[focal.box+1,focal.Atlantis.layer+1,i,j])) {
              indx <- i + numboxes*(j-1)
              df$exch[indx] <- exchange.value[focal.box+1,
                focal.Atlantis.layer+1,i,j]
            }
          }
        }

        df$dest.plot.layer <- df$numlayers - df$dest.Atlantis.layer
        df$dest.plot.layer[df$dest.plot.layer < 1] <-
          numlayers + df$dest.plot.layer[df$dest.plot.layer < 1]
        df$exch[df$dest.plot.layer > df$numlayers] <- NA

        df.plot <- dplyr::left_join(input.object$map.vertices, df, by = "boxid")
        df.plot$boxid <- factor(df.plot$boxid)

        df.focal <- dplyr::filter(
          df.plot,
          boxid == focal.box, 
          dest.plot.layer == focal.plot.layer
        ) %>%
          dplyr::select(boxid, dest.plot.layer, x, y)

        gg <- ggplot(data = df.plot,
          aes(x = x, y = y, group = boxid, fill = exch)) +
          geom_polygon(colour = "grey25", size = 0.25, na.rm = TRUE) +
          scale_fill_gradient2(low = "#e6550d", high = "#3182bd", mid = "white", midpoint=0, na.value="grey75") +
          labs(fill = "value") +
          facet_wrap( ~ dest.plot.layer, ncol = 3) +
          theme_bw() + xlab("") + ylab("") +
          coord_cartesian(xlim = rangesExchange.all$x, ylim = rangesExchange.all$y) +
          theme(plot.background = element_blank()) +
          scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL)
        gg <- gg + geom_polygon(data = df.focal, mapping = aes(x = x, y = y, group = boxid), inherit.aes = FALSE, fill = "gold", size = 0.25, na.rm = TRUE)
        if (input$CI.BoxID.all) { # add box id
          gg <- gg + geom_text(data = input.object$box.data, mapping = aes(x = x.in, y = y.in, label = boxid), size = 2.5)
        }
        gg
      })

      # -- Exchange (all) -- Table
      output$table.Exchange.all <- DT::renderDataTable({
        focal.box <- as.integer(input$SI.Box.all) # 1:numboxes
        focal.plot.layer <- as.integer(input$SI.Level.all) # 1 is surface
        # convert plotting layer to Atlantis layer
        focal.Atlantis.layer <- box.data$numlayers[focal.box+1] -
          focal.plot.layer # 0 is the layer above the sediment
        t <- round(as.integer((input$NI.Time.all - time[1]) / dt), digits = 0) + 1

        exchange.value[ , , , ] <- 0.0

        for (i in 1:numboxes) {
          for (j in 1:numlayers) {
            for (k in 1:numdests) {
              if (!is.na(input.object$exchange[k,j,i,t])) {
                dest.i <- input.object$dest.box[k,j,i,t] + 1
                dest.j <- input.object$dest.layer[k,j,i,t] + 1
                if (!is.na(dest.i) & !is.na(dest.j)) {
                  exchange.value[i,j,dest.i,dest.j] <-
                    input.object$exchange[k,j,i,t] # from i to dest
                  exchange.value[dest.i,dest.j,i,j] <-
                    -input.object$exchange[k,j,i,t] # from dest to i
                }
              }
            }
          }
        }

        boxid <- rep(0:(numboxes-1), numlayers)
        layer <- sort(rep(1:numlayers, numboxes)) - 1
        exch <- rep(0, length(boxid))

        df <- data.frame(boxid, numlayers = box.data$numlayers,
          dest.Atlantis.layer = layer, dest.plot.layer = layer, exch)
        for (i in 1:dim(exchange.value)[3]) { # destination box + 1
          for (j in 1:dim(exchange.value)[4]) { # destination layer + 1
            if (!is.na(exchange.value[focal.box+1,focal.Atlantis.layer+1,i,j])) {
              indx <- i + numboxes*(j-1)
              df$exch[indx] <- exchange.value[focal.box+1,
                focal.Atlantis.layer+1,i,j]
            }
          }
        }

        df$dest.plot.layer <- df$numlayers - df$dest.Atlantis.layer
        df$dest.plot.layer[df$dest.plot.layer < 1] <-
          numlayers + df$dest.plot.layer[df$dest.plot.layer < 1]
        df$exch[df$dest.plot.layer > df$numlayers] <- NA

        DT::datatable(dplyr::filter(df, exch != 0), rownames = FALSE,
          colnames = c('box', 'water layers', 'layer (Atlantis)', 'layer (plotting)', 'volume exchanged (m^3)'))
      })

      # -- Time-series -- plot
      output$plotTimeSeries <- renderPlot({
        if (input$CI.Enable) {
          focal.box <- as.integer(input$SI.BoxTS) # 1:numboxes
          focal.plot.layer <- as.integer(input$SI.LevelTS) # 1 is surface
          # convert plotting layer to Atlantis layer
          focal.Atlantis.layer <- box.data$numlayers[focal.box+1] -
            focal.plot.layer # 0 is the layer above the sediment

          # dest.layer[dest.layer < 0] <- NA
          # dest.layer[dest.layer >= numlayers] <- NA

          for (ts in 1:numtimes) {
            exchange.value[ , , ,] <- NA
            for (i in 1:numboxes) {
              for (j in 1:numlayers) {
                for (k in 1:numdests) {
                  if (!is.na(input.object$exchange[k,j,i,ts])) {
                    dest.i <- input.object$dest.box[k,j,i,ts] + 1
                    dest.j <- input.object$dest.layer[k,j,i,ts] + 1
                    if (!is.na(dest.i) & !is.na(dest.j)) {
                      exchange.value[i,j,dest.i,dest.j] <-
                        input.object$exchange[k,j,i,ts] # from i to dest
                      exchange.value[dest.i,dest.j,i,j] <-
                        -input.object$exchange[k,j,i,ts] # from dest to i
                    }
                  }
                }
              }
            }
            exchange.TS[ts, , ] <- exchange.value[focal.box+1, focal.Atlantis.layer+1, , ]
          }

          ts <- NULL # input.object$t
          box.layer <- NULL
          exch <- NULL
          for (i in 1:numboxes) {
            for (j in 1:numlayers) { # j-1 = Atlantis layer
              if (!all(is.na(exchange.TS[ ,i,j]))) {
                if (!input$CI.ExcludeVert) {
                  box.level <- paste(i-1,box.data$numlayers[i]-(j-1),sep = ".")
                  ts <- c(ts, input.object$t)
                  box.layer <- c(box.layer, rep(box.level,numtimes))
                  exch <- c(exch, exchange.TS[ ,i,j])

                } else {
                  if (i != (focal.box+1)) {
                    box.level <- paste(i-1,box.data$numlayers[i]-(j-1),sep = ".")
                    ts <- c(ts, input.object$t)
                    box.layer <- c(box.layer, rep(box.level,numtimes))
                    exch <- c(exch, exchange.TS[ ,i,j])
                  }
                }
              }
            }
          }
          df.plot <- data.frame(ts, box.layer, exch)
          df.plot <- df.plot[complete.cases(df.plot),]

          ggplot(data = df.plot, aes(x = ts, y = exch, color = box.layer)) +
            geom_point(size = 0.35) + geom_line() +
            xlab("Time (days)") + ylab("Exchange (m^3)") +
          coord_cartesian(xlim = rangesTimeSeries$x, ylim = rangesTimeSeries$y) +
          theme_bw()
        } else {
          df.plot <- data.frame(x = 0, y = 0)
          ggplot(data = df.plot, aes(x = x, y = y)) +
            theme_bw() + xlab("") + ylab("") +
            annotate("text", x = 0, y = 0, size = 5, colour = "red",
              label = "Plot is not enabled.\nCheck the enable box to plot.\nNote that it may take a few minutes to generate the plot.") +
            theme(
              plot.background = element_blank(),
              plot.title=element_text(colour="red")
            ) +
            scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL)
        }
      })

      # -- Horizontal flows -- Plot
      output$plotFlows <- renderPlot({
        t <- round(as.integer((input$NI.Flows - time[1]) / dt), digits = 0) + 1

        exchange.value <- array(0.0, dim = c(numboxes, numlayers,
          numboxes, numlayers))

        for (i in 1:numboxes) {
          for (j in 1:numlayers) {
            for (k in 1:numdests) {
              if (!is.na(input.object$exchange[k,j,i,t])) {
                dest.i <- input.object$dest.box[k,j,i,t] + 1
                dest.j <- input.object$dest.layer[k,j,i,t] + 1
                if (!is.na(dest.i) & !is.na(dest.j)) {
                  exchange.value[i,j,dest.i,dest.j] <-
                    input.object$exchange[k,j,i,t] /
                    (layer.depth[j]*min.length[i,dest.i]) # from i to dest
                  exchange.value[dest.i,dest.j,i,j] <-
                    -input.object$exchange[k,j,i,t] /
                    (layer.depth[j]*min.length[i,dest.i]) # from dest to i
                }
              }
            }
          }
        }

        boxes <- 1:numboxes
        dx <- array(data = 0.0, dim = c(numboxes,numlayers))
        dy <- array(data = 0.0, dim = c(numboxes,numlayers))
        max.length <- 0.0
        for (i in boxes) {
          xf <- box.data$x.in[i]
          yf <- box.data$y.in[i]
          for (j in 1:numlayers) { # plot layer
            j.f.A <- plot.to.Atlantis[i,j]
            for (i.dest in boxes[which(boxes!=i)]) {
              xd <- box.data$x.in[i.dest]
              yd <- box.data$y.in[i.dest]
              j.d.A <- plot.to.Atlantis[i.dest,j]
              dx[i,j] <- dx[i,j] + exchange.value[i,j.f.A,i.dest,j.d.A] *
                (xd - xf) / sqrt((xd-xf)*(xd-xf) + (yd-yf)*(yd-yf))
              dy[i,j] <- dy[i,j] + exchange.value[i,j.f.A,i.dest,j.d.A] *
                (yd - yf) / sqrt((xd-xf)*(xd-xf) + (yd-yf)*(yd-yf))
            }
            # remove exchange from the deepest layer of boxid = 0
            if (j == box.data$numlayers[1]) { # deepest layer of box 0
              j.d.A <- plot.to.Atlantis[1,j] # Atlantis layer of box 0
              dx[i,j] <- dx[i,j] - exchange.value[i,j.f.A,1,j.d.A] *
                (xd - xf) / sqrt((xd-xf)*(xd-xf) + (yd-yf)*(yd-yf))
              dy[i,j] <- dy[i,j] - exchange.value[i,j.f.A,1,j.d.A] *
                (yd - yf) / sqrt((xd-xf)*(xd-xf) + (yd-yf)*(yd-yf))
            }
            if ((i == 1) & (j == box.data$numlayers[1])) { # deepest layer of box 0
              dx[i,j] <- 0.0 # remove deepest layer of box 0
              dy[i,j] <- 0.0
            }
            vec.length <- sqrt(dx[i,j]*dx[i,j] + dy[i,j]*dy[i,j])
            if (vec.length > max.length) { # find the longest vector calculated
              max.length <- vec.length
            }
          }
        }

        map.size <- sqrt((max(box.data$x.in) - min(box.data$x.in)) *
            (max(box.data$x.in) - min(box.data$x.in)) +
            (max(box.data$y.in) - min(box.data$y.in)) *
            (max(box.data$y.in) - min(box.data$y.in)))
        scalar <- 0.1*map.size / max.length # longest arrow is 10% of map
        dx <- scalar*dx
        dy <- scalar*dy

        # create vector map
        layer <- NULL
        x.in  <- NULL
        y.in  <- NULL
        xend  <- NULL
        yend  <- NULL
        tmp.xend <- rep(0,numboxes)
        tmp.yend <- rep(0,numboxes)

        for (j in 1:numlayers) {
          layer <- c(layer, rep(j, numboxes))
          x.in <- c(x.in, box.data$x.in)
          y.in <- c(y.in, box.data$y.in)
          for (i in 1:numboxes) {
            tmp.xend[i] <- box.data$x.in[i] + dx[i,j]
            tmp.yend[i] <- box.data$y.in[i] + dy[i,j]
          }
          xend <- c(xend, tmp.xend)
          yend <- c(yend, tmp.yend)
        }

        df.test <- data.frame(layer, x.in, y.in, xend, yend)
        df.test <- df.test %>% mutate(len = sqrt((xend-x.in)*(xend-x.in) +
            (yend-y.in)*(yend-y.in))) %>%
          dplyr::filter(len > 0)

        boxid <- rep(0:(numboxes-1), numlayers)
        layer <- sort(rep(1:numlayers, numboxes))
        df <- data.frame(boxid, numlayers = box.data$numlayers, layer = layer)
        df.plot <- dplyr::left_join(input.object$map.vertices, df, by = "boxid")
        df.plot$boxid <- factor(df.plot$boxid)
        df.plot <- df.plot %>% mutate(water.present = (layer <= numlayers))
        df.plot$numlayers<- factor(df.plot$numlayers)

        gg <- ggplot(data = df.plot,
          aes(x = x, y = y, group = boxid, fill = water.present)) +
          geom_polygon(colour = "grey85", size = 0.25) +
          geom_segment(data = df.test,
            aes(x = x.in, y = y.in, xend = xend, yend = yend),
            arrow = arrow(length = unit(0.2,"cm")),
            inherit.aes = FALSE) +
          facet_wrap( ~ layer, ncol = 3) +
          labs(fill = "Water\npresent") +
          scale_fill_manual(values=c("wheat1", "lightskyblue")) +
          theme_bw() + xlab("") + ylab("") +
          theme(plot.background = element_blank()) +
          scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL) +
          coord_cartesian(xlim = rangesFlows$x, ylim = rangesFlows$y)
        if (input$CI.BoxIDFlows) {
          gg <- gg + geom_text(data = input.object$box.data, mapping = aes(x = x.in, y = y.in, label = boxid), size = 2.5, inherit.aes = FALSE)
        }
        gg
      })

      # -- Vertical exchanges -- Plot
      output$plotVert <- renderPlot({
        t <- round(as.integer((input$NI.Vert - time[1]) / dt), digits = 0) + 1

        exchange.value[ , , , ] <- 0.0

        for (i in 1:numboxes) {
          for (j in 1:numlayers) {
            for (k in 1:numdests) {
              if (!is.na(input.object$exchange[k,j,i,t])) {
                dest.i <- input.object$dest.box[k,j,i,t] + 1
                dest.j <- input.object$dest.layer[k,j,i,t] + 1 # Atlantis layer
                if (!is.na(dest.i) & !is.na(dest.j)) {
                  exchange.value[i,j,dest.i,dest.j] <-
                    input.object$exchange[k,j,i,t] # from i to dest
                  exchange.value[dest.i,dest.j,i,j] <-
                    -input.object$exchange[k,j,i,t] # from dest to i
                }
              }
            }
          }
        }

        vert.dir[ , ] <- 0.0 # second index is Atlantis layer

        for (i in 1:numboxes) { # boxid = i-1
          for (j in 1:numlayers) { # j is Atlantis layer
            if ((j - 1) > 0) { # a box above
              if (!is.na(exchange.value[i,j-1,i,j])) { # exchange is provided
                vert.dir[i,j] <- exchange.value[i,j-1,i,j] / box.data$area[i] # + is down
              }
            }
            if ((j + 1) < numlayers) { # a box below
              if (!is.na(exchange.value[i,j,i,j+1])) { # exchange is provided
                if (is.na(vert.dir[i,j])) { # no exchange into this box yet
                  vert.dir[i,j] <- exchange.value[i,j,i,j+1] / box.data$area[i] # + is down
                } else {
                  vert.dir[i,j] <- vert.dir[i,j] + exchange.value[i,j,i,j+1] / box.data$area[i]
                }
              }
            }
          }
        }

        if (!input$CI.Vert) {
          vert.dir <- sign(vert.dir) # + is down (this is currently not used)
        }

        df <- data.frame(boxid = 0:(numboxes-1), area = box.data$area,
          numlayers = box.data$numlayers, vert.dir)
        layer.names <- rep("x", numlayers)
        for (i in 1:numlayers) {
          layer.names[i] <- paste("l", as.character(i-1), sep = "")
        }

        names(df)[4:(numlayers+3)] <- layer.names
        tmp <- numlayers+3
        df <- tidyr::gather(df, layer, vert, 4:tmp)
        df$layer <- sort(rep(0:(numlayers-1), numboxes))

        df$dest.plot.layer <- df$numlayers - df$layer
        df$dest.plot.layer[df$dest.plot.layer < 1] <-
          numlayers + df$dest.plot.layer[df$dest.plot.layer < 1]
        df$vert[df$layer >= df$numlayers] <- NA

        df.plot <- dplyr::left_join(input.object$map.vertices, df, by = "boxid")

        gg <- ggplot(data = df.plot,
          aes(x = x, y = y, group = boxid, fill = vert)) +
          geom_polygon(colour = "grey25", size = 0.25, na.rm = TRUE) +
          scale_fill_gradient2(low = "#e6550d", high = "#3182bd", mid = "white", midpoint=0, na.value="grey75") +
          labs(fill = "Vertical\nexchange\n(m/time step)") +
          facet_wrap( ~ dest.plot.layer, ncol = 3) +
          theme_bw() + xlab("") + ylab("") +
          theme(plot.background = element_blank()) +
          scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL) +
          coord_cartesian(xlim = rangesVert$x, ylim = rangesVert$y)
        if (input$CI.BoxIDVert) { # add box id
          gg <- gg + geom_text(data = input.object$box.data, mapping = aes(x = x.in, y = y.in, label = boxid), size = 2.5, inherit.aes = FALSE)
        }
        gg
      })

      # -- Vertical exchanges -- Table
      output$table.Vert <- DT::renderDataTable({

        t <- round(as.integer((input$NI.Vert - time[1]) / dt), digits = 0) + 1

        exchange.value[ , , , ] <- 0.0
        for (i in 1:numboxes) {
          for (j in 1:numlayers) {
            for (k in 1:numdests) {
              if (!is.na(input.object$exchange[k,j,i,t])) {
                dest.i <- input.object$dest.box[k,j,i,t] + 1
                dest.j <- input.object$dest.layer[k,j,i,t] + 1
                if (!is.na(dest.i) & !is.na(dest.j)) {
                  exchange.value[i,j,dest.i,dest.j] <-
                    input.object$exchange[k,j,i,t] # from i to dest
                  exchange.value[dest.i,dest.j,i,j] <-
                    -input.object$exchange[k,j,i,t] # from dest to i
                }
              }
            }
          }
        }

        vert.dir[ , ] <- 0.0
        for (i in 1:numboxes) {
          for (j in 1:numlayers) { # surface to bottom
            if ((j - 1) > 0) { # a box above
              if (!is.na(exchange.value[i,j-1,i,j])) { # exchange is provided
                vert.dir[i,j] <- exchange.value[i,j-1,i,j] / box.data$area[i] # + is up
              }
            }
            if ((j + 1) < numlayers) { # a box below
              if (!is.na(exchange.value[i,j,i,j+1])) { # exchange is provided
                if (is.na(vert.dir[i,j])) { # no exchange into this box yet
                  vert.dir[i,j] <- exchange.value[i,j,i,j+1] / box.data$area[i] # + is up
                } else {
                  vert.dir[i,j] <- vert.dir[i,j] + exchange.value[i,j,i,j+1] / box.data$area[i]
                }
              }
            }
          }
        }

        df <- data.frame(boxid = 0:(numboxes-1), area = box.data$area,
          numlayers = box.data$numlayers, vert.dir)
        layer.names <- rep("x", numlayers)
        for (i in 1:numlayers) {
          layer.names[i] <- paste("l", as.character(i-1), sep = "")
        }

        names(df)[4:(numlayers+3)] <- layer.names
        tmp <- numlayers+3
        df <- tidyr::gather(df, layer, vert, 4:tmp)
        df$layer <- sort(rep(0:(numlayers-1), numboxes))

        df$dest.plot.layer <- df$numlayers - df$layer
        df$dest.plot.layer[df$dest.plot.layer < 1] <-
          numlayers + df$dest.plot.layer[df$dest.plot.layer < 1]
        df$vert[df$layer >= df$numlayers] <- NA
        df <- df %>% dplyr::filter(!is.na(vert))
        df <- df[c(1,2,3,4,6,5)] # reorder columns

        DT::datatable(df, rownames = FALSE,
          colnames = c('box', 'area', 'water layers', 'layer (Atlantis)', 'layer (plotting)', 'vertical exchange'),
          caption = "Vertical exchanges (m^3 / box area). Positive values indicate upward flow.")
      })

      # -- Temperature -- Plot (Spatial)
      output$plotTemp <- renderPlot({
        #  browser()
        # calculate the time index of the data matrices
        t <- round((input$NI.Temp - input.object$t[1]) / dt, digits = 0) + 1

        if (!is.null(input.object$temperature)) {
          df <- data.frame(boxid = 0:(numboxes-1), numlayers = box.data$numlayers,
            t(input.object$temperature[ , ,t]))
          layer.names <- rep("x", numlayers+1)
          for (i in 1:(numlayers+1)) {
            layer.names[i] <- paste("l", as.character(i-1), sep = "")
          }

          names(df)[3:(numlayers+3)] <- layer.names
          tmp <- numlayers+3
          df <- tidyr::gather(df, layer, temperature, 3:tmp)
          df$layer <- sort(rep(0:numlayers, numboxes))

          df$dest.plot.layer <- ifelse(df$layer == numlayers,
            "sediment", df$numlayers - df$layer)
          df$temperature[df$dest.plot.layer < 1] <- NA
          df$temperature[df$numlayers == 0] <- NA
          df$dest.plot.layer[df$dest.plot.layer < 1] <- numlayers +
            as.integer(df$dest.plot.layer[df$dest.plot.layer < 1])

          df.plot <- dplyr::left_join(input.object$map.vertices, df, by = "boxid")
          df.plot$dest.plot.layer  <- paste0( 'Layer - ',df.plot$dest.plot.layer)
          gg <- ggplot(data = df.plot,
            aes(x = x, y = y, group = boxid, fill = temperature)) +
            geom_polygon(colour = "grey25", size = 0.25, na.rm = TRUE) +
            scale_fill_gradient(low = "lightblue1", high = "#de2d26", na.value="wheat1") +
            labs(fill = "Celcius") +
            facet_wrap( ~ dest.plot.layer, ncol = 3) +
            theme_bw(base_size = 18) + xlab("") + ylab("") +
            theme(plot.background = element_blank()) +
            scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL) +
            coord_cartesian(xlim = rangesTemp$x, ylim = rangesTemp$y)
          if (input$CI.BoxIDTemp) { # add box id
            gg <- gg + geom_text(data = input.object$box.data, mapping = aes(x = x.in, y = y.in, label = boxid), size = 2.5, inherit.aes = FALSE)
          }
#          browser()
#          png('/home/por07g/Documents/Projects/Salish_Sea/Documents/Report/spatial_temp.png',  width = 1200, heigh = 900)
          gg
#          dev.off()
        } else {
          df <- data.frame(x = 0, y = 0)
          ggplot(data = df, aes(x = x, y = y)) +
            ggtitle("No Temperature data provided") +
            theme_bw() + xlab("") + ylab("") +
            theme(
              plot.background = element_blank(),
              plot.title=element_text(colour="red")
            ) +
            scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL)
        }
      })

      # -- Temperature -- Table (Spatial)
      output$table.Temp <- DT::renderDataTable({
        # calculate the time index of the data matrices
        t <- round((input$NI.Temp - input.object$t[1]) / dt, digits = 0) + 1

        if (!is.null(input.object$temperature)) {
          df <- data.frame(boxid = 0:(numboxes-1), numlayers = box.data$numlayers,
            t(input.object$temperature[ , ,t]))
          layer.names <- rep("x", numlayers+1)
          for (i in 1:(numlayers+1)) {
            layer.names[i] <- paste("l", as.character(i-1), sep = "")
          }

          names(df)[3:(numlayers+3)] <- layer.names
          tmp <- numlayers+3
          df <- tidyr::gather(df, layer, temperature, 3:tmp)
          df$layer <- sort(rep(0:numlayers, numboxes))

          df$dest.plot.layer <- ifelse(df$layer == numlayers,
            "sediment", df$numlayers - df$layer)
          df$temperature[df$dest.plot.layer < 1] <- NA
          df$dest.plot.layer[df$dest.plot.layer < 1] <- numlayers +
            as.integer(df$dest.plot.layer[df$dest.plot.layer < 1])
          df <- df[c(1,2,3,5,4)] # change order of columns
          df <- df[df$numlayers > 0, ]
          df <- df[!((df$layer != numlayers) & (df$layer >= df$numlayers)), ]

          DT::datatable(df, rownames = FALSE,
            colnames = c('box', 'water layers', 'layer (Atlantis)', 'layer (plotting)', 'temperature'),
            caption = "Temperature (Celcius).")
        }
      })

      # -- Temperature -- Plot (Time-series)
      output$plotTempTS <- renderPlot({
        focal.box <- as.integer(input$SI.TempTS) # 1:numboxes

        if (!is.null(input.object$temperature)) {
          df <- data.frame(time = time,
            t(input.object$temperature[ , focal.box+1, ]))
          layer.names <- rep("x", numlayers+1)
          for (i in 1:(numlayers+1)) {
            layer.names[i] <- paste("l", as.character(i-1), sep = "")
          }

          names(df)[2:(numlayers+2)] <- layer.names
          df <- tidyr::gather(df, layer, temperature, 2:(numlayers+2))
          df$layer <- sort(rep(0:numlayers, numtimes))

          df$dest.plot.layer <- ifelse(df$layer == numlayers,
            "sediment", box.data$numlayers[focal.box+1] - df$layer)
          df$temperature[df$dest.plot.layer < 1] <- NA
          df$temperature[df$numlayers == 0] <- NA
          df$dest.plot.layer[df$dest.plot.layer < 1] <- numlayers +
            as.integer(df$dest.plot.layer[df$dest.plot.layer < 1])

          plot.cols <- c(colorRampPalette(c("#fcbba1", "#99000d"))( numlayers ),"black")

          p <- ggplot(data = df,
                      aes(x = time, y = temperature, color = dest.plot.layer)) +
              geom_line(na.rm = TRUE) +
              geom_point(na.rm = TRUE) +
              scale_color_manual(values=plot.cols) +
              ylab("Temperature (Celcius)") + xlab("Time (days)") +
              labs(color = "Plot layer") +
              theme_bw(base_size = 18)
        } else {
            df <- data.frame(x = 0, y = 0)
            p <- ggplot(data = df, aes(x = x, y = y)) +
                ggtitle("No temperature data provided") +
                theme_bw() + xlab("") + ylab("") +
                theme(
                    plot.background = element_blank(),
                  plot.#TODO: itle=element_text(colour="red")
                ) +
                scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL)
        }
        ## browser()
        ## png('/home/por07g/Documents/Projects/Salish_Sea/Documents/Report/temporal_temp.png',  width = 1200, heigh = 900)
        ## p
        ## dev.off()
      })

      # -- Temperature -- Table (Time-series)
      output$table.TempTS <- DT::renderDataTable({
        focal.box <- as.integer(input$SI.TempTS) # 1:numboxes

        if (!is.null(input.object$temperature)) {
          df <- data.frame(time = time,
            t(input.object$temperature[ , focal.box+1, ]))
          layer.names <- rep("x", numlayers+1)
          for (i in 1:(numlayers+1)) {
            layer.names[i] <- paste("l", as.character(i-1), sep = "")
          }

          names(df)[2:(numlayers+2)] <- layer.names
          df <- tidyr::gather(df, layer, temperature, 2:(numlayers+2))
          df$layer <- sort(rep(0:numlayers, numtimes))

          df$dest.plot.layer <- ifelse(df$layer == numlayers,
            "sediment", box.data$numlayers[focal.box+1] - df$layer)
          df$temperature[df$dest.plot.layer < 1] <- NA
          df$temperature[df$numlayers == 0] <- NA
          df$dest.plot.layer[df$dest.plot.layer < 1] <- numlayers +
            as.integer(df$dest.plot.layer[df$dest.plot.layer < 1])
          df <- df[ , c(1,2,4,3)]
          df <- df[!((df$layer != numlayers) &
            (df$layer >= box.data$numlayers[focal.box+1])), ]

          DT::datatable(df, rownames = FALSE,
            colnames = c('time', 'layer (Atlantis)', 'layer (plotting)', 'temperature'),
            caption = "Temperature (Celcius).")
        }
      })

      # -- Salinity -- Plot (Spatial)
      output$plotSalt <- renderPlot({
        # calculate the time index of the data matrices
        t <- round((input$NI.Salt - input.object$t[1]) / dt, digits = 0) + 1

        df <- data.frame(boxid = 0:(numboxes-1), numlayers = box.data$numlayers,
          t(input.object$salinity[ , ,t]))
        layer.names <- rep("x", numlayers+1)
        for (i in 1:(numlayers+1)) {
          layer.names[i] <- paste("l", as.character(i-1), sep = "")
        }

        names(df)[3:(numlayers+3)] <- layer.names
        tmp <- numlayers+3
        df <- tidyr::gather(df, layer, salinity, 3:tmp)
        df$layer <- sort(rep(0:numlayers, numboxes))

        df$dest.plot.layer <- ifelse(df$layer == numlayers,
          "sediment", df$numlayers - df$layer)
        df$salinity[df$dest.plot.layer < 1] <- NA
        df$salinity[df$numlayers == 0] <- NA
        df$dest.plot.layer[df$dest.plot.layer < 1] <- numlayers +
          as.integer(df$dest.plot.layer[df$dest.plot.layer < 1])

        df.plot <- dplyr::left_join(input.object$map.vertices, df, by = "boxid")
        df.plot$dest.plot.layer  <- paste0( 'Layer - ',df.plot$dest.plot.layer)

        gg <- ggplot(data = df.plot,
          aes(x = x, y = y, group = boxid, fill = salinity)) +
          geom_polygon(colour = "grey25", size = 0.25, na.rm = TRUE) +
          scale_fill_gradient(low = "lightblue1", high = "#de2d26", na.value="wheat1") +
          labs(fill = "g/kg") +
          facet_wrap( ~ dest.plot.layer, ncol = 3) +
          theme_bw(base_size = 18) + xlab("") + ylab("") +
          theme(plot.background = element_blank()) +
          scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL) +
          coord_cartesian(xlim = rangesSalt$x, ylim = rangesSalt$y)
        if (input$CI.BoxIDSalt) { # add box id
          gg <- gg + geom_text(data = input.object$box.data, mapping = aes(x = x.in, y = y.in, label = boxid), size = 2.5, inherit.aes = FALSE)
        }

        #browser()
        #png('/home/por07g/Documents/Projects/Salish_Sea/Documents/Report/spatial_salt.png',  width = 1200, heigh = 900)
        gg
        #dev.off()

      })

      # -- Salinity -- Table (Spatial)
      output$table.Salt <- DT::renderDataTable({
        # calculate the time index of the data matrices
        t <- round((input$NI.Salt - input.object$t[1]) / dt, digits = 0) + 1

        df <- data.frame(boxid = 0:(numboxes-1), numlayers = box.data$numlayers,
          t(input.object$salinity[ , ,t]))
        layer.names <- rep("x", numlayers+1)
        for (i in 1:(numlayers+1)) {
          layer.names[i] <- paste("l", as.character(i-1), sep = "")
        }

        names(df)[3:(numlayers+3)] <- layer.names
        tmp <- numlayers+3
        df <- tidyr::gather(df, layer, salinity, 3:tmp)
        df$layer <- sort(rep(0:numlayers, numboxes))

        df$dest.plot.layer <- ifelse(df$layer == numlayers,
          "sediment", df$numlayers - df$layer)
        df$temperature[df$dest.plot.layer < 1] <- NA
        df$dest.plot.layer[df$dest.plot.layer < 1] <- numlayers +
          as.integer(df$dest.plot.layer[df$dest.plot.layer < 1])
        df <- df[c(1,2,3,5,4)] # change order of columns
        df <- df[df$numlayers > 0, ]
        df <- df[!((df$layer != numlayers) & (df$layer >= df$numlayers)), ]

        DT::datatable(df, rownames = FALSE,
          colnames = c('box', 'water layers', 'layer (Atlantis)', 'layer (plotting)', 'salinity'),
          caption = "Salinity (g/kg).")
      })

      # -- Salinity -- Plot (Time-series)
      output$plotSaltTS <- renderPlot({
        focal.box <- as.integer(input$SI.SaltTS) # 1:numboxes

        if (!is.null(input.object$salinity)) {
          df <- data.frame(time = time,
            t(input.object$salinity[ , focal.box+1, ]))
          layer.names <- rep("x", numlayers+1)
          for (i in 1:(numlayers+1)) {
            layer.names[i] <- paste("l", as.character(i-1), sep = "")
          }

          names(df)[2:(numlayers+2)] <- layer.names
          df <- tidyr::gather(df, layer, salinity, 2:(numlayers+2))
          df$layer <- sort(rep(0:numlayers, numtimes))

          df$dest.plot.layer <- ifelse(df$layer == numlayers,
            "sediment", box.data$numlayers[focal.box+1] - df$layer)
          df$salinity[df$dest.plot.layer < 1] <- NA
          df$salinity[df$numlayers == 0] <- NA
          df$dest.plot.layer[df$dest.plot.layer < 1] <- numlayers +
            as.integer(df$dest.plot.layer[df$dest.plot.layer < 1])

          plot.cols <- c(colorRampPalette(c("#fcbba1", "#99000d"))( numlayers ),"black")

          gg <- ggplot(data = df,
            aes(x = time, y = salinity, color = dest.plot.layer)) +
            geom_line(na.rm = TRUE) +
            geom_point(na.rm = TRUE) +
            scale_color_manual(values=plot.cols) +
            ylab("Salinity (g/kg)") + xlab("Time (days)") +
            labs(color = "Plot layer") +
            theme_bw(base_size = 18)
        } else {
          df <- data.frame(x = 0, y = 0)
          gg <- ggplot(data = df, aes(x = x, y = y)) +
            ggtitle("No salinity data provided") +
            theme_bw() + xlab("") + ylab("") +
            theme(
              plot.background = element_blank(),
              plot.title=element_text(colour="red")
            ) +
            scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL)
        }
        #browser()
        #png('/home/por07g/Documents/Projects/Salish_Sea/Documents/Report/temporal_salt.png',  width = 1200, heigh = 900)
        gg
        #dev.off()
      })

      # -- Salinity -- Table (Time-series)
      output$table.SaltTS <- DT::renderDataTable({
        focal.box <- as.integer(input$SI.SaltTS) # 1:numboxes

        if (!is.null(input.object$salinity)) {
          df <- data.frame(time = time,
            t(input.object$salinity[ , focal.box+1, ]))
          layer.names <- rep("x", numlayers+1)
          for (i in 1:(numlayers+1)) {
            layer.names[i] <- paste("l", as.character(i-1), sep = "")
          }

          names(df)[2:(numlayers+2)] <- layer.names
          df <- tidyr::gather(df, layer, salinity, 2:(numlayers+2))
          df$layer <- sort(rep(0:numlayers, numtimes))

          df$dest.plot.layer <- ifelse(df$layer == numlayers,
            "sediment", box.data$numlayers[focal.box+1] - df$layer)
          df$salinity[df$dest.plot.layer < 1] <- NA
          df$salinity[df$numlayers == 0] <- NA
          df$dest.plot.layer[df$dest.plot.layer < 1] <- numlayers +
            as.integer(df$dest.plot.layer[df$dest.plot.layer < 1])
          df <- df[ , c(1,2,4,3)]
          df <- df[!((df$layer != numlayers) &
              (df$layer >= box.data$numlayers[focal.box+1])), ]

          DT::datatable(df, rownames = FALSE,
            colnames = c('time', 'layer (Atlantis)', 'layer (plotting)', 'salinity'),
            caption = "Salinity (g/kg).")
        }
      })

      observeEvent(input$exitButton, {
        stopApp()
      })
    }
  )
}

# +==========================================================+
# |  make.map.object.frc : collect data for displaying maps  |
# +==========================================================+
make.map.object.frc <- function(bgm.file, cum.depth){
  bgm <- readLines(bgm.file) # read in the geometry file

  numboxes <- 0
  j <- grep(pattern = "nbox", x = bgm, value = FALSE) # file row(s)
  if (length(j) > 0) { # found rows with nbox
    jnew <- NULL
    for (jj in 1:length(j)) {
      # Valid row is when tmplt is the first entry and second is a number
      text.split <- unlist(str_split(
        gsub(pattern = "[\t ]+", x = bgm[j[jj]], replacement = " "), " "))
      if (text.split[1] == "nbox") {
        jnew <- c(jnew,j[jj]) # add the row that satisfies the criteria
      }
    }
    j <- jnew # use this list of rows as they are valid
    if (length(j) == 1) { # a single row is found
      text.split <- unlist(str_split(
        gsub(pattern = "[\t ]+", x = bgm[j], replacement = " "), " "))
      numboxes <- as.numeric(text.split[2])
    }
  }

  # Extract the box vertices
  map.vertices <- data.frame()
  for(i in 1:numboxes){
    txt.find <- paste("box", i - 1, ".vert", sep = "")
    j <- grep(txt.find, bgm)
    for (jj in 1:length(j)) {
      text.split <- unlist(str_split(
        gsub(pattern = "[\t ]+", x = bgm[j[jj]], replacement = " "), " "))
      if (text.split[1] == txt.find) {
        map.vertices <- rbind(map.vertices, cbind(i - 1, as.numeric(text.split[2]),
          as.numeric(text.split[3])))
      }
    }
  }
  names(map.vertices) <- c("boxid", "x", "y")

  # find the depths and areas, and identify island boxes
  box.indices <- rep(0, numboxes)
  for(i in 1:numboxes){ # box depth
    box.indices[i] <- grep(paste("box", i - 1, ".botz", sep = ""), bgm)
  }
  z.tmp <- strsplit(bgm[box.indices], "\t")
  z <- as.numeric(sapply(z.tmp,`[`,2))
  box.data <- data.frame(boxid = 0:(numboxes-1), z = z)
  box.data <- mutate(box.data, is.island = (z >= 0.0))
  for(i in 1:numboxes){ # box area
    box.indices[i] <- grep(paste("box", i - 1, ".area", sep = ""), bgm)
  }
  a.tmp <- strsplit(bgm[box.indices], "\t")
  a <- as.numeric(sapply(a.tmp,`[`,2))
  box.data$area <- a
  box.data <- mutate(box.data, volume = -z*area)

  # read in the internal coordinates from bgm file
  box.indices <- rep(0, numboxes)
  x.in <- rep(0, numboxes)
  y.in <- rep(0, numboxes)
  for(i in 1:numboxes){
    j <- grep(paste("box", i - 1, ".inside", sep = ""), bgm)
    text.split <- unlist(str_split(
      gsub(pattern = "[\t ]+", x = bgm[j], replacement = " "), " "))
    x.in[i] <- as.numeric(text.split[2])
    y.in[i] <- as.numeric(text.split[3])
  }
  box.data$x.in <- x.in # add internal y-location
  box.data$y.in <- y.in # add internal y-location
  box.data$boxid <- factor(box.data$boxid) # make boxid a factor

  # calculate the number of water layers per box base don cumulative depths
  # CHECK THIS IS CORRECT: boxid = 21, index = 21 (6 but should be 5)
  z <- -box.data$z # convert depths so depth below surface is positive
  z <- pmax(0,z) # remove depths above the surface
  z <- pmin(z, max(cum.depth)) # don't alow depth to be greater than max depth
  box.numlayers <- rep(0, length(z)) # vector containing number of water layers
  for (i in 1: length(z)) {
    box.numlayers[i] <- sum(z[i] > cum.depth)
  }
  box.data$numlayers <- box.numlayers # add the vector to box.data

  return(list(
    map.vertices = map.vertices,
    box.data     = box.data)
  )
}

# +===================================================================+
# |  make.exchanges.object.frc : collect exchanges data from nc file  |
# +===================================================================+
make.exchanges.object.frc <- function(exchange.file) {
  nc.exchange.out <- nc_open(exchange.file) # open .nc file

  t          <- ncvar_get(nc.exchange.out, "t")
  exchange   <- ncvar_get(nc.exchange.out, "exchange") # keep raw values
  dest.box   <- ncvar_get(nc.exchange.out, "dest_b")   # keep raw values
  dest.layer <- ncvar_get(nc.exchange.out, "dest_k") # keep raw values
  nc_close(nc.exchange.out)

  numboxes <- dim(exchange)[3] # number of boxes

  t <- t / (60*60*24) # convert seconds to days

  numlayers <- dim(exchange)[2] # number of depth layers
  numdests <- dim(exchange)[1] # number of destinations
  numtimes <- dim(exchange)[4] # number of time points

  return(list(
    t          = t,
    numdests   = numdests,
    numlayers  = numlayers,
    numboxes   = numboxes,
    numtimes   = numtimes,
    exchange   = exchange,
    dest.box   = dest.box,
    dest.layer = dest.layer
  ))
}

# +=================================================================+
# |  make.salinity.object.frc : collect salinity data from nc file  |
# +=================================================================+
make.salinity.object.frc <- function(salinity.file) {
  nc.salinity.out <- nc_open(salinity.file) # open .nc file
  salinity   <- ncvar_get(nc.salinity.out, "salinity") # keep raw values

  nc_close(nc.salinity.out)

  return(salinity)
}

# +======================================================================+
# |  make.temperature.object.frc : collect temperature data from nc file  |
# +======================================================================+
make.temperature.object.frc <- function(temperature.file) {
  nc.temperature.out <- nc_open(temperature.file) # open .nc file
  temperature <- ncvar_get(nc.temperature.out, "temperature") # keep raw values

  nc_close(nc.temperature.out)

  return(temperature)
}

# +==========================================================================+
# |  make.sh.exchange.object : collect forcing data to display in shiny app  |
# +==========================================================================+
#' @title Function that generates an object used by sh.forcings
#'
#' @description
#' Takes data from a box geometry file and a vector of cumulative water layer
#' depths, as well as netCDF files of: exchanges, salinity and temperature,
#' and generates a list object that is the parameter to \code{\link[shinyrAtlantis]{sh.forcings}} (see Examples).
#'
#' @param bgm.file Box geometry model (.bgm) file used by Atlantis that defines box boundaries and depths.
#' @param exchange.file NetCDF (.nc) file containing water exchanges between box layers.
#' @param cum.depth Vector of depths (starting at 0) that delineate the depths of the water layers for the model.
#' @param temperature.file NetCDF (.nc) file containing time-series of temperature values for each water layer in each box. This parameter is not required.
#' @param salinity.file NetCDF (.nc) file containing time-series of salinity values for each water layer in each box. This parameter is not required.
#'
#' @return R list object used by \code{\link[shinyrAtlantis]{sh.forcings}}.
#'
#' @examples
#' \dontrun{
#' exchange.file    <- "GBR108_hydro.nc"
#' salinity.file    <- "GBR108_salt.nc"
#' temperature.file <- "GBR108_temp.nc"
#' bgm.file         <- "gbr_box_03012012.bgm"
#' cum.depth <- c(0,5,10,20,50,100,200,3000) # cumulative water layer depths
#'
#' input.object <- make.sh.forcings.object(
#'   bgm.file         = bgm.file,
#'   exchange.file    = exchange.file,
#'   cum.depth        = cum.depth,
#'   temperature.file = temperature.file,
#'   salinity.file    = salinity.file
#' )
#' sh.forcings(input.object)
#' }
#' @export
make.sh.forcings.object <- function(bgm.file, exchange.file, cum.depth,
  temperature.file = NULL, salinity.file = NULL) {

  cat("-- Extracting map data\n")
  map.object <- make.map.object.frc(bgm.file, cum.depth)

  cat("-- Extracting data (this may take a while)\n")
  flux.object <- make.exchanges.object.frc(exchange.file)

  if (!is.null(temperature.file)) {
    cat("-- Extracting temperature data\n")
    temperature <- make.temperature.object.frc(temperature.file)
  } else {
    cat("-- No temperature file provided\n")
    temperature <- NULL
  }

  if (!is.null(salinity.file)) {
    cat("-- Extracting salinity data\n")
    salinity <- make.salinity.object.frc(salinity.file)
  } else {
    cat("-- No salinity file provided\n")
    salinity <- NULL
  }

  return(list(
    cum.depth    = cum.depth,
    map.vertices = map.object$map.vertices,
    box.data     = map.object$box.data,
    numboxes     = flux.object$numboxes,
    numlayers    = flux.object$numlayers,
    numdests     = flux.object$numdests,
    numtimes     = flux.object$numtimes,
    t            = flux.object$t,
    exchange     = flux.object$exchange,
    dest.layer   = flux.object$dest.layer,
    dest.box     = flux.object$dest.box,
    temperature  = temperature,
    salinity     = salinity
  ))
}

# Some examples of generating lists from data and viewing the data

# setwd("~/Documents/Projects/Fisheries/Atlantis/Models/GBR/Input")
# exchange.file    <- "GBR108_hydro.nc"
# salinity.file    <- "GBR108_salt.nc"
# temperature.file <- "GBR108_temp.nc"
# bgm.file         <- "gbr_box_03012012.bgm"
# cum.depth <- c(0,5,10,20,50,100,200,3000)

# setwd("~/Documents/Projects/Fisheries/Atlantis/Forcings Tool/GHHP/")
# exchange.file <- "Gladstone305_6HourHYdro.nc"
# bgm.file    <- "GHHP_xy_v2.bgm"
# cum.depth <- c(0,5,10,20,250)

# setwd("~/Documents/Projects/Fisheries/Atlantis/Models/JFRE")
# exchange.file    <- "JFRE_hydro.nc"
# salinity.file    <- "JFRE_salt.nc"
# temperature.file <- "JFRE_temp.nc"
# bgm.file         <- "JFRE_xy.bgm"
# cum.depth <- c(0,20,50,150,250,400,650,1000,4300)

# input.object <- make.sh.forcings.object(
#   bgm.file         = bgm.file,
#   exchange.file    = exchange.file,
#   cum.depth        = cum.depth,
#   temperature.file = temperature.file,
#   salinity.file    = salinity.file
# )
#
# sh.forcings(input.object)
