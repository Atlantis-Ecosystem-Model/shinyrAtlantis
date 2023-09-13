# 10/06/2016

#' @title Shiny application for viewing Atlantis initialisation data
#'
#' @description
#' Takes data from a .bgm box geometry file and a netCDF Atlantis input parameter file and provides
#' a visualisation of the data in the form of a shiny application. The two data
#' files must first be pre-processed by \code{\link[shinyrAtlantis]{make.sh.init.object}},
#' which generates a list object that is the parameter to \code{sh.init}
#' (see Examples).
#'
#' The \emph{Habitat} tab displays the number of water layers and the habitat
#' cover for each box. Use this tab to check that reef + flat + soft cover sum to 1 for the interior boxes.
#'
#' The \emph{Abiotic (2D)} and \emph{Abiotic (3D)} tabs display benthic and pelagic abiotic variables.
#'
#' The \emph{Biotic (2D)} and \emph{Biotic (3D)} tabs display benthic and pelagic biotic variables.
#'
#' The \emph{Nitrogen} tab displays the initial nitrogen content of biotic groups that are modelled using cohorts.
#'
#' @param input.object R list object generated from \code{\link[shinyrAtlantis]{make.sh.init.object}}.
#'
#' @return Object of class 'shiny.appobj' see \code{\link[shiny]{shinyApp}}.
#'
#' @examples
#' \dontrun{
#' bgm.file <- "VMPA_setas.bgm"
#' nc.file  <- "INIT_VMPA_Jan2015.nc"
#' input.object <- make.sh.init.object(bgm.file, nc.file)
#' sh.init(input.object)
#' }
#' @export
#' @importFrom ggplot2 guide_legend ylim
#' @importFrom ncdf4 nc_open nc_close ncvar_get
sh.init <- function(input.object){
  # set up layer indices when plotting 3D values
  depth.layers <- matrix(NA, nrow = input.object$numlevels,
    ncol = input.object$numboxes)
  for (i in 1:input.object$numboxes) {
    if (!is.na(input.object$box.info$layers.water[i]) &
        (input.object$box.info$layers.water[i] > 0)) {
      depth.layers[1:input.object$box.info$layers.water[i],i] <-
        input.object$box.info$layers.water[i]:1
    }
  }
  has.water.layers <- input.object$box.info$layers.water > 0 # box has water
  depth.layers[input.object$numlevels,has.water.layers] <- input.object$numlevels

  # set up species sub-choice indices
  ndx.2.start <- rep(0, length(input.object$species.2.names))
  ndx.2.end <- cumsum(input.object$species.2.groups)
  ndx.2.start[1] <- 1
  ndx.2.start[2:length(input.object$species.2.names)] <- 1 + ndx.2.end[1:(length(input.object$species.2.names)-1)]

  ndx.3.start <- rep(0, length(input.object$species.3.names))
  ndx.3.end <- cumsum(input.object$species.3.groups)
  ndx.3.start[1] <- 1
  ndx.3.start[2:length(input.object$species.3.names)] <- 1 + ndx.3.end[1:(length(input.object$species.3.names)-1)]

  Nums.Choices <- c("Raw data", "Divide by box area")
  # determine which 3D biotic groups can have transformable data (contains _Nums)
  Nums.names <- rep(FALSE, length(input.object$species.3.names.full))
  Nums.names[grep(pattern = "_Nums$", x = input.object$species.3.names.full)] <- TRUE

  # create HTML text for viewing on help tab
  txtHelp <- "<p>This program displays data from the .nc file used to provide initial conditions for an <b>Atlantis</b> run.</p>"
  txtHelp <- paste(txtHelp, "<p>Plots have a zoom feature. Draw a box and double click to zoom into the box. Double click to reset zoom.</p>")
  txtHelp <- paste(txtHelp, "<p>Reef, soft, and flat values are expected to sum to one (100% cover) within the domain.</p>")
  txtHelp <- paste(txtHelp, "<p>The code attempts to separate variables into biotic and abiotic groups. Biotic groups are those where there exists a variable that contains _N in the name as this indicates the presence of nitrogen. The code also attempts to distinguish data where a vertical dimension is present (3D). Some biotic groups may be presented under the 2D and 3D panels.</p>")
  txtHelp <- paste(txtHelp, "<p>If more than one time dimension is detected then only the first time layer is displayed.</p>")
  txtHelp <- paste(txtHelp, "<p>Currently the code <em>assumes a single sediment layer</em>.</p>")
  txtHelp <- paste(txtHelp, "<p>When a variable is associated with a vertical dimension the number of water layers in each box determines where the data is displayed in the table presented below the plot. ")
  txtHelp <- paste(txtHelp, "For example, suppose that at most there are seven layers: a sediment layer and six water layers. ")
  txtHelp <- paste(txtHelp, "If a box has four water layers, then the input data in the netCDF file is assumed to have the form:</p>")
  txtHelp <- paste(txtHelp, "<ul style=\"list-style-type:none\"><li>layer 0, layer 1, layer 2, layer 3, 0, 0, sediment</li></ul>")
  txtHelp <- paste(txtHelp, "<p>where layer 0 is the layer nearest the sediment and layer 3 is the surface layer. The last value is always assumed to be the sediment layer and the first value is always assumed to be the layer closet to the sediment. Any values located by the zeros are ignored. Thus, <b>the vertical data presented in the table is not displayed in the same order as it is stored in the netCDF file</b>.</p>")
  txtHelp <- paste(txtHelp, "<p>When plotting and tabulating vertical profiles, values are presented for cells where nominal_dz > 0 (irrespective of numlayers associated with the box).</p>")
  txtHelp <- paste(txtHelp, "<p>Some checks that the vertical data make biological sense include: water is warmer in the surface layers and dz values (other than at layer 0) equate across boxes. Also, in shallower waters (< 1000m) oxygen tends to decrease with depth, whereas salinity, silica, nitrogen, and phosphorous all tend to increase with depth.</p>")
  txtHelp <- paste(txtHelp, "<p>3D biotic data describing numbers (i.e., variable name contains _Nums) may be transformed by dividing by box area. This transformation shows 2D densities, which gives a better sense of whether a species is uniformly distributed throughout its horizontal range (assuming the depth range [m] is fixed throughout its horizontal range).</p>")

  plotHeight3D <- paste(as.character(350 * ((input.object$numlevels - 1) %/% 2 + 1)),
    "px", sep = "") # calculate a reasonable overall plot size for 3D plots

  plotHeightNitrogen <- paste(as.character(125 * ((length(unique(input.object$df.nitrogen$Species)) - 1) %/% 6 + 1)),
    "px", sep = "") # calculate a reasonable overall plot size for 3D plots

  # set up consistent association between colours used to categorise cover check
  myColors <- c("#deebf7", "#de2d26", "#a1d99b", "#a50f15")
  names(myColors) <- levels(input.object$box.info$cover.check)
  colScale <- scale_fill_manual(name = "check",values = myColors)

  # copy relevant box info worth showing
  df.cover <- input.object$box.info[c(1:11,13,17)]
  df.cover$z <- -df.cover$z
  names(df.cover) <- c("boxid", "reef", "flat", "soft", "canyon", "cover",
    "check", "layers (total)", "layers (water)", "depth (nc)", "depth (bgm)",
    "area", "numlayers")
  df.cover <- df.cover[c(1:9, 13, 10, 11, 12)] # reorder columns for display

  shinyApp(

    # USER INPUT FUNCTION

    ui = navbarPage(
      title = "Atlantis initialisation viewer",
      # Habitat
      tabPanel("Habitat",
        fluidPage(
          fluidRow(
            column(6, h4("Water layers")),
            column(6, h4("Habitat cover"))
          ),
          fluidRow(
            column(6,
              plotOutput("plotLayers",
                height = "475px",
                dblclick = "plotLayers_dblclick",
                brush = brushOpts(
                  id = "plotLayers_brush",
                  resetOnNew = TRUE
                )
              )
            ),
            column(6,
              plotOutput("plotCover",
                height = "475px",
                dblclick = "plotCover_dblclick",
                brush = brushOpts(
                  id = "plotCover_brush",
                  resetOnNew = TRUE
                )
              )
            )
          ),
          hr(),
          fluidRow(column(12,
            HTML("<p><b>Notes</b></p><ul><li>reef + flat + soft are expected to sum to 1 within the model domain.</li><li>layers (total) is calculated from the number of non-zero terms in the variable nominal_dz from the .nc file. Assuming a single sediment layer, layers (water) = layers (total) - 1, which should equate to numlayers.</li><li>Bracketed terms associated with the depths indicate where the data were taken from and they should be the same for both the .nc (using nominal_dz) and .bgm files.</li></ul>"))),
          fluidRow(
            column(12, DT::dataTableOutput("table.box.info"))
          )
        )
      ),
      # Abiotic 2D
      tabPanel("Abiotic (2D)",
        sidebarLayout(
          sidebarPanel(width = 3,
            selectInput(inputId = "SI.NS2", label = "Group",
              choices = input.object$nonspecies.2.names),
            h5("Group attributes"),
            htmlOutput("txtnSp2att")
          ),
          mainPanel(
            fluidRow(column(12,
              plotOutput("plot2D",
                height = "625px",
                dblclick = "plot2D_dblclick",
                brush = brushOpts(
                  id = "plot2D_brush",
                  resetOnNew = TRUE
                )
              )
            )),
            hr(),
            fluidRow(column(4, DT::dataTableOutput("table.2D")))
          )
        )
      ),
      # Abiotic 3D
      tabPanel("Abiotic (3D)",
        sidebarLayout(
          sidebarPanel(width = 3,
            selectInput(inputId = "SI.NS3", label = "Group",
              choices = input.object$nonspecies.3.names),
            h5("Group attributes"),
            htmlOutput("txtnSp3att")
          ),
          mainPanel(
            fluidRow(column(12,
              plotOutput("plot3D",
                height = plotHeight3D,
                dblclick = "plot3D_dblclick",
                brush = brushOpts(
                  id = "plot3D_brush",
                  resetOnNew = TRUE
                )
              )
            )),
            fluidRow(column(12,
              HTML("<p>Numbers in the panel headers indicate the depth at the bottom of the water layer.</p>"))
            ),
            hr(),
            fluidRow(column(12, DT::dataTableOutput("table.3D")))
          )
        )
      ),
      # Biotic 2D
      tabPanel("Biotic (2D)",
        sidebarLayout(
          sidebarPanel(width = 3,
            selectInput(inputId = "SI2.S", label = "Group",
              choices = input.object$species.2.names),
            selectInput(inputId = "SI2.SG", label = "Sub-group",
              choices = input.object$species.2.names.full[
                1:input.object$species.2.groups[1]]),
            h5("Sub-group attributes"),
            htmlOutput("txtSp2att")
          ),
          mainPanel(
            fluidRow(column(12,
              plotOutput("plotSpecies2",
                height = "625px",
                dblclick = "plotSpecies2_dblclick",
                brush = brushOpts(
                  id = "plotSpecies2_brush",
                  resetOnNew = TRUE
                )
              )
            )),
            hr(),
            fluidRow(column(4, DT::dataTableOutput("table.species2")))
          )
        )
      ),
      # Biotic 3D
      tabPanel("Biotic (3D)",
        sidebarLayout(
          sidebarPanel(width = 3,
            selectInput(inputId = "SI3.S", label = "Group",
              choices = input.object$species.3.names),
            selectInput(inputId = "SI3.SG", label = "Sub-group",
              choices = input.object$species.3.names.full[
                1:input.object$species.3.groups[1]]),
            selectInput(inputId = "SI3.Tr", label = "Data transformation",
              choices = c("Raw data")),
            h5("Sub-group attributes"),
            htmlOutput("txtSp3att")
          ),
          mainPanel(
            fluidRow(column(12,
              plotOutput("plotSpecies3",
                height = plotHeight3D,
                dblclick = "plotSpecies3_dblclick",
                brush = brushOpts(
                  id = "plotSpecies3_brush",
                  resetOnNew = TRUE
                )
              )
            )),
            fluidRow(column(12,
              HTML("<p>Numbers in the panel headers indicate the depth at the bottom of the water layer.</p>"))
            ),
            hr(),
            fluidRow(column(12, DT::dataTableOutput("table.species3")))
          )
        )
      ),
      # Nitrogen (structural and reserve)
      tabPanel("Nitrogen",
        fluidPage(
          plotOutput("plotNitrogen", height = plotHeightNitrogen)
        )
      ),
      # NUms
      tabPanel("Nums",
        fluidPage(
          plotOutput("plotNums", height = plotHeightNitrogen)
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
      values$nsp2att <- ""
      values$nsp3att <- ""
      values$sp2att  <- ""
      values$sp3att  <- ""

      # reactive variables used to set plot ranges
      ranges         <- reactiveValues(x = NULL, y = NULL)
      rangesCover    <- reactiveValues(x = NULL, y = NULL)
      ranges2D       <- reactiveValues(x = NULL, y = NULL)
      ranges3D       <- reactiveValues(x = NULL, y = NULL)
      rangesSpecies2 <- reactiveValues(x = NULL, y = NULL)
      rangesSpecies3 <- reactiveValues(x = NULL, y = NULL)

      observeEvent(input$exitButton, {
        stopApp()
      })

      # register change in 2D abiotic sub-group and update displayed netCDF attributes
      observeEvent(input$SI.NS2, {
        indx <- which(input.object$nonspecies.2.names == input$SI.NS2)
        values$nsp2att <- input.object$nonspecies.2.att[indx]
      })

      # register change in 3D abiotic sub-group and update displayed netCDF attributes
      observeEvent(input$SI.NS3, {
        indx <- which(input.object$nonspecies.3.names == input$SI.NS3)
        values$nsp3att <- input.object$nonspecies.3.att[indx]
      })

      # register change in 2D biotic sub-group and update displayed netCDF attributes
      observeEvent(input$SI2.SG, {
        indx <- which(input.object$species.2.names.full == input$SI2.SG)
        values$sp2att <- input.object$species.2.att[indx]
      })

      # register change in 3D biotic sub-group and update displayed netCDF attributes
      observeEvent(input$SI3.SG, {
        indx <- which(input.object$species.3.names.full == input$SI3.SG)
        values$sp3att <- input.object$species.3.att[indx]

        if (Nums.names[indx]) {
          updateSelectInput(session, "SI3.Tr", choices = Nums.Choices)
        } else {
          updateSelectInput(session, "SI3.Tr", choices = "Raw data")
        }
      })

      # register change in 3D biotic group and update sub-group options
      observe({
        i <- which(input.object$species.3.names == input$SI3.S)
        updateSelectInput(session, "SI3.SG",
          choices = input.object$species.3.names.full[ndx.3.start[i]:ndx.3.end[i]])
      })

      # register change in 2D biotic group and update sub-group options
      observe({
        i <- which(input.object$species.2.names == input$SI2.S)
        updateSelectInput(session, "SI2.SG",
          choices = input.object$species.2.names.full[ndx.2.start[i]:ndx.2.end[i]])
      })

      # When a double-click happens, check if there's a brush on the plot.
      # If so, zoom to the brush bounds; if not, reset the zoom.
      observeEvent(input$plotLayers_dblclick, {
        brush <- input$plotLayers_brush
        if (!is.null(brush)) {
          ranges$x <- c(brush$xmin, brush$xmax)
          ranges$y <- c(brush$ymin, brush$ymax)
        } else {
          ranges$x <- NULL
          ranges$y <- NULL
        }
      })

      # When a double-click happens, check if there's a brush on the plot.
      # If so, zoom to the brush bounds; if not, reset the zoom.
      observeEvent(input$plotCover_dblclick, {
        brush <- input$plotCover_brush
        if (!is.null(brush)) {
          rangesCover$x <- c(brush$xmin, brush$xmax)
          rangesCover$y <- c(brush$ymin, brush$ymax)
        } else {
          rangesCover$x <- NULL
          rangesCover$y <- NULL
        }
      })

      # When a double-click happens, check if there's a brush on the plot.
      # If so, zoom to the brush bounds; if not, reset the zoom.
      observeEvent(input$plot2D_dblclick, {
        brush <- input$plot2D_brush
        if (!is.null(brush)) {
          ranges2D$x <- c(brush$xmin, brush$xmax)
          ranges2D$y <- c(brush$ymin, brush$ymax)
        } else {
          ranges2D$x <- NULL
          ranges2D$y <- NULL
        }
      })

      # When a double-click happens, check if there's a brush on the plot.
      # If so, zoom to the brush bounds; if not, reset the zoom.
      observeEvent(input$plot3D_dblclick, {
        brush <- input$plot3D_brush
        if (!is.null(brush)) {
          ranges3D$x <- c(brush$xmin, brush$xmax)
          ranges3D$y <- c(brush$ymin, brush$ymax)
        } else {
          ranges3D$x <- NULL
          ranges3D$y <- NULL
        }
      })

      # When a double-click happens, check if there's a brush on the plot.
      # If so, zoom to the brush bounds; if not, reset the zoom.
      observeEvent(input$plotSpecies2_dblclick, {
        brush <- input$plotSpecies2_brush
        if (!is.null(brush)) {
          rangesSpecies2$x <- c(brush$xmin, brush$xmax)
          rangesSpecies2$y <- c(brush$ymin, brush$ymax)
        } else {
          rangesSpecies2$x <- NULL
          rangesSpecies2$y <- NULL
        }
      })

      # When a double-click happens, check if there's a brush on the plot.
      # If so, zoom to the brush bounds; if not, reset the zoom.
      observeEvent(input$plotSpecies3_dblclick, {
        brush <- input$plotSpecies3_brush
        if (!is.null(brush)) {
          rangesSpecies3$x <- c(brush$xmin, brush$xmax)
          rangesSpecies3$y <- c(brush$ymin, brush$ymax)
        } else {
          rangesSpecies3$x <- NULL
          rangesSpecies3$y <- NULL
        }
      })

      # display 2D 1biotic netCDF variable attributes
      output$txtnSp2att <- renderUI({
        HTML(values$nsp2att)
      })

      # display 3D abiotic netCDF variable attributes
      output$txtnSp3att <- renderUI({
        HTML(values$nsp3att)
      })

      # display 2D biotic netCDF variable attributes
      output$txtSp2att <- renderUI({
        HTML(values$sp2att)
      })

      # display 3D biotic netCDF variable attributes
      output$txtSp3att <- renderUI({
        HTML(values$sp3att)
      })

      # display 2D habitat data in table form
      output$table.box.info <- DT::renderDataTable({
        DT::datatable(df.cover, rownames = FALSE)
      })

      # display number of water layers
      output$plotLayers <- renderPlot({
        ggplot(data = input.object$df.map,
          aes(x = x, y = y, group = boxid, fill = layers.water)) +
          geom_polygon(colour = "black", size = 0.25) +
          scale_fill_gradient(low="#fee6ce", high="#d94801",
            na.value="tomato", guide=guide_legend()) +
          labs(fill = "layers (water)") +
          geom_text(aes(x = x.in, y = y.in, label = boxid), size = 2.5) +
          theme_bw() + xlab("") + ylab("") +
          theme(plot.background = element_blank()) +
          coord_cartesian(xlim = ranges$x, ylim = ranges$y) +
          scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL)
      })

      # display cover check
      output$plotCover <- renderPlot({
        ggplot(data = input.object$df.map,
          aes(x = x, y = y, group = boxid, fill = cover.check)) +
          geom_polygon(colour = "white", size = 0.25) +
          colScale +
          labs(fill = "reef + flat + soft") +
          geom_text(aes(x = x.in, y = y.in, label = boxid), size = 2.5) +
          theme_bw() + xlab("") + ylab("") +
          theme(plot.background = element_blank()) +
          coord_cartesian(xlim = rangesCover$x, ylim = rangesCover$y) +
          scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL)
      })

      # display 2D abiotic data in plot form
      output$plot2D <- renderPlot({
        boxid <- 0:(input.object$numboxes-1)
        indx <- which(input.object$nonspecies.2.names == input$SI.NS2)
        data.2D <- input.object$nonspecies.2.data[indx,]
        data.2D[!has.water.layers] <- NA
        df.plot <- data.frame(
          boxid = boxid,
          vals = data.2D
        )
        df.plot <- left_join(input.object$df.map, df.plot, by = "boxid")
        ggplot(data = df.plot,
          aes(x = x, y = y, group = boxid, fill = vals)) +
          geom_polygon(colour = "black", size = 0.25) +
          scale_fill_gradient(low="#fee6ce", high="#d94801",
            na.value="black", guide=guide_legend()) +
          labs(fill = "value") +
          theme_bw() + xlab("") + ylab("") +
          theme(plot.background = element_blank()) +
          coord_cartesian(xlim = ranges2D$x, ylim = ranges2D$y) +
          scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL)
      })

      # display 2D abiotic data in table form
      output$table.2D <- DT::renderDataTable({
        boxid <- 0:(input.object$numboxes-1)
        indx <- which(input.object$nonspecies.2.names == input$SI.NS2)
        data.2D = input.object$nonspecies.2.data[indx,]
        df.plot <- data.frame(
          boxid = boxid,
          vals = data.2D
        )
        names(df.plot) <- c("boxid", "value")

        DT::datatable(df.plot, rownames = FALSE)
      })

      # display 3D abiotic data in plot form
      output$plot3D <- renderPlot({
        boxid <- 0:(input.object$numboxes-1)
        level <- 1:input.object$numlevels
        indx <- which(input.object$nonspecies.3.names == input$SI.NS3)
        data.3D <- input.object$nonspecies.3.data[indx, , ]

        for (i in 1:input.object$numlevels) {
          vals <- rep(NA, input.object$numboxes) # reset values
          for (j in 1:input.object$numboxes) {
            if (is.na(depth.layers[i,j])) {
              vals[j] <- NA
            } else {
              vals[j] <- data.3D[j , depth.layers[i,j]]
            }
          }
          df.level <- data.frame(
            boxid = boxid,
            depth = rep(input.object$depths[i], input.object$numboxes),
            vals = vals
          )
          if (i == 1) {
            df.plot <- df.level
          } else{
            df.plot <- rbind(df.plot, df.level)
          }
        }
        df.plot <- left_join(input.object$df.map, df.plot, by = "boxid")
        ggplot(data = df.plot,
          aes(x = x, y = y, group = boxid, fill = vals)) +
          geom_polygon(colour = "black", size = 0.25) +
          scale_fill_gradient(low="#fee6ce", high="#d94801",
            na.value="black", guide=guide_legend()) +
          labs(fill = "value") +
          facet_wrap( ~ depth, ncol = 2) +
          theme_bw() + xlab("") + ylab("") +
          theme(plot.background = element_blank()) +
          coord_cartesian(xlim = ranges3D$x, ylim = ranges3D$y) +
          scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL)
      })

      # display 3D abiotic data in table form
      output$table.3D <- DT::renderDataTable({
        boxid <- 0:(input.object$numboxes-1)
        level <- 1:input.object$numlevels
        indx <- which(input.object$nonspecies.3.names == input$SI.NS3)
        data.3D <- input.object$nonspecies.3.data[indx, , ]

        for (i in 1:input.object$numlevels) {
          vals <- rep(NA, input.object$numboxes) # reset values
          for (j in 1:input.object$numboxes) {
            if (is.na(depth.layers[i,j])) {
              vals[j] <- NA
            } else {
              vals[j] <- data.3D[j , depth.layers[i,j]]
            }
          }
          df.level <- data.frame(
            boxid = boxid,
            vals = vals
          )
          if (i == 1) {
            df.plot <- df.level
          } else{
            df.plot <- cbind(df.plot, vals)
          }
        }
        names(df.plot) <- c("boxid", input.object$depths)

        DT::datatable(df.plot, rownames = FALSE)
      })

      # display 2D biotic data in plot form
      output$plotSpecies2 <- renderPlot({
        boxid <- 0:(input.object$numboxes-1)
        indx <- which(input.object$species.2.names.full == input$SI2.SG)
        data.2D <- input.object$species.2.data[indx, ]
        data.2D[!has.water.layers] <- NA

        df.plot <- data.frame(
          boxid = boxid,
          vals = data.2D
        )
        df.plot <- left_join(input.object$df.map, df.plot, by = "boxid")
        gp <- ggplot(data = df.plot,
                     aes(x = x, y = y, group = boxid, fill = vals)) +
            geom_polygon(colour = "black", size = 0.25) +
            scale_fill_gradient(low="#fee6ce", high="#d94801",
                                na.value="black", guide=guide_legend()) +
            labs(fill = "mg N m-2") +
            theme_bw(base_size = 18) + xlab("") + ylab("") +
            theme(plot.background = element_blank()) +
            coord_cartesian(xlim = rangesSpecies2$x, ylim = rangesSpecies2$y) +
            scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL)
        ## browser()

        ## png('/home/por07g/Documents/Projects/Salish_Sea/Documents/Report/biotic2d_crab.png',  width = 800, height = 800)
        gp
        ## dev.off()
      })

      # display 2D biotic data in table form
      output$table.species2 <- DT::renderDataTable({
        boxid <- 0:(input.object$numboxes-1)
        indx <- which(input.object$species.2.names.full == input$SI2.SG)
        data.2D <- input.object$species.2.data[indx, ]

        df.plot <- data.frame(
          boxid = boxid,
          vals = data.2D
        )
        names(df.plot) <- c("boxid", "value")

        DT::datatable(df.plot, rownames = FALSE)
      })

      # display 3D biotic data in plot form
      output$plotSpecies3 <- renderPlot({
        boxid <- 0:(input.object$numboxes-1)
        level <- 1:input.object$numlevels
        indx <- which(input.object$species.3.names.full == input$SI3.SG)
        data.3D <- input.object$species.3.data[indx, , ]

        for (i in 1:input.object$numlevels) {
          vals <- rep(NA, input.object$numboxes) # reset values
          for (j in 1:input.object$numboxes) {
            if (is.na(depth.layers[i,j])) {
              vals[j] <- NA
            } else {
              vals[j] <- data.3D[j , depth.layers[i,j]]
              if (input$SI3.Tr == "Divide by box area") {
                vals[j] <- vals[j] / input.object$box.info$area[j]
              }
            }
          }
          df.level <- data.frame(
            boxid = boxid,
            depth = rep(input.object$depths[i], input.object$numboxes),
            vals = vals
          )
          if (i == 1) {
            df.plot <- df.level
          } else{
            df.plot <- rbind(df.plot, df.level)
          }
        }
        #scale_fill_gradient(low="#fee6ce", high="#d94801",
        #na.value="black", guide=guide_legend())
        df.plot <- left_join(input.object$df.map, df.plot, by = "boxid")
        df.plot$depth <- ifelse(df.plot$depth ==  25, 1,
                          ifelse(df.plot$depth ==  50, 2,
                         ifelse(df.plot$depth ==  100, 3,
                         ifelse(df.plot$depth ==  250, 4,
                         ifelse(df.plot$depth ==  400, 5,
                         ifelse(df.plot$depth ==  700, 7, 'sediment'))))))

        df.plot$depth <- paste('Layer - ', df.plot$depth)
        gp <- ggplot(data = df.plot,
                     aes(x = x, y = y, group = boxid, fill = vals)) +
            geom_polygon(colour = "grey25", size = 0.25, na.rm = TRUE) +
            scale_fill_gradient(low="#fee6ce", high="#d94801",
                                na.value="black") +
            labs(fill = "Numbers") +
            facet_wrap( ~ depth, ncol = 3) +
            theme_bw(base_size = 18) + xlab("") + ylab("") +
            theme(plot.background = element_blank()) +
            coord_cartesian(xlim = rangesSpecies3$x, ylim = rangesSpecies3$y) +
            scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL)

     #  browser()
     #   png('/home/por07g/Documents/Projects/Salish_Sea/Documents/Report/biotic3d_HSeals.png',  width = 1200, height = 900)
        gp
     #   dev.off()
      })

      # display 3D biotic data in table form
      output$table.species3 <- DT::renderDataTable({
        boxid <- 0:(input.object$numboxes-1)
        level <- 1:input.object$numlevels
        indx <- which(input.object$species.3.names.full == input$SI3.SG)
        data.3D <- input.object$species.3.data[indx, , ]

        for (i in 1:input.object$numlevels) {
          vals <- rep(NA, input.object$numboxes) # reset values
          for (j in 1:input.object$numboxes) {
            if (is.na(depth.layers[i,j])) {
              vals[j] <- NA
            } else {
              vals[j] <- data.3D[j , depth.layers[i,j]]
              if (input$SI3.Tr == "Divide by box area") {
                vals[j] <- vals[j] / input.object$box.info$area[j]
              }
            }
          }
          df.level <- data.frame(
            boxid = boxid,
            vals = vals
          )
          if (i == 1) {
            df.plot <- df.level
          } else{
            df.plot <- cbind(df.plot, vals)
          }
        }
        names(df.plot) <- c("boxid", input.object$depths)

        DT::datatable(df.plot, rownames = FALSE)
      })

      # display nitrogen
      output$plotNitrogen <- renderPlot({
        ggplot(data = input.object$df.nitrogen,
          aes(x = Cohort, y = N.Value, color = N.Type)) +
          geom_point() +  geom_line() + ylim(0,NA) +
          facet_wrap( ~ Species, ncol=6, scales="free_y") +
          labs(color = "Source") +
          xlab("Cohort") + ylab("Nitrogen (mg N)") +
          # theme(plot.background = element_blank()) +
          theme_bw()
      })

      # display Nums
      output$plotNums <- renderPlot({
        ggplot(data = input.object$df.nums,
          aes(x = Cohort, y = Nums.Value)) +
          geom_point() +  geom_line() + ylim(0,NA) +
          facet_wrap( ~ Species, ncol=6, scales="free_y") +
          xlab("Cohort") + ylab("Total numbers (individuals)") +
          theme(plot.background = element_blank())
      })
    }
  )
}

# +====================================================+
# |  make.init.map : collect data for displaying maps  |
# +====================================================+
make.init.map <- function(bgm.file){
  #  browser()
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

  # return a list of three objects: integer, data frame, data frame
  return(list(numboxes = numboxes,
    map.vertices = map.vertices,
    box.data = box.data))
}

# +======================================================+
# |  make.init.cover : collect cover data to display  |
# +======================================================+
make.init.cover <- function(box.data, map.vertices, nc.file) {
  nc.out <- nc_open(nc.file) # open .nc file

  n.vars <- nc.out$nvars # number of variables in the .nc file
  var.names <- rep(NA, n.vars) # store all variable names
  for (i in 1:n.vars) { # find all variables
    var.names[i] <- nc.out$var[[i]]$name # add variable name to the vector
  }

  numlayers <- ncvar_get(nc.out, "numlayers")
  if (length(dim(numlayers)) > 1) {numlayers <- numlayers[,1]} # first time only

  reef <- ncvar_get(nc.out, "reef")      # reef cover per box
  if (length(dim(reef)) > 1) {reef <- reef[,1]}
  reef[is.na(reef)] <- nc.out$var[[which(var.names == "reef")]]$missval

  flat <- ncvar_get(nc.out, "flat")      # flat cover per box
  if (length(dim(flat)) > 1) {flat <- flat[,1]}
  flat[is.na(flat)] <- nc.out$var[[which(var.names == "flat")]]$missval

  soft <- ncvar_get(nc.out, "soft")      # soft cover per box
  if (length(dim(soft)) > 1) {soft <- soft[,1]}
  soft[is.na(soft)] <- nc.out$var[[which(var.names == "soft")]]$missval

  canyon <- ncvar_get(nc.out, "canyon")   # canyon cover per box
  if (length(dim(canyon)) > 1) {canyon <- canyon[,1]}
  canyon[is.na(canyon)] <- nc.out$var[[which(var.names == "canyon")]]$missval

  numboxes <- length(numlayers) # number of boxes
  boxid <- 0:(numboxes-1)       # box ids

  # create a data frame containing the coverages
  box.info <- data.frame(boxid, reef, flat, soft, canyon) # .nc box info
  box.info <- box.info %>% mutate(cover = reef+flat+soft) # add total cover

  # add cover check
  cover.check <- rep(NA, length(box.info$boxid))
  for (i in 1:length(box.info$boxid)) {
    if (!is.na(box.info$cover[i])) {
      if (box.info$cover[i] == 0) {
        cover.check[i] <- "= 0"
      } else if (box.info$cover[i] <= 0.999) {
        cover.check[i] <- "< 1"
      } else if (box.info$cover[i] >= 1.001) {
        cover.check[i] <- "> 1"
      } else {
        cover.check[i] <- "= 1"
      }
    } else {
      cover.check[i] <- "missing"
    }
  }
  box.info$cover.check <- cover.check # add the cover.check to  box.info
  box.info$cover.check <- factor(box.info$cover.check,
    levels = c("= 0", "< 1", "= 1", "> 1")) # make a factor with specified levels

  nominal_dz <- ncvar_get(nc.out, "nominal_dz") # layer depths per box
  max.z.levels <- dim(nominal_dz)[1] # number of depth levels
  df.dz <- data.frame(boxid) # first column of data frame
  for (i in 1:max.z.levels) { # add depths of each level for each box
    df.dz[, as.character(i)] <- nominal_dz[i,] # add column for each level
  }
  df.dz[is.na(df.dz)] <- 0
  df.dz.present <- as.data.frame(df.dz[2:(max.z.levels+1)] > 0)
  df.dz.present$boxid <- boxid
  df.dz.present <- df.dz.present[c(max.z.levels+1, 1:max.z.levels)]
  df.present <- tidyr::gather(df.dz.present, "level", "present", 2:(max.z.levels+1))
  box.summary.1 <- df.present %>%
    group_by(boxid) %>%
    dplyr::summarise(layers.total = sum(present, na.rm = TRUE)) %>% # depth includes sediment
    mutate(layers.water = layers.total - 1) # layers in water column
  box.summary.1$layers.water <- pmax(rep(0,numboxes), box.summary.1$layers.water)

  # create a new long format data frame from the wide format data frame
  df.box <- tidyr::gather(df.dz, "lvl", "dz", 2:(max.z.levels+1))
  # layers.total is based on non-zero nominal_dz values
  # this code assumes a single sediment layer
  df.box$lvl <- as.integer(df.box$lvl)
  box.summary.2 <- dplyr::filter(df.box, (lvl < max.z.levels)) %>%
    dplyr::group_by(boxid) %>%
    dplyr::summarise(depth.total = sum(dz, na.rm = TRUE))
  # limit water layers to not be less than zero
  box.summary <- dplyr::left_join(box.summary.1, box.summary.2, by = "boxid")

  deepest.box <- which(box.summary$depth.total == max(box.summary$depth.total))[1] # id
  dz <- nominal_dz[,deepest.box] # vector of depths from the deepest box
  numlevels <- length(dz)
  if (max.z.levels > 0) {
    depth.sediment <- dz[max.z.levels] # last element is assumed sediment depth
  } else {
    depth.sediment <- 0
  }

  # create a list of depths
  depths <- c(as.character(cumsum(dz[(max.z.levels-1):1])), "sediment")

  # Add depth and layers to box.info
  box.info <- box.info %>% dplyr::left_join(box.summary, "boxid")
  # boxes with no water levels are missing so add the data
  na.boxes <- is.na(box.info$layers.water)
  box.info$layers.water[na.boxes] <- 0
  box.info$layers.total[na.boxes] <- 0 # no water layers so no sediment layer
  box.info$depth.total[na.boxes] <- 0.0

  # add box.data calculated from the .bgm file to box.info
  box.data$boxid <- boxid # make boxid an integer for joining
  box.info <- box.info %>% dplyr::left_join(box.data, "boxid")
  numlayers[is.na(numlayers)] <- nc.out$var[[which(var.names == "numlayers")]]$missval
  box.info$numlayers <- numlayers

  map.vertices$boxid <- as.integer(map.vertices$boxid) # enforce integer
  # create data frame for plotting
  df.map <- merge(map.vertices, box.info, by = "boxid")

  nc_close(nc.out)

  return(list(
    numlevels = numlevels,
    depths = depths,
    box.info = box.info,
    df.map = df.map
  ))
}

# +======================================================+
# |  make.init.data.ini : collect remaining data to display  |
# +======================================================+
make.init.data <- function(nc.file, numboxes, numlevels) {
  nc.out <- nc_open(nc.file) # open .nc file

  n.vars <- nc.out$nvars # number of variables in the .nc file
  var.names <- rep(NA, n.vars) # store all variable names
  for (i in 1:n.vars) { # find all variables
    var.names[i] <- nc.out$var[[i]]$name # add variable name to the vector
  }

  # find biotic variable names (step 1 of two steps)
  # find variable names ending in _N or _Nx
  species.names <- var.names[grep(pattern = "(_N)*_N[0-9]*$", x = var.names)]
  for (i in 1:length(species.names)) {
    species.old <- species.names[i]
    str_sub(species.old, start = str_length(species.old)-1,
      end = str_length(species.old)) <- "" # remove _N
    if (substr(species.old, nchar(species.old), nchar(species.old)) == "_") {
      str_sub(species.old, start = str_length(species.old),
        end = str_length(species.old)) <- "" # remove _N
    }
    species.names[i] <- species.old # replace name with _N removed
  }
  species.names <- unique(species.names) # remove duplicates from cohorts

  # find all abiotic variables
  i.species <- NA
  for (sp in species.names) {
    i.species <- c(i.species, grep(pattern = sp, x = var.names))
  }
  i.species <- unique(i.species)
  i.species <- i.species[-1] # remove NA in row 1
  non.species <- rep(TRUE, n.vars) # start off assuming all variables are abiotic
  non.species[i.species] <- FALSE # flag the biotic variables
  nonspecies.names <- var.names[non.species == TRUE] # get abiotic names

  # find odd species in the nonspecies.names list (step 2)
  # biotic variables of the form _N[number] still classified as abiotic
  odd.species <- nonspecies.names[grep(pattern = "_N", x = nonspecies.names)]
  if (length(odd.species > 0)) {
    odd.species <- str_split(odd.species, pattern = "_N")
    odd.name <- NA
    for (i in 1:length(odd.species)) {
      odd.name <- c(odd.name, odd.species[[i]][1])
    }
    odd.species <- unique(na.omit(odd.name))
    species.names <- c(species.names, odd.species) # add odd species to species list
  }

  # recalculate abiotic variable names
  i.species <- NA
  for (sp in species.names) {
    i.species <- c(i.species, grep(pattern = sp, x = var.names))
  }
  i.species <- unique(i.species[-1]) # remove NA in row 1
  non.species <- rep(TRUE, n.vars) # initially assume all are nonspecies
  non.species[i.species] <- FALSE # remove all species
  i.nonspecies <- which(non.species == TRUE)
  nonspecies.names <- var.names[i.nonspecies]
  # Light_Adaption variables are considered non-species (fix)

  # split abiotic variables into 2D and 3D groups
  nonspecies.2.d <- NA # number of dimensions
  nonspecies.3.d <- NA # number of dimensions
  nonspecies.2.names <- NA
  nonspecies.3.names <- NA
  nonspecies.2.b <- NA # box dimension
  nonspecies.3.b <- NA # box dimension
  nonspecies.3.z <- NA # depth dimension
  for (i in i.nonspecies) {
    d <- nc.out$var[[i]]$ndims
    found.depth <- FALSE
    for (j in 1:d) {
      if (nc.out$var[[i]]$dim[[j]]$name == "b") {
        b.indx <- j
      }
      if (nc.out$var[[i]]$dim[[j]]$name == "z") {
        z.indx <- j
        found.depth <- TRUE # a 3D variable
      }
    }
    if (found.depth) {
      nonspecies.3.d <- c(nonspecies.3.d, d)
      nonspecies.3.names <- c(nonspecies.3.names, nc.out$var[[i]]$name)
      nonspecies.3.b <- c(nonspecies.3.b, b.indx)
      nonspecies.3.z <- c(nonspecies.3.z, z.indx)
    } else {
      nonspecies.2.d <- c(nonspecies.2.d, d)
      nonspecies.2.names <- c(nonspecies.2.names, nc.out$var[[i]]$name)
      nonspecies.2.b <- c(nonspecies.2.b, b.indx)
    }
  }
  nonspecies.2.d <- nonspecies.2.d[-1] # remove starting NA
  nonspecies.2.names <- nonspecies.2.names[-1] # remove starting NA
  nonspecies.2.b <- nonspecies.2.b[-1] # remove starting NA
  nonspecies.3.d <- nonspecies.3.d[-1] # remove starting NA
  nonspecies.3.names <- nonspecies.3.names[-1] # remove starting NA
  nonspecies.3.b <- nonspecies.3.b[-1] # remove starting NA
  nonspecies.3.z <- nonspecies.3.z[-1] # remove starting NA

  # create matrices identifying box-layers containing water
  numlayers.all <- ncvar_get(nc.out, "numlayers")
  numlayers.all[is.na(numlayers.all)] <- 0
  max.numlayers <- dim(ncvar_get(nc.out, "volume"))[1] # includes sediment
  # 2D
  not.valid.2 <-rep(TRUE, numboxes)
  not.valid.2[numlayers.all > 0] <- FALSE
  # 3D
  not.valid.3 <- matrix(data = TRUE, nrow = numlevels, ncol = numboxes)
  for (i in 1:numboxes) {
    if (numlayers.all[i] > 0) { # some water layers
      not.valid.3[max.numlayers,i] <- FALSE
      for (j in 1:numlayers.all[i]) {
        not.valid.3[j,i] <- FALSE
      }
    }
  }

  # extract abiotic 2D data
  nonspecies.2.n <- length(nonspecies.2.names)
  nonspecies.2.data <- array(data = NA, dim = c(nonspecies.2.n, numboxes))
  nonspecies.2.att <- rep("", nonspecies.2.n)
  for (i in 1:nonspecies.2.n) {
    tmp <- ncvar_get(nc.out, nonspecies.2.names[i]) # get all variable data
    # add the fill value to the missing values
    if (ncatt_get(nc.out, varid = nonspecies.2.names[i], attname = "_FillValue")$hasatt) {
      tmp[is.na(tmp)] <- ncatt_get(nc.out, varid = nonspecies.2.names[i],
        attname = "_FillValue")$value
    } else {
      tmp[is.na(tmp)] <- 0
    }
    tmp[not.valid.2] <- NA
    tmp.dim <- length(dim(tmp))
    if (nonspecies.2.d[i] == 2) {
      # expect to be stored as a 2D array
      if (tmp.dim == 2) {
        if (nonspecies.2.b[i] == 1) {
          nonspecies.2.data[i,] <- tmp[ ,1] # spatial data is in first dimension
        } else {
          nonspecies.2.data[i,] <- tmp[1, ] # spatial data is in first dimension
        }
      } else {
        nonspecies.2.data[i,] <- tmp # actually a 1D array!
      }
    } else {
      # expect stored as a 3D array
      if (nonspecies.2.b == 1) {
        nonspecies.2.data[i,] <- tmp[ ,1,1] # spatial data is in first dimension
      } else {
        nonspecies.2.data[i,] <- tmp[1, ,1] # spatial data is in first dimension
      }
    }

    # get the attribute details and store as HTML text
    tmp <- ncatt_get(nc.out, nonspecies.2.names[i]) # attribute names
    tmp.n <- length(tmp) # number of attributes
    tmp.names <- names(tmp)
    txt <- ""
    for (k in 1:tmp.n) {
      txt.tmp <- paste(tmp.names[k], ": ", as.character(tmp[k]), sep = "")
      txt <- paste(txt, txt.tmp, sep = "<br/>")
    }
    nonspecies.2.att[i] <- txt
  }

  # extract abiotic 3D data
  nonspecies.3.n <- length(nonspecies.3.names)
  nonspecies.3.data <- array(data = NA, dim = c(nonspecies.3.n, numboxes, numlevels))
  nonspecies.3.att <- rep("", nonspecies.3.n)
  for (i in 1:nonspecies.3.n) {
    tmp <- ncvar_get(nc.out, nonspecies.3.names[i]) # get all variable data
    # add the fill value to the missing values
    if (ncatt_get(nc.out, varid = nonspecies.3.names[i], attname = "_FillValue")$hasatt) {
      tmp[is.na(tmp)] <- ncatt_get(nc.out, varid = nonspecies.3.names[i],
        attname = "_FillValue")$value
    } else {
      tmp[is.na(tmp)] <- 0
    }
    tmp[not.valid.3] <- NA
    tmp.dim <- length(dim(tmp))
    if (nonspecies.3.d[i] == 2) {
      # expect to be stored as a 2D array
      for (j in 1:numlevels) {
        if (nonspecies.3.z[i] == 1) { # depth index is first dimension
          nonspecies.3.data[i, ,j] <- tmp[j, ]
        } else { # depth index is second dimension
          nonspecies.3.data[i, ,j] <- tmp[ ,j]
        }
      }
    } else {
      # expect to be stored as a 3D array
      if (tmp.dim == 2) {
        # only two dimensions!
        for (j in 1:numlevels) {
          if (nonspecies.3.z[i] == 1) { # depth index is first dimension
            nonspecies.3.data[i, ,j] <- tmp[j, ]
          } else { # depth index is second dimension
            nonspecies.3.data[i, ,j] <- tmp[ ,j]
          }
        }
      } else {
        for (j in 1:numlevels) {
          if (nonspecies.3.z[i] == 1) { # depth index is first dimension
            nonspecies.3.data[i, ,j] <- tmp[j, ,1]
          } else { # depth index is second dimension
            nonspecies.3.data[i, ,j] <- tmp[ ,j,1]
          }
        }
      }
    }

    tmp <- ncatt_get(nc.out, nonspecies.3.names[i])
    tmp.n <- length(tmp)
    tmp.names <- names(tmp)
    txt <- ""
    for (k in 1:tmp.n) {
      txt.tmp <- paste(tmp.names[k], ": ", as.character(tmp[k]), sep = "")
      txt <- paste(txt, txt.tmp, sep = "<br/>")
    }
    nonspecies.3.att[i] <- txt
  }

  # split species into 2D and 3D groups
  species.2.names.full <- NA # full names (2D)
  species.3.names.full <- NA # full names (3D)
  species.n <- length(species.names)
  species.2.N <- rep(0, species.n) # number of subgroups per species group (2D)
  species.3.N <- rep(0, species.n) # number of subgroups per species group (2D)
  for (i in 1:species.n) { # go through each species
    sp <- species.names[i] # extract the species group name
    rgexprn <- paste("^", sp, sep="") # what if superset names (see below)?
    j <- grep(rgexprn, var.names) # indices associated with species group
    tmp.names <- gsub(sp, "", var.names[j]) # remove species name
    j2 <- NULL
    for (k in 1:length(j)) {
      if (substr(tmp.names[k], 1, 1) == "_" |
          substr(tmp.names[k], 1, 1) %in% as.character(1:9)) { # valid variable
        j2 <- c(j2, j[k])
      }
    }

    for (k in j2) {
      tmp <- ncvar_get(nc.out, var.names[k]) # get the associated data set
      tmp.dims <- length(dim(tmp)) # dimensions of the associated data
      # TO DO: check that these assumptions about dimensions are correct
      if (tmp.dims == 1) {
        # 2D species distribution with no time replicates
        species.2.names.full <- c(species.2.names.full, var.names[k])
        species.2.N[i] <- species.2.N[i] + 1
      } else if (tmp.dims == 2) {
        if (dim(tmp)[1] == numlevels) {
          # 3D with no time replicates
          species.3.names.full <- c(species.3.names.full, var.names[k])
          species.3.N[i] <- species.3.N[i] + 1
        } else {
          # 2D with time replicates
          species.2.names.full <- c(species.2.names.full, var.names[k])
          species.2.N[i] <- species.2.N[i] + 1
        }
      } else if (tmp.dims ==3) {
        # 3D species distribtion with time replicates
        species.3.names.full <- c(species.3.names.full, var.names[k])
        species.3.N[i] <- species.3.N[i] + 1
      }
    }
  }
  species.2.names.full <- species.2.names.full[-1] # remove starting NA
  species.3.names.full <- species.3.names.full[-1] # remove starting NA

  species.2.names <- NA # names (2D)
  species.3.names <- NA # names (3D)
  for (i in 1:species.n) { # gp through each species
    if (species.2.N[i] > 0) {
      species.2.names <- c(species.2.names, species.names[i])
    }
    if (species.3.N[i] > 0) {
      species.3.names <- c(species.3.names, species.names[i])
    }
  }
  species.2.names <- species.2.names[-1] # remove starting NA
  species.3.names <- species.3.names[-1] # remove starting NA

  # 2D species data
  species.2.n <- length(species.2.names)
  species.2.all.n <- sum(species.2.N)
  species.2.groups <- rep(0, species.2.n) # number of subgroups per species group (2D)
  i <- 0
  for (j in 1:species.n) {
    if (species.2.N[j] > 0) {
      i <- i + 1
      species.2.groups[i] <- species.2.N[j]
    }
  }
  species.2.data <- array(data = NA, dim = c(species.2.all.n, numboxes))
  species.2.att <- rep("", species.2.all.n)
  i <- 0
  for (sp in species.2.names.full) {
    i <- i + 1
    tmp <- ncvar_get(nc.out, sp) # get the associated data set
    # add the fill value to the missing values
    if (ncatt_get(nc.out, varid = sp, attname = "_FillValue")$hasatt) {
      tmp[is.na(tmp)] <- ncatt_get(nc.out, varid = sp, attname = "_FillValue")$value
    } else {
      tmp[is.na(tmp)] <- 0
    }
    tmp[not.valid.2] <- NA # remove data from non-data boxes

    tmp.dims <- length(dim(tmp)) # dimensions of the associated data
    if (tmp.dims == 1) {
      # 2D species distribution with no time replicates
      species.2.data[i, ] <- tmp
    } else {
      # 2D with time replicates
      species.2.data[i, ] <- tmp[ ,1]
    }

    tmp <- ncatt_get(nc.out, sp)
    tmp.n <- length(tmp)
    tmp.names <- names(tmp)
    txt <- ""
    for (k in 1:tmp.n) {
      txt.tmp <- paste(tmp.names[k], ": ", as.character(tmp[k]), sep = "")
      txt <- paste(txt, txt.tmp, sep = "<br/>")
    }
    species.2.att[i] <- txt # add 2D species attribure info
  }

  # 3D species data
  species.3.n <- length(species.3.names)
  species.3.all.n <- sum(species.3.N)
  species.3.groups <- rep(0, species.3.n) # number of subgroups per species group (2D)
  i <- 0
  for (j in 1:species.n) {
    if (species.3.N[j] > 0) {
      i <- i + 1
      species.3.groups[i] <- species.3.N[j]
    }
  }
  species.3.data <- array(data = NA, dim = c(species.3.all.n, numboxes, numlevels))
  species.3.att <- rep("", species.3.all.n)
  i <- 0
  for (sp in species.3.names.full) {
    i <- i + 1
    tmp <- ncvar_get(nc.out, sp) # get the associated data set
    # add the fill value to the missing values
    if (ncatt_get(nc.out, varid = sp, attname = "_FillValue")$hasatt) {
      tmp[is.na(tmp)] <- ncatt_get(nc.out, varid = sp, attname = "_FillValue")$value
    } else {
      tmp[is.na(tmp)] <- 0
    }
    tmp[not.valid.3] <- NA
    tmp.dims <- length(dim(tmp)) # dimensions of the associated data
    if (tmp.dims == 2) {
      # 3D species distribution with no time replicates
      for (j in 1:numlevels) {
        species.3.data[i, ,j] <- tmp[j, ]
      }
    } else {
      # 3D with time replicates
      for (j in 1:numlevels) {
        species.3.data[i, ,j] <- tmp[j, ,1]
      }
    }

    tmp <- ncatt_get(nc.out, sp)
    tmp.n <- length(tmp)
    tmp.names <- names(tmp)
    txt <- ""
    for (k in 1:tmp.n) {
      txt.tmp <- paste(tmp.names[k], ": ", as.character(tmp[k]), sep = "")
      txt <- paste(txt, txt.tmp, sep = "<br/>")
    }
    species.3.att[i] <- txt
  }

  # get structural and reserve nitrogen weights across cohorts
  Species <- NA # species name
  Cohort <- NA  # cohort index
  N.Type <- NA  # structural or reserve
  N.Value <- NA # mg N
  for (sp in species.3.names) {
    for (i in 1:30) { # only consider up to 30 cohorts
      # look for structural data
      txt.find <- paste(sp, as.character(i), "_ResN", sep = "")
      j <- grep(txt.find, species.3.names.full)
      if (length(j) == 1) {
        Species <- c(Species, sp)
        Cohort <- c(Cohort, i)
        N.Type <- c(N.Type, "Reserve")
        tmp <- ncvar_get(nc.out, txt.find) # get the associated data set
        tmp.dims <- length(dim(tmp)) # dimensions of the associated data
        if (tmp.dims == 3) {
          tmp <- tmp[ , ,1] # remove time dimension
        }
        if (ncatt_get(nc.out, varid = txt.find, attname = "_FillValue")$hasatt) {
          tmp[is.na(tmp)] <- ncatt_get(nc.out, varid = txt.find, attname = "_FillValue")$value
        } else {
          tmp[is.na(tmp)] <- 0
        }
        tmp[not.valid.3] <- NA
        if (sum(!is.na(tmp)) > 0) {
          N.val <- max(tmp[ , ], na.rm = TRUE)
        } else {
          N.val <- 0.0 # No data provided
        }
        N.Value <- c(N.Value, N.val)
      }

      # look for reserve nitrogen
      txt.find <- paste(sp, as.character(i), "_StructN", sep = "")
      j <- grep(txt.find, species.3.names.full)
      if (length(j) == 1) {
        Species <- c(Species, sp)
        Cohort <- c(Cohort, i)
        N.Type <- c(N.Type, "Structural")
        tmp <- ncvar_get(nc.out, txt.find) # get the associated data set
        tmp.dims <- length(dim(tmp)) # dimensions of the associated data
        if (tmp.dims == 3) {
          tmp <- tmp[ , ,1] # remove time dimension
        }
        if (ncatt_get(nc.out, varid = txt.find, attname = "_FillValue")$hasatt) {
          tmp[is.na(tmp)] <- ncatt_get(nc.out, varid = txt.find, attname = "_FillValue")$value
        } else {
          tmp[is.na(tmp)] <- 0
        }
        tmp[not.valid.3] <- NA
        if (sum(!is.na(tmp)) > 0) {
          N.val <- max(tmp[ , ], na.rm = TRUE)
        } else {
          N.val <- 0.0 # No data provided
        }
        N.Value <- c(N.Value, N.val)
      }
    }
  }

  df.nitrogen <- data.frame(Species, Cohort, N.Type, N.Value)
  df.nitrogen <- df.nitrogen[-1,]

  # get numbers across cohorts
  Species <- NA # species name
  Cohort <- NA  # cohort index
  Nums.Value <- NA # mg N
  for (sp in species.3.names) {
    for (i in 1:30) { # only consider up to 30 cohorts
      # look for structural data
      txt.find <- paste(sp, as.character(i), "_Nums", sep = "")
      j <- grep(txt.find, species.3.names.full)
      if (length(j) == 1) {
        Species <- c(Species, sp)
        Cohort <- c(Cohort, i)
        N.Type <- c(N.Type, "Reserve")
        tmp <- ncvar_get(nc.out, txt.find) # get the associated data set
        tmp.dims <- length(dim(tmp)) # dimensions of the associated data
        if (tmp.dims == 3) {
          tmp <- tmp[ , ,1] # remove time dimension
        }
        if (ncatt_get(nc.out, varid = txt.find, attname = "_FillValue")$hasatt) {
          tmp[is.na(tmp)] <- ncatt_get(nc.out, varid = txt.find, attname = "_FillValue")$value
        } else {
          tmp[is.na(tmp)] <- 0
        }
        tmp[not.valid.3] <- NA
        if (sum(!is.na(tmp)) > 0) {
          N.val <- sum(tmp[ , ], na.rm = TRUE)
        } else {
          N.val <- 0.0 # No data provided
        }
        Nums.Value <- c(Nums.Value, N.val)
      }
    }
  }

  df.nums <- data.frame(Species, Cohort, Nums.Value)
  df.nums <- df.nums[-1,]

  nc_close(nc.out)

  return(list(
    nonspecies.2.names = nonspecies.2.names,
    nonspecies.3.names = nonspecies.3.names,
    nonspecies.2.data = nonspecies.2.data,
    nonspecies.3.data = nonspecies.3.data,
    nonspecies.2.att = nonspecies.2.att,
    nonspecies.3.att = nonspecies.3.att,
    species.2.names = species.2.names,
    species.3.names = species.3.names,
    species.2.names.full = species.2.names.full,
    species.3.names.full = species.3.names.full,
    species.2.groups = species.2.groups,
    species.3.groups = species.3.groups,
    species.2.data = species.2.data,
    species.3.data = species.3.data,
    species.2.att = species.2.att,
    species.3.att = species.3.att,
    df.nitrogen = df.nitrogen,
    df.nums = df.nums
  ))
}

# +=====================================================+
# |  make.sh.init.object : collect all data to display  |
# +=====================================================+
#' @title Function that generates a list object used by sh.init
#'
#' @description
#' Takes data from a box geometry model and a netCDF Atlantis input parameter file and generates a
#' list object that is the parameter to \code{\link[shinyrAtlantis]{sh.init}} (see Examples).
#'
#' @param bgm.file Box geometry model (.bgm) file used by Atlantis that defines box boundaries and depths.
#' @param nc.file NetCDF (.nc) file used by Atlantis to set initial conditions.
#'
#' @return R list object.
#'
#' @examples
#' \dontrun{
#' bgm.file <- "VMPA_setas.bgm"
#' nc.file  <- "INIT_VMPA_Jan2015.nc"
#' input.object <- make.sh.init.object(bgm.file, nc.file)
#' sh.init(input.object)
#' }
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by
#' @importFrom stringr str_sub
#' @importFrom ncdf4 ncatt_get
#' @importFrom stringr str_length
make.sh.init.object <- function(bgm.file, nc.file) {
  cat("-- Extracting map data\n")
  map.object <- make.init.map(bgm.file)

  cat("-- Extracting cover variables\n")
  cover.object <- make.init.cover(map.object$box.data, map.object$map.vertices,
    nc.file)

  cat("-- Extracting additional variables (this may take a few minutes)\n")
  data.object <- make.init.data(nc.file, map.object$numboxes, cover.object$numlevels)

  return(list(
    numboxes = map.object$numboxes,

    numlevels = cover.object$numlevels,
    depths = cover.object$depths,
    box.info = cover.object$box.info,
    df.map = cover.object$df.map,

    nonspecies.2.names = data.object$nonspecies.2.names,
    nonspecies.3.names = data.object$nonspecies.3.names,
    nonspecies.2.data = data.object$nonspecies.2.data,
    nonspecies.3.data = data.object$nonspecies.3.data,
    nonspecies.2.att = data.object$nonspecies.2.att,
    nonspecies.3.att = data.object$nonspecies.3.att,
    species.2.names = data.object$species.2.names,
    species.3.names = data.object$species.3.names,
    species.2.names.full = data.object$species.2.names.full,
    species.3.names.full = data.object$species.3.names.full,
    species.2.groups = data.object$species.2.groups,
    species.3.groups = data.object$species.3.groups,
    species.2.data = data.object$species.2.data,
    species.3.data = data.object$species.3.data,
    species.2.att = data.object$species.2.att,
    species.3.att = data.object$species.3.att,
    df.nitrogen = data.object$df.nitrogen,
    df.nums = data.object$df.nums
  ))
}
