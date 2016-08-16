# 03/05/2016
#
# +==================================================================+
# |  sh.prm : shiny application for viewing Atlantis prm input file  |
# +==================================================================+
#' @title Shiny application for viewing Atlantis biological parameters
#'
#' @description
#' Uses data from an Atlantis box geometry model file, a group file, and a biological parameter file
#' and provides a visualisation of the data in the form of a shiny application.
#' The three data files must first be pre-processed by \code{\link[shinyrAtlantis]{make.sh.prm.object}},
#' which generates a list object that is the parameter to \code{sh.prm} (see Examples).
#'
#' The \emph{Map} tab displays the spatial distribution of box depths.
#' 
#' The \emph{Groups} tab displays the information stored in the Atlantis group file.
#' 
#' The \emph{Global params} tab shows the global variables that have values provided
#' in the biological parameters file.
#' 
#' The \emph{Group params} tab allows the user to see a list of possible parameters associated with groups
#' (Definitions) and the values provided in the biological parameters file (Values). 
#' Use the checkboxes to view the group parameters of interest.
#' 
#' The \emph{Habitat} tab shows which groups are associated with habitats.
#' 
#' The \emph{Distributions} tab shows horizontal and vertical distributions for
#' chosen groups. Initial values, recruitment and migration parameters can be viewed here.
#'
#' The \emph{Feeding} tab shows clearance rates, maximum growth rates, 
#' prey availability and refuge-related parameters.
#' 
#' The \emph{Migration} tab provides migrant biomasses (structural and reserve) 
#' and numbers.
#' 
#' The \emph{Recruitment} tab shows the relation between structural and
#' reserve biomass across all groups.
#' 
#' @param obj R list object generated from \code{\link[shinyrAtlantis]{make.sh.prm.object}}
#'
#' @return object of class 'shiny.appobj' see \code{\link[shiny]{shinyApp}}
#'
#' @examples
#' \dontrun{
#' bgm.file <- "SEAP_extended_shelf.bgm"
#' grp.file <- "SEAP_Groups_Aquacult.csv"
#' prm.file <- "SEAP_biol_pH_Aquacult.prm"
#' obj <- make.sh.prm.object(bgm.file, grp.file, prm.file)
#' sh.prm(obj)
#' }
#' @export
#' @importFrom ggplot2 xlim element_rect geom_hline position_jitter scale_x_log10 scale_y_log10 geom_bar coord_flip scale_y_continuous
sh.prm <- function(obj){
  # obj is a list: numboxes, map_base, box.data, grp.def, grp.att, 
  #   gen.prm, grp.hab, grp.dist
  
  # set up variables needed to plot the map 
  df.map    <- merge(obj$map_base, obj$box.data, by = "boxid")
  df.map$z <- -df.map$z # change to absolute depth
  df.map$z[df.map$z == 0] <- NA
  depth.min <- min(df.map$z) # deepest box
  depth.max <- max(df.map$z) # shallowest box
  
  numboxes <- obj$numboxes
  flagfishrates <- obj$flagfishrates
  flag_fine_ontogenetic_diets <- obj$gen.prm$Value[which(obj$gen.prm$Parameter == "flag_fine_ontogenetic_diets")]
  no.clearance.data <- sum(obj$clearance.data$co.1, na.rm = TRUE) == 0  
  
  # checkbox labels for group templates
  def.grp.file <- system.file("extdata", "grpTemplates.csv", package = "shinyrAtlantis")
  df.prms.all <- read.csv(file = def.grp.file, header = TRUE)
  tmplts <- df.prms.all$Template
  tmplts <- setNames(1:length(tmplts), tmplts)
  
  # select input names for horizontal
  grp.codes <- setNames(1:length(obj$grp.def$Code), obj$grp.def$Code)
  grp.codes.ord <- sort(obj$grp.def$Code)
  grp.codes.ord <- setNames(1:length(grp.codes.ord), grp.codes.ord)
  
  # food availability groups
  prey.types <- c(as.character(obj$grp.def$Code), "DLsed", "DRsed", "DCsed")
  prey.names <- c(as.character(obj$grp.def$Name), "DLsed", "DRsed", "DCsed")
  group.types <- c(as.character(obj$grp.def$GroupType), "LAB_DET", "REF_DET", "CARRION")
  
  # create HTML text for viewing on help tab  
  txtHelp <- "<p>This tool displays data from the .prm file used to provide initial conditions for an <b>Atlantis</b> run.</p>"
  txtHelp <- paste(txtHelp, "<p>Some plots have a zoom feature. Draw a box and double click to zoom into the box. Double click to reset zoom.</p>") 
  txtHelp <- paste(txtHelp, "<p>This tool is not completed and the following problems are currently being fixed or implemented:</p>")
  txtHelp <- paste(txtHelp, "<ul><li>Update the definitions of general and group parameters. Definitions are often unclear and in some cases missing.</li>")
  txtHelp <- paste(txtHelp, "<li>Add information on growth</li>")
  txtHelp <- paste(txtHelp, "<li>Add information on migration</li>")
  txtHelp <- paste(txtHelp, "<li>Improve error checking so that warnings are not produced</li></ul>")
  
  shinyApp(
    
    # USER INPUT FUNCTION
    
    ui = navbarPage(
      title = "Atlantis parameter viewer",
      # MAP
      tabPanel("Map", 
        fluidRow(  
          column(3,
            h4("Depth distribution"),
            HTML("<p>This plot has a zoom feature.</p><p>Draw a box and double click to zoom into the box.</p><p>Double click to reset the zoom.</p>") 
          ),
          column(7,
            plotOutput("box_location_plot", 
              height = "600px",
              dblclick = "box_location_plot_dblclick",
              brush = brushOpts(
                id = "box_location_plot_brush",
                resetOnNew = TRUE
              )
            )
          )
        )
      ),
      # GROUP DEFINITIONS
      tabPanel("Groups",
        fluidRow(
          column(12, DT::dataTableOutput('table.grp'))
        )
      ),
      # GLOBAL PARAMS
      tabPanel("Global params",
        fluidRow(
          column(12, DT::dataTableOutput('table.gen.prm'))
        )
      ),
      # GROUP PARAMETERS
      navbarMenu("Group params",
        tabPanel("Definitions",
          fluidRow(
            column(12, DT::dataTableOutput('table.grp.def'))
          )
        ),
        tabPanel("Values",
          fluidPage(
            title = 'Title',
            sidebarLayout(
              sidebarPanel(
                checkboxGroupInput("checkGroup", label = "Parameter templates", 
                  choices = tmplts, selected = 1), width= 3
              ),
              mainPanel(
                fluidRow(
                  column(12, DT::dataTableOutput('table.prm'))
                )
              )
            )
          )
        )
      ),
      # HABITAT
      tabPanel("Habitat",
        fluidRow(
          column(12, wellPanel(
            h4("habitat_XXX")))
        ),        
        fluidRow(
          column(12, DT::dataTableOutput('table.hab'))
        ),
        fluidRow(
          column(12, wellPanel(
            h4("ad_habitat_XXX")))
        ),        
        fluidRow(
          column(12, DT::dataTableOutput('table.hab.ad'))
        ),
        fluidRow(
          column(12, wellPanel(
            h4("juv_habitat_XXX")))
        ),        
        fluidRow(
          column(12, DT::dataTableOutput('table.hab.juv'))
        )
      ),
      # DISTRIBUTION
      tabPanel("Distributions",
        sidebarPanel(
          selectInput(inputId = 'SIHGroup', label = 'Group: XXX', 
            choices = grp.codes.ord),
          numericInput('NIH', 'Quarter: Q', 1, min = 1, max = 4),
          width = 2
        ),
        mainPanel(
          fluidRow(
            column(10, h3(textOutput('VTO.dist.species')))
          ),
          hr(),
          fluidRow(column(12, h3("Horizontal and vertical distributions"))),
          fluidRow(
            column(5, h4("Invertebrate/Adult")),
            column(5, h4("Juvenile"))
          ),
          fluidRow(
            column(5, 
              plotOutput("H_ad_distribution_plot", 
                height = "325px",
                dblclick = "H_ad_distribution_plot_dblclick",
                brush = brushOpts(
                  id = "H_ad_distribution_plot_brush",
                  resetOnNew = TRUE
                )
              )
            ),
            column(5, 
              plotOutput("H_juv_distribution_plot", 
                height = "325px",
                dblclick = "H_juv_distribution_plot_dblclick",
                brush = brushOpts(
                  id = "H_juv_distribution_plot_brush",
                  resetOnNew = TRUE
                )
              )
            )
          ),
          fluidRow(
            column(5, plotOutput("vert_distribution_plot", height = "325px")),
            column(5, plotOutput("vert_juv_distribution_plot", height = "325px"))
          ),
          fluidRow(column(12, h4("Corresponding density plots [per m^2]"))),
          fluidRow(
            column(5, 
              plotOutput("H_ad_distribution_area_plot", height = "325px")),
            column(5, 
              plotOutput("H_juv_distribution_area_plot", height = "325px"))
          ),
          hr(),
          fluidRow(column(12, h3("Recruitment"))),
          fluidRow(
            column(5, 
              plotOutput("H_recruit_plot", 
                height = "325px",
                dblclick = "H_recruit_plot_dblclick",
                brush = brushOpts(
                  id = "H_recruit_plot_brush",
                  resetOnNew = TRUE
                )
              )
            ),
            column(5, 
              plotOutput("H_recruit_area_plot", height = "325px"))
          ),
          fluidRow(
            column(5, 
              plotOutput("vertical_recruit_plot", height = "325px"))
          ),
          hr(),
          fluidRow(column(12, h3("Migration: fraction of occupants that exit when group migrates out of model domain"))),
          fluidRow(
            column(5, 
              plotOutput("migration.ad.plot", height = "325px")),
            column(5, 
              plotOutput("migration.juv.plot", height = "325px"))
          ),
          fluidRow(
            column(5, 
              plotOutput("migration.plot", height = "325px"))
          ),
          width = 10
        )
      ),
      navbarMenu("Feeding",
        # Clearance rates
        tabPanel("Clearance Rates",
          fluidPage(
            fluidRow(column(12, h4("Clearance rates: C_XXX"))),
            if (flagfishrates == "0") {
              HTML( paste("<p><b>flagfishrates</b> = ", flagfishrates, 
                " so clearance rates are absolute [m^3 per day per undividual].</p>", sep = ""))  
            } else {
              HTML( paste("<p><b>flagfishrates</b> = ", flagfishrates, 
                " so clearance rates are proportional to body mass [m^3 per day per (mg N)].</p>", sep = ""))  
            }
          ),
          hr(),
          fluidRow(column(12, plotOutput("clearance.plot", height = "800px"))
          ),  
          fluidRow(
            column(12, DT::dataTableOutput('table.clearance'))
          )
        ),
        # Maximum growth rates
        tabPanel("Maximum growth rates",
          fluidPage(
            fluidRow(column(12, h4("Maximum growth rates: mum_XXX")))
          ),
          fluidRow(
            column(12, plotOutput("growth.max.plot", height = "800px"))
          ),
          fluidRow(
            column(12, DT::dataTableOutput('table.growth.max'))
          )
        ),
        # Availibiity
        tabPanel("Availability (base-line)",
          fluidRow(column(12, 
            if (flag_fine_ontogenetic_diets == 0) {
              HTML( paste("<p><b>flag_fine_ontogenetic_diets</b> = ", flag_fine_ontogenetic_diets, 
                " so only juvenile and adult diets.</p>", sep = ""))  
            } else {
              HTML( paste("<p><b>flag_fine_ontogenetic_diets</b> = ", flag_fine_ontogenetic_diets, 
                " so cohort-dependent diets.</p>", sep = ""))  
            })
          ),
          fluidRow(
            column(2, selectInput(inputId = 'SIPGroup', label = 'Group: XXX', 
              choices = grp.codes.ord)),
            column(10, h3(textOutput('VTO.prey.species')))
          ),
          hr(),
          fluidRow(column(12, h4("pPREY[1,2]XXX[1,2]"))),
          fluidRow(column(12, 
            plotOutput("avail.prey.plot", height = "600px"))
          ),  
          fluidRow(
            column(12, DT::dataTableOutput('table.avail.prey'))
          ),
          hr(),
          fluidRow(column(12, h4("p_YYYXXX"))),
          fluidRow(column(12, 
            plotOutput("avail.age.plot", height = "400px"))
          ),  
          fluidRow(
            column(12, DT::dataTableOutput('table.age.prey'))
          )
        ),
        # Refuge
        tabPanel("Availability modifiers",
          fluidRow(column(12, h4("Cover modifier"))),
          fluidRow(column(12, 
            plotOutput("refuge.1.plot", height = "625px"))),
          hr(),
          fluidRow(column(12, h4("Refuge modifier"))),
          fluidRow(column(12, 
            plotOutput("refuge.2.plot", height = "200px")))
        )
      ),
      # Migration
      tabPanel("Migration",
        fluidRow(
          column(12, HTML("<p><b>Note:</b> the spatial patterns of migration can be found under the <em>Distributions</em> tab (at the bottom).</p>"))
        ),
        fluidRow(
          column(12, wellPanel(
            h4("KMIGa_INVERT_XXX: migrant biomass")))
        ),        
        fluidRow(
          column(12, plotOutput("mig.bio.plot", height = "250px"))
        ),
        hr(),
        fluidRow(
          column(12, wellPanel(
            h4("KMIGa_XXX: migrant numbers")))
        ),        
        fluidRow(
          column(12, plotOutput("mig.num.plot", height = "750px"))
        ),
        hr(),
        fluidRow(
          column(12, wellPanel(
            h4("KMIGa_XXXsn, KMIGa_XXXrn: migrant state (mg N)")))
        ),    
        fluidRow(
          column(12, HTML("<p><b>Key:</b> black = reserve, blue = structural</p>")),
          column(12, plotOutput("mig.plot", height = "750px"))
        )
      ),
      # Recruitment
      tabPanel("Recruitment",
        fluidRow(
          column(12, wellPanel(
            h4("Weight of recruits (per individual)")))
        ),        
        fluidRow(
          column(3, 
            HTML("<p>Group names have been jittered in the vertical.</p>"),
            HTML("<p>x-axis is KWSR_XXX and y-axis is KWRR_XXX.</p>")
          ),
          column(7, plotOutput("recruit.mass.plot", height = "400px"))
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
    
    server = function(input, output) {
      ranges_box_location_plot <- reactiveValues(x = NULL, y = NULL)
      ranges_H_ad_distribution_plot <- reactiveValues(x = NULL, y = NULL)
      ranges_H_juv_distribution_plot <- reactiveValues(x = NULL, y = NULL)
      ranges_H_recruit_plot <- reactiveValues(x = NULL, y = NULL) 
      
      # When a double-click happens, check if there's a brush on the plot.
      # If so, zoom to the brush bounds; if not, reset the zoom.
      observeEvent(input$box_location_plot_dblclick, {
        brush <- input$box_location_plot_brush
        if (!is.null(brush)) {
          ranges_box_location_plot$x <- c(brush$xmin, brush$xmax)
          ranges_box_location_plot$y <- c(brush$ymin, brush$ymax)
        } else {
          ranges_box_location_plot$x <- NULL
          ranges_box_location_plot$y <- NULL
        }
      })
      
      # When a double-click happens, check if there's a brush on the plot.
      # If so, zoom to the brush bounds; if not, reset the zoom.
      observeEvent(input$H_ad_distribution_plot_dblclick, {
        brush <- input$H_ad_distribution_plot_brush
        if (!is.null(brush)) {
          ranges_H_ad_distribution_plot$x <- c(brush$xmin, brush$xmax)
          ranges_H_ad_distribution_plot$y <- c(brush$ymin, brush$ymax)
        } else {
          ranges_H_ad_distribution_plot$x <- NULL
          ranges_H_ad_distribution_plot$y <- NULL
        }
      })
      
      # When a double-click happens, check if there's a brush on the plot.
      # If so, zoom to the brush bounds; if not, reset the zoom.
      observeEvent(input$H_juv_distribution_plot_dblclick, {
        brush <- input$H_juv_distribution_plot_brush
        if (!is.null(brush)) {
          ranges_H_juv_distribution_plot$x <- c(brush$xmin, brush$xmax)
          ranges_H_juv_distribution_plot$y <- c(brush$ymin, brush$ymax)
        } else {
          ranges_H_juv_distribution_plot$x <- NULL
          ranges_H_juv_distribution_plot$y <- NULL
        }
      })
      
      # When a double-click happens, check if there's a brush on the plot.
      # If so, zoom to the brush bounds; if not, reset the zoom.
      observeEvent(input$H_recruit_plot_dblclick, {
        brush <- input$H_recruit_plot_brush
        if (!is.null(brush)) {
          ranges_H_recruit_plot$x <- c(brush$xmin, brush$xmax)
          ranges_H_recruit_plot$y <- c(brush$ymin, brush$ymax)
        } else {
          ranges_H_recruit_plot$x <- NULL
          ranges_H_recruit_plot$y <- NULL
        }
      })
      
      output$table.grp <- DT::renderDataTable({
        DT::datatable(obj$grp.def, rownames = FALSE)
      })  
      
      output$table.grp.def <- DT::renderDataTable({
        DT::datatable(df.prms.all, rownames = FALSE)
      })  
      
      output$table.prm <- DT::renderDataTable({
        DT::datatable(obj$grp.att[c(1,2,3,as.integer(input$checkGroup)+3)], 
          rownames = FALSE)
      })
      
      output$table.gen.prm <- DT::renderDataTable({
        DT::datatable(obj$gen.prm, rownames = FALSE)
      })
      
      output$table.hab <- DT::renderDataTable({
        DT::datatable(obj$grp.hab$df.hab, rownames = FALSE)
      })
      
      output$table.hab.ad <- DT::renderDataTable({
        DT::datatable(obj$grp.hab$df.hab.ad, rownames = FALSE)
      })
      
      output$table.hab.juv <- DT::renderDataTable({
        DT::datatable(obj$grp.hab$df.hab.juv, rownames = FALSE)
      })
      
      output$table.clearance <- DT::renderDataTable({
        DT::datatable(obj$clearance.data, rownames = FALSE)
      })
      
      output$table.growth.max <- DT::renderDataTable({
        DT::datatable(obj$grp.growth, rownames = FALSE)
      })
      
      observeEvent(input$exitButton, {
        stopApp()
      })
      
      output$box_location_plot <- renderPlot({
        ggplot(data = df.map, aes(x = x, y = y, group = boxid, fill = z)) +
          geom_polygon(colour = "grey90", size = 0.25) +          
          scale_fill_gradient(low = "#9ecae1", high = "#084594", na.value="grey90") +
          labs(fill = "Depth (m)") +
          geom_text(aes(x = x.in, y = y.in, label = boxid), size = 2.5) +
          theme_bw() + xlab("Longitude") + ylab("Latitude") + 
          theme(plot.background = element_blank()) + 
          coord_cartesian(xlim = ranges_box_location_plot$x, 
            ylim = ranges_box_location_plot$y) # + 
        # scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL)
      })
      
      # There must be an easier way?
      output$VTO.dist.species <- renderText({
        as.character(obj$grp.def$Long.Name[which(obj$grp.def$Code == names(grp.codes.ord[as.integer(input$SIHGroup)]))])
      })
      
      output$VTO.prey.species <- renderText({
        as.character(obj$grp.def$Long.Name[which(obj$grp.def$Code == names(grp.codes.ord[as.integer(input$SIPGroup)]))])
      })
      
      output$H_ad_distribution_plot <- renderPlot({
        xxx <- as.integer(input$SIHGroup)
        n <- as.integer(input$NIH)
        df.ad <- data.frame(
          boxid = 0:(numboxes - 1), 
          p = obj$grp.dist$hor.ad[xxx,n,])
        df.map.ad <- merge(obj$map_base, df.ad, by = "boxid")
        if (sum(df.ad$p, na.rm = TRUE) == 0) {
          ggplot(data = df.map.ad, aes(x = x, y = y, group = boxid)) +
            geom_polygon(colour = "grey90", size = 0.25, fill = "red") +          
            ggtitle("FXXX_SQ: No data provided") + 
            theme_bw() + xlab("") + ylab("") +
            theme(
              plot.background = element_blank(),
              plot.title=element_text(colour="red")
            ) + 
            scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL)
        } else {
          ggplot(data = df.map.ad, aes(x = x, y = y, group = boxid, fill = p)) +
            geom_polygon(colour = "grey50", size = 0.25) +          
            scale_fill_gradient(high = "blue", low = "white", na.value="grey90") +
            labs(fill = "Fraction") + ggtitle("FXXX_SQ") + 
            theme_bw() + xlab("") + ylab("") +
            theme(plot.background = element_blank()) + 
            scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL) +
            coord_cartesian(xlim = ranges_H_ad_distribution_plot$x, 
              ylim = ranges_H_ad_distribution_plot$y)
        }  
      })
      
      output$H_juv_distribution_plot <- renderPlot({
        xxx <- as.integer(input$SIHGroup)
        n <- as.integer(input$NIH)
        df.juv <- data.frame(
          boxid = 0:(numboxes-1), 
          p = obj$grp.dist$hor.juv[xxx,n,])
        df.map.juv <- merge(obj$map_base, df.juv, by = "boxid")
        if (sum(df.juv$p, na.rm = TRUE) == 0) {
          ggplot(data = df.map.juv, aes(x = x, y = y, group = boxid)) +
            geom_polygon(colour = "grey90", size = 0.25, fill = "red") +          
            ggtitle("FXXX_SQjuv: No data provided") + 
            theme_bw() + xlab("") + ylab("") +
            theme(
              plot.background = element_blank(),
              plot.title=element_text(colour="red")
            ) + 
            scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL)
        } else {
          ggplot(data = df.map.juv, aes(x = x, y = y, group = boxid, fill = p)) +
            geom_polygon(colour = "grey50", size = 0.25) +          
            scale_fill_gradient(high = "blue", low = "white", na.value="grey90") +
            labs(fill = "Fraction") + ggtitle("FXXX_SQjuv") + 
            theme_bw() + xlab("") + ylab("") +
            theme(plot.background = element_blank()) + 
            scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL) +
            coord_cartesian(xlim = ranges_H_juv_distribution_plot$x, 
              ylim = ranges_H_juv_distribution_plot$y)
        }  
      })
      
      output$vert_distribution_plot <- renderPlot({
        xxx <- as.integer(input$SIHGroup)
        levs <- sum(!is.na(obj$grp.dist$vert.day[xxx, ]))
        df.day <- data.frame(
          depth = 1:levs, 
          day = obj$grp.dist$vert.day[xxx,1:levs])
        df.night <- data.frame(
          depth = 1:levs, 
          night = obj$grp.dist$vert.night[xxx,1:levs])
        df.vert <- merge(df.day, df.night, by = "depth")
        df.vert <- tidyr::gather(df.vert, period, fraction, 2:3)
        if (sum(df.vert$fraction, na.rm = TRUE) == 0) {
          df.blank <- data.frame(x = 1, y = 0)
          ggplot(data = df.blank, aes(x = x, y = y)) +
            geom_point(size = 0) + coord_flip() +
            ggtitle("VERTday_XXX[2], VERTnight_XXX[2]:\nNo data provided") + 
            theme_bw() + xlim(1,levs) + ylim(0,1) +
            xlab("Depth layer (deep:shallow)") + ylab("Fraction") +
            theme(
              plot.background = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title=element_text(colour="red")
            )
        } else {
          ggplot(data = df.vert, aes(x = depth, y = fraction, colour = period)) +
            geom_line() + geom_point() + ylim(0,1) + xlim(1,levs) + coord_flip() +
            ggtitle("VERTday_XXX[2], VERTnight_XXX[2]") + 
            scale_color_manual(values=c("#FF6666", "blue")) + 
            xlab("Depth layer (deep:shallow)") + ylab("Fraction") +
            theme_bw() + 
            theme(
              panel.grid.minor = element_blank(),
              plot.background = element_blank()
            )
        }  
      })
      
      output$vert_juv_distribution_plot <- renderPlot({
        xxx <- as.integer(input$SIHGroup)
        levs <- sum(!is.na(obj$grp.dist$vert.juv.day[xxx, ]))
        df.day <- data.frame(
          depth = 1:levs, 
          day = obj$grp.dist$vert.juv.day[xxx,1:levs])
        df.night <- data.frame(
          depth = 1:levs, 
          night = obj$grp.dist$vert.juv.night[xxx,1:levs])
        df.vert <- merge(df.day, df.night, by = "depth")
        df.vert <- tidyr::gather(df.vert, period, fraction, 2:3)
        if (sum(df.vert$fraction, na.rm = TRUE) == 0) {
          df.blank <- data.frame(x = 1, y = 0)
          ggplot(data = df.blank, aes(x = x, y = y)) +
            geom_point(size = 0) + coord_flip() +
            ggtitle("VERTday_XXX1, VERTnight_XXX1:\nNo data provided") + 
            theme_bw() + xlim(1,levs) + ylim(0,1) +
            xlab("Depth layer (deep:shallow)") + ylab("Fraction") +
            theme(
              plot.background = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title=element_text(colour="red")
            )
        } else {
          ggplot(data = df.vert, aes(x = depth, y = fraction, colour = period)) +
            geom_line() + geom_point() + ylim(0,1) + xlim(1,levs) +
            ggtitle("VERTday_XXX1, VERTnight_XXX1") + coord_flip() +
            scale_color_manual(values=c("#FF6666", "blue")) + 
            xlab("Depth layer (deep:shallow)") + ylab("Fraction") +
            theme_bw() + 
            theme(
              panel.grid.minor = element_blank(),
              plot.background = element_blank()
            )
        }  
      })
      
      output$H_ad_distribution_area_plot <- renderPlot({
        xxx <- as.integer(input$SIHGroup)
        n <- as.integer(input$NIH)
        df.ad <- data.frame(
          boxid = 0:(numboxes - 1), 
          p = obj$grp.dist$hor.ad[xxx,n,])
        df.ad$p <- df.ad$p/obj$box.data$area # 2D-density
        df.map.ad <- merge(obj$map_base, df.ad, by = "boxid")
        if (sum(df.ad$p, na.rm = TRUE) == 0) {
          ggplot(data = df.map.ad, aes(x = x, y = y, group = boxid)) +
            geom_polygon(colour = "grey90", size = 0.25, fill = "red") +          
            ggtitle("No data provided") + 
            theme_bw() + xlab("") + ylab("") +
            theme(
              plot.background = element_blank(),
              plot.title=element_text(colour="red")
            ) + 
            scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL)
        } else {
          ggplot(data = df.map.ad, aes(x = x, y = y, group = boxid, fill = p)) +
            geom_polygon(colour = "grey90", size = 0.25) +          
            scale_fill_gradient(high = "blue", low = "white", na.value="grey90") +
            labs(fill = "Density [m^-2]") + ggtitle("Invertebrate/Adult") +
            theme_bw() + xlab("") + ylab("") +
            theme(plot.background = element_blank()) + 
            scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL) +
            coord_cartesian(xlim = ranges_H_ad_distribution_plot$x, 
              ylim = ranges_H_ad_distribution_plot$y)
        }  
      })
      
      output$H_juv_distribution_area_plot <- renderPlot({
        xxx <- as.integer(input$SIHGroup)
        n <- as.integer(input$NIH)
        df.juv <- data.frame(
          boxid = 0:(numboxes-1), 
          p = obj$grp.dist$hor.juv[xxx,n,])
        df.juv$p <- df.juv$p/obj$box.data$area # 2D-density
        df.map.juv <- merge(obj$map_base, df.juv, by = "boxid")
        if (sum(df.juv$p, na.rm = TRUE) == 0) {
          ggplot(data = df.map.juv, aes(x = x, y = y, group = boxid)) +
            geom_polygon(colour = "grey90", size = 0.25, fill = "red") +          
            ggtitle("No data provided") + 
            theme_bw() + xlab("") + ylab("") +
            theme(
              plot.background = element_blank(),
              plot.title=element_text(colour="red")
            ) + 
            scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL)
        } else {
          ggplot(data = df.map.juv, aes(x = x, y = y, group = boxid, fill = p)) +
            geom_polygon(colour = "grey90", size = 0.25) +          
            scale_fill_gradient(high = "blue", low = "white", na.value="grey90") +
            labs(fill = "Density [m^-2]") + ggtitle("Juvenile") + 
            theme_bw() + xlab("") + ylab("") +
            theme(plot.background = element_blank()) + 
            scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL) +
            coord_cartesian(xlim = ranges_H_juv_distribution_plot$x, 
              ylim = ranges_H_juv_distribution_plot$y)
        }  
      })
      
      output$H_recruit_plot <- renderPlot({
        xxx <- as.integer(input$SIHGroup)
        df.recruit <- data.frame(
          boxid = 0:(numboxes - 1), 
          p = obj$grp.dist$hor.recruit[xxx,])
        df.map.recruit <- merge(obj$map_base, df.recruit, by = "boxid")
        if (sum(df.recruit$p, na.rm = TRUE) == 0) {
          ggplot(data = df.map.recruit, aes(x = x, y = y, group = boxid)) +
            geom_polygon(colour = "grey90", size = 0.25, fill = "red") +          
            ggtitle("XXX_recruit_hdistrib: No data provided") + 
            theme_bw() + xlab("") + ylab("") +
            theme(
              plot.background = element_blank(),
              plot.title=element_text(colour="red")
            ) + 
            scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL)
        } else {
          ggplot(data = df.map.recruit, aes(x = x, y = y, group = boxid, fill = p)) +
            geom_polygon(colour = "grey50", size = 0.25) +          
            scale_fill_gradient(high = "blue", low = "white", na.value="grey90") +
            labs(fill = "Fraction") + ggtitle("XXX_recruit_hdistrib") + 
            theme_bw() + xlab("") + ylab("") + 
            theme(plot.background = element_blank()) + 
            scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL) +
            coord_cartesian(xlim = ranges_H_recruit_plot$x, 
              ylim = ranges_H_recruit_plot$y)
        }  
      })
      
      output$migration.plot <- renderPlot({
        xxx <- as.integer(input$SIHGroup)
        df.migration <- data.frame(
          boxid = 0:(numboxes - 1), 
          p = obj$grp.dist$mig.invert[xxx,])
        df.map.migration <- merge(obj$map_base, df.migration, by = "boxid")
        
        if (sum(!is.na(df.migration$p)) == 0) {
          ggplot(data = df.map.migration, aes(x = x, y = y, group = boxid)) +
            geom_polygon(colour = "grey90", size = 0.25, fill = "red") +          
            ggtitle("MigIOBox_XXX: No data provided") + 
            theme_bw() + xlab("") + ylab("") +
            theme(
              plot.background = element_blank(),
              plot.title=element_text(colour="red")
            ) + 
            scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL)
        } else {
          ggplot(data = df.map.migration, aes(x = x, y = y, group = boxid, fill = p)) +
            geom_polygon(colour = "grey50", size = 0.25) +          
            scale_fill_gradient(high = "blue", low = "white", na.value="grey90") +
            labs(fill = "Fraction") + 
            ggtitle("MigIOBox_XXX") + 
            theme_bw() + xlab("") + ylab("") + 
            theme(plot.background = element_blank()) + 
            scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL) # +
          # coord_cartesian(xlim = ranges_H_recruit_plot$x, ylim = ranges_H_recruit_plot$y)
        }  
      })
      
      output$migration.ad.plot <- renderPlot({
        xxx <- as.integer(input$SIHGroup)
        df.migration <- data.frame(
          boxid = 0:(numboxes - 1), 
          p = obj$grp.dist$mig.ad[xxx,])
        df.map.migration <- merge(obj$map_base, df.migration, by = "boxid")
        
        if (sum(!is.na(df.migration$p)) == 0) {
          ggplot(data = df.map.migration, aes(x = x, y = y, group = boxid)) +
            geom_polygon(colour = "grey90", size = 0.25, fill = "red") +          
            ggtitle("MigIOBox_XXXad: No data provided") + 
            theme_bw() + xlab("") + ylab("") +
            theme(
              plot.background = element_blank(),
              plot.title=element_text(colour="red")
            ) + 
            scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL)
        } else {
          ggplot(data = df.map.migration, aes(x = x, y = y, group = boxid, fill = p)) +
            geom_polygon(colour = "grey50", size = 0.25) +          
            scale_fill_gradient(high = "blue", low = "white", na.value="grey90") +
            labs(fill = "Fraction") + 
            ggtitle("MigIOBox_XXXad") + 
            theme_bw() + xlab("") + ylab("") + 
            theme(plot.background = element_blank()) + 
            scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL) # +
          # coord_cartesian(xlim = ranges_H_recruit_plot$x, ylim = ranges_H_recruit_plot$y)
        }  
      })
      
      output$migration.juv.plot <- renderPlot({
        xxx <- as.integer(input$SIHGroup)
        df.migration <- data.frame(
          boxid = 0:(numboxes - 1), 
          p = obj$grp.dist$mig.juv[xxx,])
        df.map.migration <- merge(obj$map_base, df.migration, by = "boxid")
        
        if (sum(!is.na(df.migration$p)) == 0) {
          ggplot(data = df.map.migration, aes(x = x, y = y, group = boxid)) +
            geom_polygon(colour = "grey90", size = 0.25, fill = "red") +          
            ggtitle("MigIOBox_XXXjuv: No data provided") + 
            theme_bw() + xlab("") + ylab("") +
            theme(
              plot.background = element_blank(),
              plot.title=element_text(colour="red")
            ) + 
            scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL)
        } else {
          ggplot(data = df.map.migration, aes(x = x, y = y, group = boxid, fill = p)) +
            geom_polygon(colour = "grey50", size = 0.25) +          
            scale_fill_gradient(high = "blue", low = "white", na.value="grey90") +
            labs(fill = "Fraction") + 
            ggtitle("MigIOBox_XXXjuv") + 
            theme_bw() + xlab("") + ylab("") + 
            theme(plot.background = element_blank()) + 
            scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL) # +
          # coord_cartesian(xlim = ranges_H_recruit_plot$x, ylim = ranges_H_recruit_plot$y)
        }  
      })
      
      output$H_recruit_area_plot <- renderPlot({
        xxx <- as.integer(input$SIHGroup)
        df.recruit <- data.frame(
          boxid = 0:(numboxes - 1), 
          p = obj$grp.dist$hor.recruit[xxx,])
        df.recruit$p <- df.recruit$p/obj$box.data$area # 2D-density
        df.map.recruit <- merge(obj$map_base, df.recruit, by = "boxid")
        if (sum(df.recruit$p, na.rm = TRUE) == 0) {
          ggplot(data = df.map.recruit, aes(x = x, y = y, group = boxid)) +
            geom_polygon(colour = "grey90", size = 0.25, fill = "red") +          
            ggtitle("XXX_recruit_hdistrib: No data provided") + 
            theme_bw() + xlab("") + ylab("") +
            theme(
              plot.background = element_blank(),
              plot.title=element_text(colour="red")
            ) + 
            scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL)
        } else {
          ggplot(data = df.map.recruit, aes(x = x, y = y, group = boxid, fill = p)) +
            geom_polygon(colour = "grey90", size = 0.25) +          
            scale_fill_gradient(high = "blue", low = "white", na.value="grey90") +
            labs(fill = "Density [m^-2]") + ggtitle("Recruitment density") + 
            theme_bw() + xlab("") + ylab("") +
            theme(plot.background = element_blank()) + 
            scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL) +
            coord_cartesian(xlim = ranges_H_recruit_plot$x, 
              ylim = ranges_H_recruit_plot$y)
        }  
      })
      
      output$vertical_recruit_plot <- renderPlot({
        xxx <- as.integer(input$SIHGroup)
        levs <- sum(!is.na(obj$grp.dist$vert.recruit[xxx, ]))
        df.recruit <- data.frame(
          depth = 1:levs, 
          fraction = obj$grp.dist$vert.recruit[xxx,1:levs])
        if (sum(df.recruit$fraction, na.rm = TRUE) == 0) {
          df.blank <- data.frame(x = 1, y = 0)
          ggplot(data = df.blank, aes(x = x, y = y)) +
            geom_point(size = 0) + coord_flip() +
            ggtitle("XXX_recruit_vdistrib:\nNo data provided") + 
            theme_bw() + xlim(1,levs) + ylim(0,1) +
            xlab("Depth layer (deep:shallow)") + ylab("Fraction") +
            theme(
              plot.background = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title=element_text(colour="red")
            )
        } else {
          ggplot(data = df.recruit, aes(x = depth, y = fraction)) +
            geom_line(colour = "blue") + geom_point(colour = "blue") + ylim(0,1) + xlim(1,levs) +
            ggtitle("XXX_recruit_vdistrib") + coord_flip() +
            xlab("Depth layer (deep:shallow)") + ylab("Fraction") +
            theme_bw() + 
            theme(
              panel.grid.minor = element_blank(),
              plot.background = element_blank()
            )
        }  
      })
      
      output$refuge.1.plot <- renderPlot({
        j <- which(obj$gen.prm$Parameter == "flag_refuge_model", arr.ind = TRUE)
        flag_refuge_model <- obj$gen.prm$Value[j]
        
        j <- which(obj$gen.prm$Parameter == "flag_rel_cover", arr.ind = TRUE)
        flag_rel_cover <- obj$gen.prm$Value[j]
        
        if (flag_rel_cover == 0) {
          x.axis.text <- "Cumulative cover of the biogenic and abiotic habitats"
        } else {
          x.axis.text <- "Average cover of the biogenic and abiotic habitats"
        }
        ggplot(obj$refuge.data$model.1, aes(x = Cover, y = refuge_status, color = Stage)) + 
          geom_line() + 
          geom_hline(yintercept = 1, color = "grey", linetype = "dashed") +
          facet_wrap( ~ Code, ncol = 8) + ylim(0,NA) + 
          xlab(x.axis.text) + ylab("Relative availability") +
          theme_bw() + 
          scale_color_manual(values=c("Blue 3", "Red 3")) +
          theme(
            strip.text = element_text(colour = "white", face="bold"),
            strip.background = element_rect(fill="Red 3"),
            plot.background = element_blank()
          )
      })
      
      output$refuge.2.plot <- renderPlot({
        j <- which(obj$gen.prm$Parameter == "flag_refuge_model", arr.ind = TRUE)
        flag_refuge_model <- obj$gen.prm$Value[j]
        
        j <- which(obj$gen.prm$Parameter == "flag_rel_cover", arr.ind = TRUE)
        flag_rel_cover <- obj$gen.prm$Value[j]
        
        if(length(obj$refuge.data$model.2$Rugosity) > 0) {
          ggplot(obj$refuge.data$model.2, aes(x = Rugosity, y = refuge_status)) + 
            geom_line() + 
            geom_hline(yintercept = 1, color = "grey", linetype = "dashed") +
            facet_wrap( ~ Code, ncol = 8) + ylim(0,NA) + 
            xlab("Rugosity") + ylab("Relative availability") +
            theme_bw() + 
            theme(
              strip.text = element_text(colour = "white", face="bold"),
              strip.background = element_rect(fill="Red 3"),
              plot.background = element_blank()
            )
        } else {
          df.blank <- data.frame(x = 1, y = 0)
          ggplot(data = df.blank, aes(x = x, y = y)) +
            geom_point(size = 0) +
            ggtitle("No data provided") + 
            theme_bw() + xlim(0,1) + ylim(0,1) +
            xlab("") + ylab("") +
            theme(
              plot.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              axis.ticks = element_blank(), 
              axis.text = element_blank(),
              plot.title=element_text(colour="red")
            )
        }  
      })
      
      output$table.avail.prey <- DT::renderDataTable({
        xxx <- as.integer(input$SIPGroup)
        
        # generate the data frame to plot
        Prey <- prey.types
        Name <- prey.names
        GroupType <- group.types
        JuvJuv <- obj$prey.data$grp.data[xxx, 1, , 1]
        JuvAd  <- obj$prey.data$grp.data[xxx, 1, , 2]
        AdJuv  <- obj$prey.data$grp.data[xxx, 2, , 1]
        AdAd   <- obj$prey.data$grp.data[xxx, 2, , 2]
        df.avail.prey <- data.frame(Prey, Name, GroupType, JuvJuv, JuvAd, AdJuv, AdAd)
        
        DT::datatable(df.avail.prey, rownames = FALSE)
      })
      
      output$table.age.prey <- DT::renderDataTable({
        xxx <- as.integer(input$SIPGroup)
        
        # generate the data frame to plot
        ogd <- dplyr::arrange(obj$grp.def, Code)
        Prey       <- ogd$Code
        Name       <- ogd$Name
        GroupType <- ogd$GroupType
        m <- obj$prey.data$age.data[xxx, , ]
        df.age.prey <- data.frame(Prey, Name, GroupType, m)
        # names(df.age.prey[4:13]) <- TO BE DONE
        
        DT::datatable(df.age.prey, rownames = FALSE)
      })
      
      
      output$avail.age.plot <- renderPlot({
        xxx <- as.integer(input$SIPGroup)
        
        # generate the data frame to plot
        ogd <- dplyr::arrange(obj$grp.def, Code)
        Prey <- ogd$Code
        Name <- ogd$Name
        GroupType <- ogd$GroupType
        m <- obj$prey.data$age.data[xxx, , ]
        df.age.prey <- data.frame(Prey, Name, GroupType, m)
        
        df.age.prey <- df.age.prey[!is.na(df.age.prey$X1), ]
        max.cohorts <- dim(df.age.prey)[2] - 3
        names(df.age.prey)[4:(3+max.cohorts)] <- 1:max.cohorts
        df.age.prey <- tidyr::gather(data = df.age.prey, key = "Cohort",
          "Availability", 4:(3+max.cohorts))
        
        if (dim(df.age.prey)[1] > 0) {
          ggplot(data = df.age.prey, aes(x = Cohort, y = Availability, group = Prey)) +
            geom_point() + geom_line() + 
            xlab("Cohort") + ylab("Availability") + ylim(c(0, NA)) +
            facet_wrap( ~ Prey, ncol = 4, scales="free_y") +
            theme_bw() + 
            theme(
              panel.grid.minor = element_blank(),
              strip.text = element_text(colour = "white", face="bold"),
              strip.background = element_rect(fill="Red 3"),
              plot.background = element_blank()
            )
        } else {
          df.blank <- data.frame(x = 1, y = 0)
          ggplot(data = df.blank, aes(x = x, y = y)) +
            geom_point(size = 0) +
            ggtitle("No data provided") + 
            theme_bw() + xlim(0,1) + ylim(0,1) +
            xlab("") + ylab("") +
            theme(
              plot.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              axis.ticks = element_blank(), 
              axis.text = element_blank(),
              plot.title=element_text(colour="red")
            )
        } 
      })
      
      output$avail.prey.plot <- renderPlot({
        xxx <- as.integer(input$SIPGroup)
        
        # generate the data frame to plot
        Prey <- prey.types
        JuvJuv <- obj$prey.data$grp.data[xxx, 1, , 1]
        JuvAd  <- obj$prey.data$grp.data[xxx, 1, , 2]
        AdJuv  <- obj$prey.data$grp.data[xxx, 2, , 1]
        AdAd   <- obj$prey.data$grp.data[xxx, 2, , 2]
        df.avail.prey <- data.frame(Prey, JuvJuv, JuvAd, AdJuv, AdAd)
        df.avail.prey <- tidyr::gather(df.avail.prey, Interaction, Fraction, 
          JuvJuv:AdAd)
        df.avail.prey <- dplyr::mutate(df.avail.prey, 
          Pred.Ad = ((Interaction == "AdJuv") | (Interaction == "AdAd")))
        df.avail.prey <- dplyr::mutate(df.avail.prey, 
          Prey.Ad = ((Interaction == "JuvAd") | (Interaction == "AdAd")))
        df.avail.prey <- dplyr::arrange(df.avail.prey, Prey, Pred.Ad)   
        df.avail.prey <- df.avail.prey[-c(2)] # remove Interaction
        
        if (sum(df.avail.prey$Fraction, na.rm = TRUE) == 0) {
          df.blank <- data.frame(x = 1, y = 0)
          ggplot(data = df.blank, aes(x = x, y = y)) +
            geom_point(size = 0) +
            ggtitle("No data provided") + 
            theme_bw() + xlim(0,1) + ylim(0,1) +
            xlab("") + ylab("") +
            theme(
              plot.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              axis.ticks = element_blank(), 
              axis.text = element_blank(),
              plot.title=element_text(colour="red")
            )
        } else {
          ggplot(data = df.avail.prey, 
            aes(x = Pred.Ad, y = Fraction, fill = Prey.Ad)) +
            geom_bar(position="dodge", stat="identity") +  
            scale_fill_manual(values=c("Red 3", "Blue 3")) + 
            xlab("Predator is adult") + ylab("Fraction") +
            facet_wrap( ~ Prey, ncol = 8) +
            labs(fill = "Prey is adult") +
            theme_bw() + 
            theme(
              panel.grid.minor = element_blank(),
              strip.text = element_text(colour = "white", face="bold"),
              strip.background = element_rect(fill="Red 3"),
              plot.background = element_blank()
            )
        }  
      })
      
      output$clearance.plot <- renderPlot({
        if (no.clearance.data) {
          df.blank <- data.frame(x = 1, y = 0)
          ggplot(data = df.blank, aes(x = x, y = y)) +
            geom_point(size = 0) +
            ggtitle("No data provided") + 
            theme_bw() + xlim(0,1) + ylim(0,1) +
            xlab("") + ylab("") +
            theme(
              plot.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              axis.ticks = element_blank(), 
              axis.text = element_blank(),
              plot.title=element_text(colour="red")
            )
        } else {
          # get maximum number of cohorts
          df.clearance <- obj$clearance.data
          max.cohorts <- dim(df.clearance)[2]-5
          names(df.clearance)[6:(5+max.cohorts)] <- as.character(1:max.cohorts)
          df.clearance <- tidyr::gather(data = df.clearance, key = "Cohort",
            value = "Clearance", 6:(5+max.cohorts))
          df.clearance <- df.clearance[!is.na(df.clearance$Clearance),]
          df.clearance$Cohort <- as.numeric(df.clearance$Cohort)
          
          ggplot(data = df.clearance, aes(x = Cohort, y = Clearance, group = Code)) +
            geom_point() + geom_line() + 
            xlab("Cohort") + ylab("Clearance rate (m^3 per day per individual/(mg N))") + ylim(c(0, NA)) +
            facet_wrap( ~ Code, ncol = 4, scales="free_y") +
            theme_bw() + 
            theme(
              panel.grid.minor = element_blank(),
              strip.text = element_text(colour = "white", face="bold"),
              strip.background = element_rect(fill="Red 3"),
              plot.background = element_blank()
            )
        }  
      })
      
      output$growth.max.plot <- renderPlot({
        no.data <- sum(!is.na(obj$grp.growth$co.1)) == 0
        if (no.clearance.data) {
          df.blank <- data.frame(x = 1, y = 0)
          ggplot(data = df.blank, aes(x = x, y = y)) +
            geom_point(size = 0) +
            ggtitle("No data provided") + 
            theme_bw() + xlim(0,1) + ylim(0,1) +
            xlab("") + ylab("") +
            theme(
              plot.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              axis.ticks = element_blank(), 
              axis.text = element_blank(),
              plot.title=element_text(colour="red")
            )
        } else {
          # get maximum number of cohorts
          df.growth <- obj$grp.growth
          max.cohorts <- dim(df.growth)[2]-5
          names(df.growth)[6:(5+max.cohorts)] <- as.character(1:max.cohorts)
          df.growth <- tidyr::gather(data = df.growth, key = "Cohort",
            value = "Growth", 6:(5+max.cohorts))
          df.growth <- df.growth[!is.na(df.growth$Growth),]
          df.growth$Cohort <- as.numeric(df.growth$Cohort)
          
          ggplot(data = df.growth, aes(x = Cohort, y = Growth, group = Code)) +
            geom_point() + geom_line() + 
            xlab("Cohort") + ylab("Maximum growth rate (mg N d-1)") + ylim(c(0, NA)) +
            facet_wrap( ~ Code, ncol = 4, scales="free_y") +
            theme_bw() + 
            theme(
              panel.grid.minor = element_blank(),
              strip.text = element_text(colour = "white", face="bold"),
              strip.background = element_rect(fill="Red 3"),
              plot.background = element_blank()
            )
        }  
      })
      
      output$mig.num.plot <- renderPlot({
        no.data <- sum(!is.na(obj$grp.mig$mig.num$co.1)) == 0
        if (no.data) {
          df.blank <- data.frame(x = 1, y = 0)
          ggplot(data = df.blank, aes(x = x, y = y)) +
            geom_point(size = 0) +
            ggtitle("No data provided") + 
            theme_bw() + xlim(0,1) + ylim(0,1) +
            xlab("") + ylab("") +
            theme(
              plot.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              axis.ticks = element_blank(), 
              axis.text = element_blank(),
              plot.title=element_text(colour="red")
            )
        } else {
          df.num <- obj$grp.mig$mig.num
          max.cohorts <- dim(df.num)[2]-1
          names(df.num)[2:(1+max.cohorts)] <- 1:max.cohorts
          df.num <- tidyr::gather(data = df.num, key = "Cohort",
            value = "number", 2:(1+max.cohorts))
          df.num <- df.num[!is.na(df.num$number),]
          
          ggplot(data = df.num, aes(x = Cohort, y = number, group = Code)) +
            geom_point() + geom_line() + 
            xlab("Cohort") + ylab("Number") + ylim(c(0, NA)) +
            facet_wrap( ~ Code, ncol = 4, scales="free_y") +
            theme_bw() + 
            theme(
              panel.grid.minor = element_blank(),
              strip.text = element_text(colour = "white", face="bold"),
              strip.background = element_rect(fill="Red 3"),
              plot.background = element_blank()
            )
        }  
      })
      
      output$mig.bio.plot <- renderPlot({
        # TO DO: do not use complete cases!
        
        no.data <- sum(!is.na(obj$grp.mig$mig.bio$co.1)) == 0
        if (no.data) {
          df.blank <- data.frame(x = 1, y = 0)
          ggplot(data = df.blank, aes(x = x, y = y)) +
            geom_point(size = 0) +
            ggtitle("No data provided") + 
            theme_bw() + xlim(0,1) + ylim(0,1) +
            xlab("") + ylab("") +
            theme(
              plot.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              axis.ticks = element_blank(), 
              axis.text = element_blank(),
              plot.title=element_text(colour="red")
            )
        } else {
          df.bio <- obj$grp.mig$mig.bio
          max.cohorts <- dim(df.bio)[2]-1
          names(df.bio)[2:(1+max.cohorts)] <- 1:max.cohorts
          df.bio <- tidyr::gather(data = df.bio, key = "Cohort",
            value = "biomass", 2:(1+max.cohorts))
          df.bio <- df.bio[!is.na(df.bio$biomass),]
          
          ggplot(data = df.bio, aes(x = Cohort, y = biomass, group = Code)) +
            geom_point() + geom_line() + 
            xlab("Cohort") + ylab("Biomass (mg N)") + ylim(c(0, NA)) +
            facet_wrap( ~ Code, ncol = 4, scales="free_y") +
            theme_bw() + 
            theme(
              panel.grid.minor = element_blank(),
              strip.text = element_text(colour = "white", face="bold"),
              strip.background = element_rect(fill="Red 3"),
              plot.background = element_blank()
            )
        }  
      })
      
      output$mig.plot <- renderPlot({
        no.data <- sum(!is.na(obj$grp.mig$mig.res$co.1)) == 0
        if (no.data) {
          df.blank <- data.frame(x = 1, y = 0)
          ggplot(data = df.blank, aes(x = x, y = y)) +
            geom_point(size = 0) +
            ggtitle("No data provided") + 
            theme_bw() + xlim(0,1) + ylim(0,1) +
            xlab("") + ylab("") +
            theme(
              plot.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              axis.ticks = element_blank(), 
              axis.text = element_blank(),
              plot.title=element_text(colour="red")
            )
        } else {
          df.res <- obj$grp.mig$mig.res
          max.cohorts <- dim(df.res)[2] - 1
          names(df.res)[2:(1+max.cohorts)] <- 1:max.cohorts
          df.res <- tidyr::gather(data = df.res, key = "Cohort",
            value = "mass", 2:(1+max.cohorts))
          df.res <- df.res[complete.cases(df.res),]
          
          df.str <- obj$grp.mig$mig.str
          max.cohorts <- dim(df.str)[2] - 1
          names(df.str)[2:(1+max.cohorts)] <- 1:max.cohorts
          df.str <- tidyr::gather(data = df.str, key = "Cohort",
            value = "mass", 2:(1+max.cohorts))
          df.str <- df.str[complete.cases(df.str),]
          
          ggplot(data = df.res, aes(x = Cohort, y = mass, group = Code)) +
            geom_point() + geom_line() + 
            geom_point(data = df.str, 
              aes(x = Cohort, y = mass, group = Code), colour = "blue") + 
            geom_line(data = df.str, 
              aes(x = Cohort, y = mass, group = Code), colour = "blue") + 
            xlab("Cohort") + ylab("Mass (mg N)") + ylim(c(0, NA)) +
            facet_wrap( ~ Code, ncol = 4, scales="free_y") +
            theme_bw() + 
            theme(
              panel.grid.minor = element_blank(),
              strip.text = element_text(colour = "white", face="bold"),
              strip.background = element_rect(fill="Red 3"),
              plot.background = element_blank()
            )
        }  
      })
      
      output$recruit.mass.plot <- renderPlot({
        # create a data frame for plotting
        df.recruit.mass <- data.frame(
          Code = obj$grp.att$Code,
          KWRR = obj$grp.att$KWRR_XXX,
          KWSR = obj$grp.att$KWSR_XXX
        )
        
        no.mass.data <- sum(df.recruit.mass$KWSR, na.rm = TRUE) == 0.0
        df.recruit.mass <- df.recruit.mass[complete.cases(df.recruit.mass), ]
        
        if (no.mass.data) {
          df.blank <- data.frame(x = 1, y = 0)
          ggplot(data = df.blank, aes(x = x, y = y)) +
            geom_point(size = 0) +
            ggtitle("No data provided") + 
            theme_bw() + xlim(0,1) + ylim(0,1) +
            xlab("") + ylab("") +
            theme(
              plot.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              axis.ticks = element_blank(), 
              axis.text = element_blank(),
              plot.title=element_text(colour="red")
            )
        } else {
          ggplot(data = df.recruit.mass, aes(x = KWSR, y = KWRR, label = Code)) +
            geom_point(colour = "red") +
            geom_text(size = 3, position=position_jitter(width=0, height=6)) + 
            xlab("Structural mass (mg N)") + ylab("Reserve mass (mg N)") + 
            scale_x_log10() + scale_y_log10() + # coord_fixed() +
            theme_bw() + 
            theme(
              panel.grid.minor = element_blank(),
              strip.text = element_text(colour = "white", face="bold"),
              strip.background = element_rect(fill="Red 3"),
              plot.background = element_blank()
            )
        }  
        
      })  
      
    }
  )  
}

# +===================================================+
# |  make.prm.map : collect data for displaying maps  |
# +===================================================+
make.prm.map <- function(bgm.file){
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
      numboxes <- as.numeric(unlist(str_extract_all(bgm[j],"\\(?[0-9.-]+\\)?")[[1]])[1])
    } 
  }
  
  # Extract the box vertices
  map_base <- data.frame()
  for(i in 1:numboxes){
    txt.find <- paste("box", i - 1, ".vert", sep = "")
    j <- grep(txt.find, bgm)
    for (jj in 1:length(j)) {
      text.split <- unlist(str_split(
        gsub(pattern = "[\t ]+", x = bgm[j[jj]], replacement = " "), " "))
      if (text.split[1] == txt.find) {
        map_base <- rbind(map_base, cbind(i - 1, as.numeric(text.split[2]),
          as.numeric(text.split[3])))
      } 
    }
  }  
  names(map_base) <- c("boxid", "x", "y")  
  
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
  return(list(numboxes = numboxes, map_base = map_base, box.data = box.data))
}

# +=======================================================+
# |  make.prm.groups : collect group data for displaying  |
# +=======================================================+
make.prm.groups <- function(grp.file){
  df <- read.csv(grp.file)
  
  # extract habitat types
  habitat.types <- c(as.character(df$Code[df$IsCover == 1]), "reef", "flat", "soft", "canyon")
  
  # check that column title is not LongName
  tmp <- which(names(df) == "LongName")
  if (!is.null(tmp)) {
    names(df)[tmp] <- "Long.Name"
  }
  
  # check that column title is not InvertType: convert to GroupType
  tmp <- which(names(df) == "InvertType")
  if (!is.null(tmp)) {
    names(df)[tmp] <- "GroupTYpe"
  }

  return (list(grp.vals = df, habitat.types = habitat.types))
}  

# +======================================================================+
# |  make.prm.attributes : collect group attributes data for displaying  |
# +======================================================================+
make.prm.attributes <- function(prm.file, grp.vals){
  prm <- readLines(prm.file) # read in the biological parameter file
  grp.att <- grp.vals[c("Code", "Name", "GroupType")] # develop this data frame
  
  def.grp.file <- system.file("extdata", "grpTemplates.csv", package = "shinyrAtlantis")
  df.prms.all <- read.csv(file = def.grp.file, header = TRUE)
  tmplts <- df.prms.all$Template # group templates to search for
  Codes <- grp.att$Code # groups to search for
  
  for(tmplt in tmplts) { # look for each template
    cat("-")
    p.vals <- rep(NA, length(Codes))
    i <- 0
    for (xxx in Codes) { # look for each Code
      i <- i + 1 # xxx index
      txt.find <- gsub(pattern = "XXX", replacement = xxx, x = tmplt)
      j <- grep(pattern = txt.find, x = prm, value = FALSE) # file row(s)
      if (length(j) > 0) { # found the parameter
        jnew <- NULL
        for (jj in 1:length(j)) {
          # Valid row is when tmplt is the first entry and second is a number
          text.split <- unlist(str_split(
            gsub(pattern = "[ ]+", x = prm[j[jj]], replacement = " "), " "))
          if (text.split[1] == txt.find) {
            jnew <- c(jnew,j[jj]) # add the row that satisfies the criteria
          }
        }
        j <- jnew # use this list of rows as they are valid
        if (length(j) == 1) { # a single row is found
          # get parameter value (1 after nums)
          p.val <- as.numeric(unlist(str_split(prm[j],"[\t ]+"))[2])
          p.vals[i] <- p.val
        } 
      }
    }
    grp.att$tmplt <- p.vals # add the group attribute column
    names(grp.att)[names(grp.att) == "tmplt"] <- as.character(tmplt) # rename column
  }
  cat("\n")
  
  return(grp.att)
}

# +=======================================================================+
# |  make.prm.general : collect non-group attributes data for displaying  |
# +=======================================================================+
make.prm.general <- function(prm.file){
  prm <- readLines(prm.file) # read in the biological parameter file
  # read in the parameter definition file
  def.all.file <- system.file("extdata", "paramdefns.csv", package = "shinyrAtlantis")
  df.prm.defns <- read.csv(def.all.file, header = TRUE)
  df.prms <- df.prm.defns # data frame to return
  df.prms$Value <- NA # set initial values
  df.prms <- df.prms[c("Parameter","Units", "Value", "Required", "Suggested", "Definition")] # name df columns
  
  for(tmplt in df.prms$Parameter) { # look for each parameter
    j <- grep(pattern = tmplt, x = prm, value = FALSE) # file row(s)
    if (length(j) > 0) { # found the parameter
      jnew <- NULL
      for (jj in 1:length(j)) {
        # Valid row is when tmplt is the first entry and second is a number
        text.split <- unlist(str_split(
          gsub(pattern = "[\t ]+", x = prm[j[jj]], replacement = " "), " "))
        if (text.split[1] == tmplt) {
          jnew <- c(jnew,j[jj]) # add the row that satisfies the criteria
        }
      }
      j <- jnew # use this list of rows as they are valid
      if (length(j) == 1) { # a single row is found
        text.split <- unlist(str_split(
          gsub(pattern = "[\t ]+", x = prm[j], replacement = " "), " "))
        df.prms$Value[which(df.prms$Parameter == tmplt)] <- 
          as.numeric(text.split[2])
      } 
    } else {
      # do nothing as the template is not found
    }
  }
  return(df.prms)
}

# +======================================+
# |  GetHabitats : collect habitat data  |
# +======================================+
GetHabitats <- function(grp.att, tmplt, prm, habitat.types) {
  Codes <- grp.att$Code
  habitats <- length(habitat.types)
  codes <- length(Codes)
  
  txt.rows <- data.frame(Codes, matrix(NA, nrow = codes, ncol = habitats))
  names(txt.rows) <- c("Code", habitat.types)
  for (xxx in Codes) { # look for each Code
    txt.find <- gsub(pattern = "XXX", replacement = xxx, x = tmplt)
    j <- grep(pattern = txt.find, x = prm, value = FALSE) # file row(s)
    if (length(j) == 1) { # a single row is found. Data is on next row
      hab.vals <- na.omit(as.numeric(unlist(
        strsplit(prm[j+1], "[^0-9.]+"))))
      k <- which(txt.rows$Code == xxx)
      txt.rows[k,] <- c(xxx, hab.vals)
    }
  }  
  grp.att <- merge(grp.att, txt.rows, by = "Code", all = TRUE) # add column
  
  return(grp.att)
}

# +===============================================================+
# |  make.prm.habitats : collect all habitat data for displaying  |
# +===============================================================+
make.prm.habitats <- function(prm.file, grp.object, habitat.types){
  prm        <- readLines(prm.file) # read in the biological parameter file
  grp.att    <- grp.object[c("Code","Name", "GroupType")] # develop this data frame
  tmplts     <- c("habitat_XXX", "ad_habitat_XXX", "juv_habitat_XXX")
  df.hab     <- GetHabitats(grp.att, tmplts[1], prm, habitat.types)
  df.hab.ad  <- GetHabitats(grp.att, tmplts[2], prm, habitat.types)
  df.hab.juv <- GetHabitats(grp.att, tmplts[3], prm, habitat.types)
  
  return(list(df.hab = df.hab, df.hab.ad = df.hab.ad, df.hab.juv = df.hab.juv))
}

# +=======================================================================+
# |  make.prm.migration : collect group distribution data for displaying  |
# +=======================================================================+
make.prm.migration <- function(prm.file, Code){
  prm   <- readLines(prm.file) # read in the biological parameter file
  Code <- sort(Code)
  codes <- length(Code)
  
  # find the maximum number of cohorts
  max.cohorts <- 0
  for (xxx in Code) { # look for each Code
    # invertebrate migration data
    txt.find <- paste("KMIGa_", xxx, sep = "")
    j <- grep(pattern = txt.find, x = prm, value = FALSE) # file row(s)
    if (length(j) > 0) { # at least one row with txt.find
      for (jj in 1:length(j)) {
        levs <- as.numeric(unlist(str_extract_all(prm[j[jj]],"\\(?[0-9.-]+\\)?")[[1]])[1])
        if (levs > max.cohorts) {
          max.cohorts <- levs
        }
      }
    }
  }  
  
  # numbers (vertebrates)
  grp.mig.num  <- array(NA, dim=c(codes, max.cohorts))
  dimnames(grp.mig.num)[[1]]  <- Code
  dimnames(grp.mig.num)[[2]]  <- paste(rep("co", max.cohorts), 1:max.cohorts, sep = "-")
  # biomass (invertebrates)
  grp.mig.bio <- array(NA, dim=c(codes, max.cohorts))
  dimnames(grp.mig.bio)[[1]]  <- Code
  dimnames(grp.mig.bio)[[2]]  <- paste(rep("co", max.cohorts), 1:max.cohorts, sep = "-")
  # structural nitrogen
  grp.mig.str <- array(NA, dim=c(codes, max.cohorts))
  dimnames(grp.mig.str)[[1]]  <- Code
  dimnames(grp.mig.str)[[2]]  <- paste(rep("co", max.cohorts), 1:max.cohorts, sep = "-")
  # reserve nitrogen
  grp.mig.res <- array(NA, dim=c(codes, max.cohorts))
  dimnames(grp.mig.res)[[1]]  <- Code
  dimnames(grp.mig.res)[[2]]  <- paste(rep("co", max.cohorts), 1:max.cohorts, sep = "-")
  
  for (xxx in Code) { # look for each Code
    # invertebrate migration data
    txt.find <- paste("KMIGa_", xxx, sep = "")
    j <- grep(pattern = txt.find, x = prm, value = FALSE) # file row(s)
    txt.find <- paste("KMIGa_INVERT_", xxx, sep = "")
    j <- c(j, grep(pattern = txt.find, x = prm, value = FALSE)) # file row(s)
    if (length(j) > 0) { # at least one row with txt.find
      for (jj in 1:length(j)) {
        levs <- as.numeric(unlist(str_extract_all(prm[j[jj]],"\\(?[0-9.-]+\\)?")[[1]])[1])
        levs <- min(levs, max.cohorts) # cut unnecessary values
        vals <- as.numeric(str_split(prm[j[jj]+1], "[\t ]+")[[1]])
        mig.txt <- unlist(str_split(prm[j[jj]], "[\t ]+"))[1]
        if (mig.txt == paste("KMIGa_", xxx, sep = "")) { 
          # numbers
          grp.mig.num[xxx,1:levs] <- vals[1:levs]
        } else if (mig.txt == paste("KMIGa_INVERT_", xxx, "", sep = "")) {
          # biomass
          grp.mig.bio[xxx,1:levs] <- vals[1:levs]
        } else if (mig.txt == paste("KMIGa_", xxx, "sn", sep = "")) {
          # structural nitrogen
          grp.mig.str[xxx,1:levs] <- vals[1:levs]
        } else if (mig.txt == paste("KMIGa_", xxx, "rn", sep = "")) {
          # reserve nitrogen
          grp.mig.res[xxx,1:levs] <- vals[1:levs]
        }
      }
    }
  }
  
  grp.mig.num <- data.frame(Code = Code, grp.mig.num)
  grp.mig.bio <- data.frame(Code = Code, grp.mig.bio)
  grp.mig.str <- data.frame(Code = Code, grp.mig.str)
  grp.mig.res <- data.frame(Code = Code, grp.mig.res)
  
  return(list(mig.num = grp.mig.num, mig.bio = grp.mig.bio,
    mig.str = grp.mig.str, mig.res = grp.mig.res))
}

# +===========================================================================+
# |  make.prm.distributions : collect group distribution data for displaying  |
# +===========================================================================+
make.prm.distributions <- function(prm.file, Code, numboxes){
  prm   <- readLines(prm.file) # read in the biological parameter file
  Code <- sort(Code)
  codes <- length(Code)
  
  # Horizontal distributions (?)
  grp.hor.ad  <- array(NA, dim=c(codes, 4, numboxes))
  grp.hor.juv <- array(NA, dim=c(codes, 4, numboxes))
  dimnames(grp.hor.ad)[[1]]  <- Code
  dimnames(grp.hor.juv)[[1]] <- Code
  
  # Adult vertebrates or invertebrates
  grp.vert.day   <- array(NA, dim=c(codes, 10)) # assume at most 10 water layers
  grp.vert.night <- array(NA, dim=c(codes, 10)) # assume at most 10 water layers
  dimnames(grp.vert.day)[[1]]  <- Code
  dimnames(grp.vert.night)[[1]] <- Code
  
  # Juvenile vertebrates
  grp.vert.juv.day   <- array(NA, dim=c(codes, 10)) # assume at most 10 water layers
  grp.vert.juv.night <- array(NA, dim=c(codes, 10)) # assume at most 10 water layers
  dimnames(grp.vert.juv.day)[[1]]  <- Code
  dimnames(grp.vert.juv.night)[[1]] <- Code
  
  for (xxx in Code) { # look for each Code
    # Horizontal data
    for (i in 1:4) { # look for each index (1 to 4)
      txt.find <- paste("F",xxx,"_S",as.character(i),sep = "")
      j <- grep(pattern = txt.find, x = prm, value = FALSE) # file row(s)
      if (length(j) > 0) { # at least one row with txt.find
        for (jj in 1:length(j)) {
          vals <- as.numeric(str_split(prm[j[jj]+1], "[\t ]+")[[1]])
          min.len <- min(numboxes, length(vals))
          if (length(grep("juv",prm[j[jj]])) > 0) { # juvenile data
            grp.hor.juv[xxx,i,1:min.len] <- vals[1:min.len]
          } else { # adult data
            grp.hor.ad[xxx,i,1:min.len] <- vals[1:min.len]
          }
        }         
      }
    }
    
    # Vertical data
    # Invertebrates
    txt.find <- paste("VERTday_",xxx,sep = "")
    j <- grep(pattern = txt.find, x = prm, value = FALSE) # file row(s)
    if (length(j) == 1) { # unique row, so vertebrate
      levs <- as.numeric(unlist(str_extract_all(prm[j],"\\(?[0-9.-]+\\)?")[[1]])[1])
      levs <- min(levs,10)
      vals <- as.numeric(str_split(prm[j+1], "[\t ]+")[[1]])
      grp.vert.day[xxx,1:levs] <- vals[1:levs]
    }  
    txt.find <- paste("VERTnight_",xxx,sep = "")
    j <- grep(pattern = txt.find, x = prm, value = FALSE) # file row(s)
    if (length(j) == 1) { # unique row
      levs <- as.numeric(unlist(str_extract_all(prm[j],"\\(?[0-9.-]+\\)?")[[1]])[1])
      levs <- min(levs,10)
      vals <- as.numeric(str_split(prm[j+1], "[\t ]+")[[1]])
      grp.vert.night[xxx,1:levs] <- vals[1:levs]
    }
    
    # Vertebrates (juveniles and adults): 
    
    # Juvenile vertebrates
    txt.find <- paste("VERTday_",xxx,"1",sep = "")
    j <- grep(pattern = txt.find, x = prm, value = FALSE) # file row(s)
    if (length(j) == 1) { # unique row
      levs <- as.numeric(unlist(str_extract_all(prm[j],"\\(?[0-9.-]+\\)?")[[1]])[2])
      levs <- min(levs,10)
      vals <- as.numeric(str_split(prm[j+1], "[\t ]+")[[1]])
      grp.vert.juv.day[xxx,1:levs] <- vals[1:levs]
    }  
    txt.find <- paste("VERTnight_",xxx,"1",sep = "")
    j <- grep(pattern = txt.find, x = prm, value = FALSE) # file row(s)
    if (length(j) == 1) { # unique row
      levs <- as.numeric(unlist(str_extract_all(prm[j],"\\(?[0-9.-]+\\)?")[[1]])[2])
      levs <- min(levs,10)
      vals <- as.numeric(str_split(prm[j+1], "[\t ]+")[[1]])
      grp.vert.juv.night[xxx,1:levs] <- vals[1:levs]
    }
    
    # Adult vertebrates
    txt.find <- paste("VERTday_",xxx,"2",sep = "")
    j <- grep(pattern = txt.find, x = prm, value = FALSE) # file row(s)
    if (length(j) == 1) { # unique row
      levs <- as.numeric(unlist(str_extract_all(prm[j],"\\(?[0-9.-]+\\)?")[[1]])[2])
      levs <- min(levs,10)
      vals <- as.numeric(str_split(prm[j+1], "[\t ]+")[[1]])
      grp.vert.day[xxx,1:levs] <- vals[1:levs]
    }  
    txt.find <- paste("VERTnight_",xxx,"2",sep = "")
    j <- grep(pattern = txt.find, x = prm, value = FALSE) # file row(s)
    if (length(j) == 1) { # unique row
      levs <- as.numeric(unlist(str_extract_all(prm[j],"\\(?[0-9.-]+\\)?")[[1]])[2])
      levs <- min(levs,10)
      vals <- as.numeric(str_split(prm[j+1], "[\t ]+")[[1]])
      grp.vert.night[xxx,1:levs] <- vals[1:levs]
    }
  }
  
  # Extract recruitment data
  
  # Horizontal recruitment
  grp.hor.recruit  <- array(NA, dim=c(codes, numboxes))
  dimnames(grp.hor.recruit)[[1]]  <- Code
  # Vertical recruitment
  grp.vert.recruit <- array(NA, dim=c(codes, 10)) # assume at most 10 water levels
  dimnames(grp.vert.recruit)[[1]]  <- Code
  
  for (xxx in Code) { # look for each Code
    # Horizontal data
    txt.find <- paste(xxx, "_recruit_hdistrib", sep = "")
    j <- grep(pattern = txt.find, x = prm, value = FALSE) # file row(s)
    if (length(j) > 0) { # at least one row with txt.find
      if (length(j) == 1) { # unique row
        vals <- as.numeric(str_split(prm[j[1]+1], "[\t ]+")[[1]])
        min.len <- min(numboxes, length(vals))
        if (length(grep("juv",prm[j[jj]])) > 0) { # juvenile data
          grp.hor.recruit[xxx,1:min.len] <- vals[1:min.len]
        } else { # adult data
          grp.hor.recruit[xxx,1:min.len] <- vals[1:min.len]
        }
      }         
    }
    
    # Vertical data
    txt.find <- paste(xxx,"_recruit_vdistrib",sep = "")
    j <- grep(pattern = txt.find, x = prm, value = FALSE) # file row(s)
    if (length(j) == 1) { # unique row, so vertebrate
      levs <- as.numeric(unlist(str_extract_all(prm[j],"\\(?[0-9.-]+\\)?")[[1]])[1])
      levs <- min(levs,10)
      vals <- as.numeric(str_split(prm[j+1], "[\t ]+")[[1]])
      grp.vert.recruit[xxx,1:levs] <- vals[1:levs]
    }  
  }
  
  # Extract migration data
  
  # invertebrate migration
  grp.migration  <- array(NA, dim=c(codes, numboxes))
  dimnames(grp.migration)[[1]]  <- Code
  # adult migration
  grp.migration.ad <- array(NA, dim=c(codes, numboxes))
  dimnames(grp.migration.ad)[[1]]  <- Code
  # juvenile migration
  grp.migration.juv <- array(NA, dim=c(codes, numboxes))
  dimnames(grp.migration.juv)[[1]]  <- Code
  
  for (xxx in Code) { # look for each Code
    # invertebrate migration data
    txt.find <- paste("MigIOBox_", xxx, sep = "")
    j <- grep(pattern = txt.find, x = prm, value = FALSE) # file row(s)
    if (length(j) > 0) { # at least one row with txt.find
      for (jj in 1:length(j)) {
        levs <- as.numeric(unlist(str_extract_all(prm[j[jj]],"\\(?[0-9.-]+\\)?")[[1]])[1])
        levs <- min(levs, numboxes)
        vals <- as.numeric(str_split(prm[j[jj]+1], "[\t ]+")[[1]])
        mig.txt <- unlist(str_split(prm[j[jj]], "[\t ]+"))[1]
        if (mig.txt == paste("MigIOBox_", xxx, sep = "")) { 
          # invertebrate
          grp.migration[xxx,1:levs] <- vals[1:levs]
        } else if (mig.txt == paste("MigIOBox_", xxx, "ad", sep = "")) {
          # adult
          grp.migration.ad[xxx,1:levs] <- vals[1:levs]
        } else if (mig.txt == paste("MigIOBox_", xxx, "juv", sep = "")) {
          # juvenile
          grp.migration.juv[xxx,1:levs] <- vals[1:levs]
        }
      }
    }
  }
  
  return(list(hor.ad = grp.hor.ad, hor.juv = grp.hor.juv,
    vert.day = grp.vert.day, vert.night = grp.vert.night,
    vert.juv.day = grp.vert.juv.day, vert.juv.night = grp.vert.juv.night,
    hor.recruit = grp.hor.recruit, vert.recruit = grp.vert.recruit,
    mig.invert = grp.migration, mig.ad = grp.migration.ad, mig.juv = grp.migration.juv))
}

# +====================================================+
# |  make.prm.prey : collect prey data for displaying  |
# +====================================================+
make.prm.prey <- function(prm.file, Code) {
  prm <- readLines(prm.file) # read in the biological parameter file
  Code <- sort(Code)
  codes <- length(Code)
  # TO BE DONE: This assumption is not universal?
  prey.codes <- c(as.character(Code), "DLsed", "DRsed", "DCsed")
  preys <- length(prey.codes)
  
  prey.data <- array(NA, dim=c(codes, 2, preys, 2))
  dimnames(prey.data)[[1]]  <- Code
  
  for (xxx in Code) { # look for each Code
    cat("-")
    for (pred.class in 1:2) {
      for (prey.class in 1:2) {
        txt.find <- paste("pPREY", as.character(pred.class), 
          as.character(xxx), as.character(prey.class), sep = "")
        j <- grep(pattern = txt.find, x = prm, value = FALSE) # file row(s)
        if (length(j) == 1) {
          vals <- as.numeric(unlist(str_split(string = prm[j[1]+1], pattern = "[\t ]+")))
          min.length <- min(length(vals), preys)
          prey.data[xxx, pred.class, 1:min.length, prey.class] <- vals[1:min.length]
        }  
      }
    } 
  }
  
  age.data <- array(NA, dim=c(codes, codes, 10))
  dimnames(age.data)[[1]]  <- Code
  dimnames(age.data)[[2]]  <- Code
  
  for (xxx in Code) { # look for each Code
    cat("-")
    for (yyy in Code) {
      txt.find <- paste("p_", as.character(yyy), as.character(xxx), sep = "")
      j <- grep(pattern = txt.find, x = prm, value = FALSE) # file row(s)
      if (length(j) == 1) {
        vals <- as.numeric(unlist(str_split(string = prm[j[1]+1], pattern = "[\t\n ]+")))
        age.data[xxx, yyy, ] <- na.omit(vals)
      }
    } 
  }
  cat("\n")
  
  return(list(grp.data = prey.data, age.data = age.data))
}

# +========================================================+
# |  make.prm.growth : collect growth data for displaying  |
# +========================================================+
make.prm.growth <- function(prm.file, grp.object) {
  prm <- readLines(prm.file) # read in the biological parameter file
  Code <- grp.object$grp.vals$Code # group names
  codes <- length(Code) # number of groups
  
  coh.classes <- rep(x = NA, codes) # store number of cohort classes
  coh.txt <- rep(x = NA, codes) # text containing mum rates
  k <- 0
  for (xxx in Code) { # look for each Code
    k <- k + 1
    txt.find <- paste("mum_", as.character(xxx),sep = "")
    j <- grep(pattern = txt.find, x = prm, value = FALSE) # file row(s)
    
    if (length(j) > 0) {
      for (jj in 1:length(j)) {
        vals <- unlist(str_split(string = prm[j[jj]], pattern = "[\t ]+"))
        if (vals[1] == txt.find) { # must be the line of interest in prm
          coh.classes[k] <- as.integer(vals[2]) # second value is array size
          coh.txt[k] <- prm[j[jj]+1] # collect the clearance text
        }
      }  
    } else {
      coh.classes[k] <- 1 # not found so assume a single value
    }  
  }
  
  max.classes <- max(coh.classes, na.rm = TRUE) # max size of array (>= 1)
  growth.data <- array(NA, dim=c(codes, max.classes))
  dimnames(growth.data)[[1]]  <- Code # name rows and columns
  dimnames(growth.data)[[2]]  <- paste(rep("co", max.classes), 1:max.classes, sep = "-")
  
  for (j in 1:codes) { # check each group
    if (!is.na(coh.classes[j])) { # found data
      tmp1 <- coh.classes[j] # expected number of rates
      tmp2 <- as.numeric(unlist(str_split(coh.txt[j], pattern = "[\t ]+")))
      tmp2 <- tmp2[!is.na(tmp2)] # remove any blanks
      tmp3 <- length(tmp2) # number of rates found
      if (tmp1 == tmp3) { # check that the data and expected size match
        tmp2 <- c(tmp2, rep(NA, max.classes-tmp3)) # fill up the row 
        growth.data[j, ] <- tmp2 # add the clearance data to the array
      }
    }
  }
  
  # store clearance rates and additional details in a data frame
  # need to check for naming of the "is predator" column
  if (!is.null(grp.object$grp.vals$IsPredator)) {
    df.growth.data <- data.frame(Code = Code, 
      Long.Name = grp.object$grp.vals$Long.Name, 
      GroupType = grp.object$grp.vals$GroupType, 
      IsPredator = grp.object$grp.vals$IsPredator, 
      NumCohorts = grp.object$grp.vals$NumCohorts,
      growth.data)
  } else if (!is.null(grp.object$grp.vals$isPredator)) {
    df.growth.data <- data.frame(Code = Code, 
      Long.Name = grp.object$grp.vals$Long.Name, 
      GroupType = grp.object$grp.vals$GroupType, 
      isPredator = grp.object$grp.vals$isPredator, 
      NumCohorts = grp.object$grp.vals$NumCohorts,
      growth.data)
  } else {
    df.growth.data <- data.frame(Code = Code, 
      Long.Name = grp.object$grp.vals$Long.Name, 
      GroupType = grp.object$grp.vals$GroupType, 
      NumCohorts = grp.object$grp.vals$NumCohorts,
      growth.data)
  } 
  
  return(df.growth.data)
}

# +===================================================================+
# |  make.prm.clearance : collect clearance rate data for displaying  |
# +===================================================================+
make.prm.clearance <- function(prm.file, grp.object) {
  prm <- readLines(prm.file) # read in the biological parameter file
  Code <- grp.object$grp.vals$Code # group names
  codes <- length(Code) # number of groups
  
  # get flagfishrates as it determines units   
  j <- grep(pattern = "flagfishrates", x = prm, value = FALSE) # file row(s)
  if (length(j) == 1) { 
    flagfishrates <- unlist(str_split(string = prm[j[1]], pattern = "[\t ]+"))[2]
  } else {
    flagfishrates <- NA # must be unambiguous
  }
  
  coh.classes <- rep(x = NA, codes) # store number of cohort classes
  coh.txt <- rep(x = NA, codes) # text containing clearance rates
  k <- 0
  for (xxx in Code) { # look for each Code
    k <- k + 1
    txt.find <- paste("C_", as.character(xxx),sep = "")
    j <- grep(pattern = txt.find, x = prm, value = FALSE) # file row(s)
    
    if (length(j) > 0) {
      for (jj in 1:length(j)) {
        vals <- unlist(str_split(string = prm[j[jj]], pattern = "[\t ]+"))
        if (vals[1] == txt.find) { # must be the line of interest in prm
          coh.classes[k] <- as.integer(vals[2]) # second value is array size
          coh.txt[k] <- prm[j[jj]+1] # collect the clearance text
        }
      }  
    } else {
      coh.classes[k] <- 1 # group is not found so assume a single cohort
    }  
  }
  
  max.classes <- max(coh.classes, na.rm = TRUE) # max size of array (>= 1)
  clearance.data <- array(NA, dim=c(codes, max.classes))
  dimnames(clearance.data)[[1]]  <- Code # name rows and columns
  dimnames(clearance.data)[[2]]  <- paste(rep("co", max.classes), 1:max.classes, sep = "-")
  
  for (j in 1:codes) { # check each group
    if (!is.na(coh.classes[j])) { # found data
      tmp1 <- coh.classes[j] # expected number of rates
      tmp2 <- as.numeric(unlist(str_split(coh.txt[j], pattern = "[\t ]+")))
      tmp2 <- tmp2[!is.na(tmp2)] # remove any blanks
      tmp3 <- length(tmp2) # number of rates found
      if (tmp1 == tmp3) { # check that the data and expected size match
        tmp2 <- c(tmp2, rep(NA, max.classes-tmp3)) # fill up the row 
        clearance.data[j, ] <- tmp2 # add the clearance data to the array
      }
    }
  }
  
  # store clearance rates and additional details in a data frame
  # need to check for naming of the "is predator" column
  if (!is.null(grp.object$grp.vals$IsPredator)) {
    df.clearance.data <- data.frame(Code = Code, 
      Long.Name = grp.object$grp.vals$Long.Name, 
      GroupType = grp.object$grp.vals$GroupType, 
      IsPredator = grp.object$grp.vals$IsPredator, 
      NumCohorts = grp.object$grp.vals$NumCohorts,
      clearance.data)
  } else if (!is.null(grp.object$grp.vals$isPredator)) {
    df.clearance.data <- data.frame(Code = Code, 
      Long.Name = grp.object$grp.vals$Long.Name, 
      GroupType = grp.object$grp.vals$GroupType, 
      isPredator = grp.object$grp.vals$isPredator, 
      NumCohorts = grp.object$grp.vals$NumCohorts,
      clearance.data)
  } else {
    df.clearance.data <- data.frame(Code = Code, 
      Long.Name = grp.object$grp.vals$Long.Name, 
      GroupType = grp.object$grp.vals$GroupType, 
      NumCohorts = grp.object$grp.vals$NumCohorts,
      clearance.data)
  } 
  
  return(list(flagfishrates = flagfishrates, 
    clearance.data = df.clearance.data))
}

# +=========================================================+
# |  make.prm.refuges : collect refuge data for displaying  |
# +=========================================================+
make.prm.refuges <- function(grp.vals, gen.prm, grp.att) {
  j <- which(gen.prm$Parameter == "flag_refuge_model", arr.ind = TRUE)
  if (is.na(gen.prm$Value[j])) {
    gen.prm$Value[j] <- 0 # set no refuge as the default
  }
  flag_refuge_model <- gen.prm$Value[j]
  
  j <- which(gen.prm$Parameter == "flag_rel_cover", arr.ind = TRUE)
  if (is.na(gen.prm$Value[j])) {
    gen.prm$Value[j] <- 0 # set cumulative as the default
  }
  flag_rel_cover <- gen.prm$Value[j]
  
  if (flag_refuge_model == 2) {
    j <- which(gen.prm$Parameter == "RugCover_Coefft", arr.ind = TRUE)
    Rcoefft <- gen.prm$Value[j]
    j <- which(gen.prm$Parameter == "RugCover_Const", arr.ind = TRUE)
    Rconst <- gen.prm$Value[j]
    j <- which(gen.prm$Parameter == "RugCover_Cap", arr.ind = TRUE)
    Rcap <- gen.prm$Value[j]
    j <- which(gen.prm$Parameter == "min_rugosity", arr.ind = TRUE)
    minR <- gen.prm$Value[j]
    j <- which(gen.prm$Parameter == "max_rugosity", arr.ind = TRUE)
    maxR <- gen.prm$Value[j]
  }
  
  model.1 <- data.frame(Code = "xxx", Stage = "xxx", Cover = -1, refuge_status = -1)
  model.2 <- data.frame(Code = "xxx", Rugosity = -1, refuge_status = -1)
  
  for (sp in grp.vals$Code) {
    j <- which(grp.att$Code == sp, arr.ind = TRUE) # get row
    if ((flag_refuge_model == 1) | ((flag_refuge_model == 2) & 
        (grp.vals$GroupType[j] != "CORAL"))) {
      prms <- c(grp.att$Acov_juv[j], grp.att$Bcov_juv[j],
        grp.att$Kcov_juv[j], grp.att$Acov_ad[j], 
        grp.att$Bcov_ad[j], grp.att$Kcov_ad[j])
      if (sum(is.na(prms)) == 0) { # all parameters present
        cover <- seq(from = 0, to = 1, by = 0.05)
        refuge_status_j <- prms[1]*(exp(-prms[3]*cover + prms[2]) + 1.0/prms[3])
        refuge_status_a <- prms[4]*(exp(-prms[6]*cover + prms[5]) + 1.0/prms[6])
        Code <- c(as.character(model.1$Code), rep(as.character(sp), 42))
        Stage <- c(as.character(model.1$Stage), rep("Juvenile", 21), rep("Adult", 21))
        Cover <- c(model.1$Cover, cover, cover)
        refuge_status <- c(model.1$refuge_status, refuge_status_j, refuge_status_a)
        model.1 <- data.frame(Code, Stage, Cover, refuge_status)
      } else { # not enough data to generate the panel plot
        cover <- seq(from = 0, to = 1, by = 0.05)
        refuge_status_j <- rep(NA, 21)
        refuge_status_a <- rep(NA, 21)
        Code <- c(as.character(model.1$Code), rep(as.character(sp), 42))
        Stage <- c(as.character(model.1$Stage), rep("Juvenile", 21), rep("Adult", 21))
        Cover <- c(model.1$Cover, cover, cover)
        refuge_status <- c(model.1$refuge_status, refuge_status_j, refuge_status_a)
        model.1 <- data.frame(Code, Stage, Cover, refuge_status)
      }
    } else if ((flag_refuge_model == 2) & (grp.vals$GroupType[j] == "CORAL")) {
      # rugosity refuge model  
      localRugosity <- seq(from = minR, to = maxR, by = 0.05*(maxR-minR))
      step1 <- Rcoefft*log(localRugosity) + Rconst
      Rscalar <- grp.att$XXX_RugCover_scalar[j]
      refuge_status <- pmin(Rscalar/step1, Rcap)
      
      Code <- c(as.character(model.2$Code), rep(as.character(sp), 21))
      Rugosity <- c(model.2$Rugosity, localRugosity)
      refuge_status <- c(model.2$refuge_status, refuge_status)
      model.2 <- data.frame(Code, Rugosity, refuge_status)
    }
  }
  model.1 <- model.1[-1,] # remove the first row
  model.1$Stage <- factor(model.1$Stage)
  model.2 <- model.2[-1,] # remove the first row
  
  return(list(model.1 = model.1, model.2 = model.2))
}

# +==============================================================+
# |  make.sh.prm.object : collect all parameter data to display  |
# +=====================================--=======================+
#' @title Function that generates a list object used by sh.prm
#'
#' @description
#' Takes data from a box geometry file, a group file, and an Atlantis input parameter file
#' and generates a list object that is the parameter to \code{\link[shinyrAtlantis]{sh.prm}} (see Examples).
#'
#' @param bgm.file Box geometry model (.bgm) file used by Atlantis that defines box boundaries and depths.
#' @param grp.file Text file (.csv) used by Atlantis containing group attributes.
#' @param prm.file Text file (.prm) used by Atlantis containing biological parameters.
#' @export
#'
#' @return R list object.
#'
#' @examples
#' \dontrun{
#' bgm.file <- "SEAP_extended_shelf.bgm"
#' grp.file <- "SEAP_Groups_Aquacult.csv"
#' prm.file <- "SEAP_biol_pH_Aquacult.prm"
#' obj <- make.sh.prm.object(bgm.file, grp.file, prm.file)
#' sh.prm(obj)
#' }
#' @export
#' @importFrom stringr str_extract str_extract_all
make.sh.prm.object <- function(bgm.file, grp.file, prm.file) {
  cat("Generating object (10 steps)\n")
  cat("1.  Extracting map data\n")
  map.objects <- make.prm.map(bgm.file)
  numboxes <- map.objects$numboxes
  
  cat("2.  Reading group summaries\n")
  grp.object <- make.prm.groups(grp.file)
  grp.vals <- grp.object$grp.vals
  Code <- grp.vals$Code
  habitat.types <- grp.object$habitat.types
  
  cat("3.  Extracting general parameters\n")
  gen.prm <- make.prm.general(prm.file)
  
  cat("4.  Extracting group parameters (this may take a few minutes)\n")
  grp.att <- make.prm.attributes(prm.file, grp.vals)
  
  cat("5.  Extracting habitat parameters\n")
  grp.hab <- make.prm.habitats(prm.file, grp.vals, habitat.types) # three data frames
  
  cat("6.  Extracting distribution information\n")
  grp.dist <- make.prm.distributions(prm.file, Code, numboxes)
  
  cat("7.  Extracting migration information\n")
  grp.mig <- make.prm.migration(prm.file, Code)
  
  cat("8.  Extracting prey availability (this may take a few minutes)\n")
  prey.data <- make.prm.prey(prm.file, Code)
  
  cat("9.  Extracting clearance information\n")
  clearance.object <- make.prm.clearance(prm.file, grp.object)
  
  cat("10. Extracting growth information\n")
  growth.data <- make.prm.growth(prm.file, grp.object)
  
  cat("11. Extracting refuge information\n")
  refuge.data <- make.prm.refuges(grp.vals, gen.prm, grp.att)
  
  return(list(
    numboxes = map.objects$numboxes, # number of boxes (scalar)
    map_base = map.objects$map_base, # 
    box.data = map.objects$box.data, 
    grp.def = grp.object$grp.vals,
    habitat.types = habitat.types,
    gen.prm = gen.prm,
    grp.att = grp.att,
    grp.hab = grp.hab,
    grp.dist = grp.dist,
    grp.mig = grp.mig,
    prey.data = prey.data,
    flagfishrates = clearance.object$flagfishrates,
    clearance.data = clearance.object$clearance.data,
    grp.growth = growth.data,
    refuge.data = refuge.data
  ))    
}
