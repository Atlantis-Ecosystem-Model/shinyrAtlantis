# # shprm.R
# # 15/01/2016
# 
# rm(list = ls()) # clear memory
# 
# library(shiny)
# library(dplyr)
# library(ggplot2)
# library(DT)
# library(stringr)

# +==================================================================+
# |  sh.prm : shiny application for viewing Atlantis prm input file  |
# +==================================================================+
#' Shiny Parameters
#'
#' @param obj object from \code{\link{mark.prm.object}}
#' @param def.grp.file groups def
#' @return object of class 'shiny.appobj' see \code{\link[shiny]{shinyApp}}
#' @export
#'
sh.prm <- function(obj, def.grp.file){
  # obj is a list: numboxes, map_base, box.data, grp.def, grp.att, 
  #   gen.prm, grp.hab, grp.dist
  
  # set up variables needed to plot the map  
  df.map    <- merge(obj$map_base, obj$box.data, by = "boxid")
  depth.min <- min(df.map$z) # deepest box
  depth.max <- max(df.map$z) # shallowest box
  # set slider limits so plot always renders
  depth.display.min <- round(depth.min - 2) 
  depth.display.max <- round(depth.max - 1)
  
  numboxes <- obj$numboxes
  
  # checkbox labels for group templates
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
          column(12, wellPanel(
            sliderInput("SI.max.depth", label = "Maximum depth to display", 
              min = depth.display.min, max = depth.display.max, 
              value = depth.display.min, round = FALSE)))
        ),
        fluidRow(  
          column(2),
          column(8,
            plotOutput("box_location_plot", height = "500px"))
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
      tabPanel("Distribution",
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
          fluidRow(column(12, h3("Distribution of age-groups"))),
          fluidRow(
            column(5, h4("Invertebrate/Adult")),
            column(5, h4("Juvenile"))
          ),
          fluidRow(
            column(5, 
              plotOutput("H_ad_distribution_plot", height = "325px")),
            column(5, 
              plotOutput("H_juv_distribution_plot", height = "325px"))
          ),
          fluidRow(
            column(5, plotOutput("vert_distribution_plot", height = "400px")),
            column(5, plotOutput("vert_juv_distribution_plot", height = "400px"))
          ),
          hr(),
          fluidRow(column(12, h4("Density plots (per unit area): FXXX_SQ[juv]"))),
          fluidRow(
            column(5, 
              plotOutput("H_ad_distribution_area_plot", height = "400px")),
            column(5, 
              plotOutput("H_juv_distribution_area_plot", height = "400px"))
          ),
          hr(),
          fluidRow(column(12, h3("Recruitment"))),
          fluidRow(
            column(5, 
              plotOutput("H_recruit_plot", height = "325px")),
            column(5, 
              plotOutput("H_recruit_area_plot", height = "325px"))
          ),
          fluidRow(
            column(5, 
              plotOutput("vertical_recruit_plot", height = "400px"))
          ),
          width = 10
        )
      ),
      # Refuge
      tabPanel("Refuge",
        fluidRow(column(12, h4("Cover-based availability"))),
        fluidRow(column(12, 
          plotOutput("refuge.1.plot", height = "625px"))),
        hr(),
        fluidRow(column(12, h4("Refuge-based availability"))),
        fluidRow(column(12, 
          plotOutput("refuge.2.plot", height = "200px")))
      ),  
      tabPanel("Feeding",
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
        fluidRow(
          column(12, DT::dataTableOutput('table.age.prey'))
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
      
      observeEvent(input$exitButton, {
        stopApp()
      })
      
      output$box_location_plot <- renderPlot({
        df.map$limitedz <- ifelse((df.map$z >= input$SI.max.depth) & (df.map$z < 0), 
          df.map$z, NA)
        ggplot(data = df.map, aes(x = x, y = y, group = boxid, fill = limitedz)) +
          geom_polygon(colour = "grey90", size = 0.25) +          
          scale_fill_gradient(high = "#9ecae1", low = "#084594", na.value="grey90",
            limits=c(input$SI.max.depth, 0)) +
          labs(fill = "Depth (m)") +
          geom_text(aes(x = x.in, y = y.in, label = boxid), size = 2.5) +
          theme_bw() + xlab("") + ylab("") +
          theme(plot.background = element_blank()) + 
          scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL)
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
            geom_polygon(colour = "grey90", size = 0.25) +          
            scale_fill_gradient(high = "blue", low = "white", na.value="grey90") +
            labs(fill = "Fraction") + ggtitle("FXXX_SQ") + 
            theme_bw() + xlab("") + ylab("") +
            theme(plot.background = element_blank()) + 
            scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL)
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
            geom_polygon(colour = "grey90", size = 0.25) +          
            scale_fill_gradient(high = "blue", low = "white", na.value="grey90") +
            labs(fill = "Fraction") + ggtitle("FXXX_SQjuv") + 
            theme_bw() + xlab("") + ylab("") +
            theme(plot.background = element_blank()) + 
            scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL)
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
            scale_colour_manual(values=c("#FF6666", "blue")) + 
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
            scale_colour_manual(values=c("#FF6666", "blue")) + 
            # scale_colour_manual(values=c("red", "blue")) + 
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
            scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL)
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
            scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL)
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
            geom_polygon(colour = "grey90", size = 0.25) +          
            scale_fill_gradient(high = "blue", low = "white", na.value="grey90") +
            labs(fill = "Fraction") + ggtitle("XXX_recruit_hdistrib") + 
            theme_bw() + xlab("") + ylab("") +
            theme(plot.background = element_blank()) + 
            scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL)
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
            scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL)
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
          theme_bw() + theme(plot.background = element_blank())
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
            theme_bw() + theme(plot.background = element_blank())
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
            scale_fill_manual(values=c("#FF6666", "blue")) + 
            xlab("Predator is adult") + ylab("Fraction") +
            facet_wrap( ~ Prey, ncol = 8) +
            labs(fill = "Prey is adult") +
            theme_bw() + 
            theme(
              panel.grid.minor = element_blank(),
              plot.background = element_blank()
            )
        }  
      })
    }
  )  
}

#' @importFrom stringr str_extract str_extract_all
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
          (stringr::str_extract(text.split[2], "[0-9.-]+") == text.split[2])) {
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
  
  return (list(grp.vals = df, habitat.types = habitat.types))
}  

# +======================================================================+
# |  make.prm.attributes : collect group attributes data for displaying  |
# +======================================================================+
make.prm.attributes <- function(prm.file, grp.vals, def.grp.file){
  prm <- readLines(prm.file) # read in the biological parameter file
  grp.att <- grp.vals[c("Code", "Name", "GroupType")] # develop this data frame
  
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
make.prm.general <- function(prm.file, def.all.file){
  prm <- readLines(prm.file) # read in the biological parameter file
  # read in the parameter definition file
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
  grp.vert.day   <- array(NA, dim=c(codes, 10))
  grp.vert.night <- array(NA, dim=c(codes, 10))
  dimnames(grp.vert.day)[[1]]  <- Code
  dimnames(grp.vert.night)[[1]] <- Code
  
  # Juvenile vertebrates
  grp.vert.juv.day   <- array(NA, dim=c(codes, 10))
  grp.vert.juv.night <- array(NA, dim=c(codes, 10))
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
      vals <- as.numeric(str_split(prm[j+1], "[\t ]+")[[1]])
      grp.vert.day[xxx,1:levs] <- vals[1:levs]
    }  
    txt.find <- paste("VERTnight_",xxx,sep = "")
    j <- grep(pattern = txt.find, x = prm, value = FALSE) # file row(s)
    if (length(j) == 1) { # unique row
      levs <- as.numeric(unlist(str_extract_all(prm[j],"\\(?[0-9.-]+\\)?")[[1]])[1])
      vals <- as.numeric(str_split(prm[j+1], "[\t ]+")[[1]])
      grp.vert.night[xxx,1:levs] <- vals[1:levs]
    }
    
    # Vertebrates (juveniles and adults): 
    
    # Juvenile vertebrates
    txt.find <- paste("VERTday_",xxx,"1",sep = "")
    j <- grep(pattern = txt.find, x = prm, value = FALSE) # file row(s)
    if (length(j) == 1) { # unique row
      levs <- as.numeric(unlist(str_extract_all(prm[j],"\\(?[0-9.-]+\\)?")[[1]])[2])
      vals <- as.numeric(str_split(prm[j+1], "[\t ]+")[[1]])
      grp.vert.juv.day[xxx,1:levs] <- vals[1:levs]
    }  
    txt.find <- paste("VERTnight_",xxx,"1",sep = "")
    j <- grep(pattern = txt.find, x = prm, value = FALSE) # file row(s)
    if (length(j) == 1) { # unique row
      levs <- as.numeric(unlist(str_extract_all(prm[j],"\\(?[0-9.-]+\\)?")[[1]])[2])
      vals <- as.numeric(str_split(prm[j+1], "[\t ]+")[[1]])
      grp.vert.juv.night[xxx,1:levs] <- vals[1:levs]
    }
    
    # Adult vertebrates
    txt.find <- paste("VERTday_",xxx,"2",sep = "")
    j <- grep(pattern = txt.find, x = prm, value = FALSE) # file row(s)
    if (length(j) == 1) { # unique row
      levs <- as.numeric(unlist(str_extract_all(prm[j],"\\(?[0-9.-]+\\)?")[[1]])[2])
      vals <- as.numeric(str_split(prm[j+1], "[\t ]+")[[1]])
      grp.vert.day[xxx,1:levs] <- vals[1:levs]
    }  
    txt.find <- paste("VERTnight_",xxx,"2",sep = "")
    j <- grep(pattern = txt.find, x = prm, value = FALSE) # file row(s)
    if (length(j) == 1) { # unique row
      levs <- as.numeric(unlist(str_extract_all(prm[j],"\\(?[0-9.-]+\\)?")[[1]])[2])
      vals <- as.numeric(str_split(prm[j+1], "[\t ]+")[[1]])
      grp.vert.night[xxx,1:levs] <- vals[1:levs]
    }
  }
  
  # Extract recruitment data
  
  # Horizontal recruitment
  grp.hor.recruit  <- array(NA, dim=c(codes, numboxes))
  dimnames(grp.hor.recruit)[[1]]  <- Code
  # Vertical recruitment
  grp.vert.recruit <- array(NA, dim=c(codes, 10))
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
      vals <- as.numeric(str_split(prm[j+1], "[\t ]+")[[1]])
      grp.vert.recruit[xxx,1:levs] <- vals[1:levs]
    }  
  }
  
  return(list(hor.ad = grp.hor.ad, hor.juv = grp.hor.juv,
    vert.day = grp.vert.day, vert.night = grp.vert.night,
    vert.juv.day = grp.vert.juv.day, vert.juv.night = grp.vert.juv.night,
    hor.recruit = grp.hor.recruit, vert.recruit = grp.vert.recruit))
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

# +=========================================================+
# |  make.prm.refuges : collect refuge data for displaying  |
# +=========================================================+
make.prm.refuges <- function(grp.vals, gen.prm, grp.att) {
  j <- which(gen.prm$Parameter == "flag_refuge_model", arr.ind = TRUE)
  flag_refuge_model <- gen.prm$Value[j]
  
  j <- which(gen.prm$Parameter == "flag_rel_cover", arr.ind = TRUE)
  flag_rel_cover <- gen.prm$Value[j]

  if (flag_refuge_model == 2) {
    j <- which(gen.prm$Parameter == "RugCover_Coefft", arr.ind = TRUE)
    Rcoefft <- gen.prm$Value[j]
    j <- which(gen.prm$Parameter == "RugCover_Const", arr.ind = TRUE)
    Rconst <- gen.prm$Value[j]
    j <- which(gen.prm$Parameter == "RugCover_Cap", arr.ind = TRUE)
    Rcap<- gen.prm$Value[j]
    j <- which(gen.prm$Parameter == "min_rugosity", arr.ind = TRUE)
    minR<- gen.prm$Value[j]
    j <- which(gen.prm$Parameter == "max_rugosity", arr.ind = TRUE)
    maxR<- gen.prm$Value[j]
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
  model.1 <- model.1[-1,]# remove the first row
  model.1$Stage <- factor(model.1$Stage)
  model.2 <- model.2[-1,]# remove the first row
  
  return(list(model.1 = model.1, model.2 = model.2))
}

# +============================================================+
# |  make.init.object : collect all parameter data to display  |
# +============================================================+
#' Make PRM object
#'
#' @param bgm.file  BGM
#' @param grp.file  groups
#' @param prm.file  parameters
#' @param def.all.file defs
#' @param def.grp.file def groups
#' @return list . . .
#' @export
#'
#' @examples
#' #See readme
make.prm.object <- function(bgm.file, grp.file, prm.file, def.all.file, def.grp.file) {
  cat("-- Extracting map data\n")
  map.objects <- make.prm.map(bgm.file)
  numboxes <- map.objects$numboxes
  
  cat("-- Reading group summaries\n")
  grp.object <- make.prm.groups(grp.file)
  grp.vals <- grp.object$grp.vals
  Code <- grp.vals$Code
  habitat.types <- grp.object$habitat.types
  
  cat("-- Extracting general parameters\n")
  gen.prm <- make.prm.general(prm.file, def.all.file)
  
  cat("-- Extracting group parameters (this may take a few minutes)\n")
  grp.att <- make.prm.attributes(prm.file, grp.vals, def.grp.file)
  
  cat("-- Extracting habitat parameters\n")
  grp.hab <- make.prm.habitats(prm.file, grp.vals, habitat.types) # three data frames
  
  cat("-- Extracting horizontal distributions (this may take a few minutes)\n")
  grp.dist <- make.prm.distributions(prm.file, Code, numboxes)
  
  cat("-- Extracting prey availability (this may take a few minutes)\n")
  prey.data <- make.prm.prey(prm.file, Code)
  
  cat("-- Extracting refuge information\n")
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
    prey.data = prey.data,
    refuge.data = refuge.data
  ))    
}

# # === sh.ast R code needed if supported by a package ===
# 
# # globally available file names
# wd <- "/Users/ric352/Documents/Projects/Fisheries/Atlantis/Setup Tool/"
# setwd(wd)
# def.all.file <- paste(wd, "paramdefns.csv", sep = "")
# def.grp.file <- paste(wd, "grpTemplates.csv", sep = "")
# 
# # AMS model
# wd <- "/Users/ric352/Documents/Projects/Fisheries/Atlantis/Setup Tool/AMS/"
# setwd(wd)
# bgm.file <- paste(wd, "ams71.bgm", sep = "")
# grp.file <- paste(wd, "AMSGroups_SPF.csv", sep = "")
# prm.file <- paste(wd, "ams71_SPF_biology.prm", sep = "")
# obj.ast.AMS <- make.prm.object(bgm.file, grp.file, prm.file)
# sh.prm(obj.ast.AMS) # run the shiny App
# # 
# # # Baltic model
# # wd <- "/Users/ric352/Documents/Projects/Fisheries/Atlantis/Setup Tool/Baltic/"
# # setwd(wd)
# # bgm.file <- paste(wd, "Baltic_aea_v2.bgm", sep = "")
# # grp.file <- paste(wd, "BalticGroups_Atlantis2015.csv", sep = "")
# # prm.file <- paste(wd, "Baltic_biol_FSnoEvo.prm", sep = "")
# # obj.ast.Baltic <- make.setup.object(bgm.file, grp.file, prm.file)
# # sh.ast(obj.ast.Baltic) # run the shiny App
# # 
# # JFRE model
# # wd <- "/Users/ric352/Documents/Projects/Fisheries/Atlantis/Setup Tool/JFRE"
# # setwd(wd)
# # bgm.file <- paste(wd, "JFRE_ll.bgm", sep = "")
# # grp.file <- paste(wd, "JFREGroups.csv", sep = "")
# # prm.file <- paste(wd, "JFREBiol.prm", sep = "")
# # habitat.types.JFRE <- c("MA", "reef", "flat", "soft", "canyon")
# # obj.ast.JFRE <- make.setup.object(bgm.file, grp.file, prm.file, habitat.types.JFRE)
# # sh.ast(obj.ast.JFRE) # run the shiny App
# # 
# # SEAP model
# # wd <- "/Users/ric352/Documents/Projects/Fisheries/Atlantis/Setup Tool/SEAP/"
# # setwd(wd)
# # bgm.file <- paste(wd, "SEAP_extended_shelf.bgm", sep = "")
# # grp.file <- paste(wd, "SEAP_Groups_Aquacult.csv", sep = "")
# # prm.file <- paste(wd, "SEAP_biol_pH_Aquacult.prm", sep = "")
# # obj.ast.SEAP <- make.setup.object(bgm.file, grp.file, prm.file)
# # sh.ast(obj.ast.SEAP) # run the shiny App
# # 
# # GBR model
# # wd <- "/Users/ric352/Documents/Projects/Fisheries/Atlantis/Setup Tool/GBR/"
# # setwd(wd)
# # bgm.file <- paste(wd, "gbr_test.bgm", sep = "")
# # grp.file <- paste(wd, "GBRGroups.csv", sep = "")
# # prm.file <- paste(wd, "GBR_biol SAR.prm", sep = "")
# # obj.ast.GBR <- make.prm.object(bgm.file, grp.file, prm.file)
# # sh.prm(obj.ast.GBR) # run the shiny App
# # 
# # Guam model
# # wd <- "/Users/ric352/Documents/Projects/Fisheries/Atlantis/Setup Tool/Guam/"
# # setwd(wd)
# # bgm.file <- paste(wd, "Guam_utm1.bgm", sep = "")
# # grp.file <- paste(wd, "GUAM_FGDemCoral.csv", sep = "")
# # prm.file <- paste(wd, "GUAM_biolW_Dem.prm", sep = "")
# # obj.ast.Guam <- make.setup.object(bgm.file, grp.file, prm.file)
# # sh.ast(obj.ast.Guam) # run the shiny App
# # 
# # === vat::create_vadt R code needed if supported by a package ===
# # 
# # wdir <- "~/Atlantis/RunFiles/SEAP/" # working directory
# # setwd(wdir)
# # 
# # outdir    <- "SEAPout/"
# # fgfile    <- "params/SEAP_Groups_Aquacult.csv"
# # biolprm   <- "params/SEAP_biol_pH_Aquacult.prm"
# # ncout     <- "SEAPoutput"
# # startyear <- 2010 
# # toutinc   <- 365
# # diet      <- TRUE
# # 
# # obj.vat <- create_vadt(outdir = outdir, fgfile = fgfile, biolprm = biolprm, 
# #   ncout = ncout, startyear = 2010, toutinc = 365, diet = TRUE)
# # 
# # vadt(obj.vat, NULL)
