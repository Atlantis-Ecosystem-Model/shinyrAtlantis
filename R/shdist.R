
#' shiny application for generating Atlantis horizontal distributions
#'
#' @param map.object 
#'
#' @export
sh.dist <- function(map.object){
  # Global parameters
  num.boxes <- map.object$numboxes
  map.vertices <- map.object$map.vertices
  
  box.data   <- map.object$box.data
  box.data$z <- - box.data$z
  largest.box <- which(box.data$area == max(box.data$area)) # index not id
  max.depth <- max(box.data$z)
  
  df.map              <- merge(map.vertices, box.data, by = "boxid")
  df.map$valid.z      <- TRUE
  df.map$distribution <- TRUE
  
  txtHelp <- "<p>This tool can be used to create simple horizontal probability distributions that can then be cut-and-pasted into an <b>Atlantis</b> parameter file (.prm).</p>"
  txtHelp <- paste(txtHelp, "<p>The horizontal range of aquatic species is often related to a depth range. This tool uses depth information to help generate a horizontal probability distribution when the species is assumed to be <b>uniformly distributed</b> in the horizontal.</p>") 
  txtHelp <- paste(txtHelp, "<p>To create the probability distribution enter the depth range of the species into the side panel. This change will trigger the boxes that include the depth range to be displayed in both plots.</p>")
  txtHelp <- paste(txtHelp, "<p>The left plot depicts the boxes that satisfy the specified depth range. The right plot depicts the horizontal range of the species. The text below each plot presents the boxes that are highlighted in the associated plot.</p>")
  txtHelp <- paste(txtHelp, "<p>Add or remove boxes to define the realised horizontal range, by referring to a distribution map for the species. If depth dominates the range of a species then the right plot should encompass the species' horizontal range. Usually you will have to remove boxes as other environmental factors limit the potential distribution of a species. Make sure that the bounding boxes are removed from the horizontal distribution.</p>")
  txtHelp <- paste(txtHelp, "<p>The wide text box below both plots presents the fraction of the population present in all boxes that define its range (as specified by the right plot). These values assume that the species is uniformly distributed in the horizontal across its range.</p>")
  txtHelp <- paste(txtHelp, "<p>Enter into <em>Header text</em> the text that <b>Atlantis</b> uses to identify the meaning of the probability distribution. The header text above the probability distribution will match this change and it will also add the number of boxes in the domain.</p>")
  txtHelp <- paste(txtHelp, "<p>When you are satisfied with the probability distribution cut-and-paste the text in the wide text box into the .prm file.</p>")

  shinyApp(
    
    # USER INPUT FUNCTION
    
    ui = navbarPage(
      title = "Atlantis horizontal distribution creator",
      tabPanel("Create", 
        sidebarLayout(
          sidebarPanel(width = 2,
            numericInput("DepthMin", "Minimum depth (m)", 
              value = 1, min = 1, max = max.depth),
            numericInput("DepthMax", "Maximum depth (m)", 
              value = max.depth + 1, min = 1, max = max.depth + 1),
            hr(),
            selectInput("BoxAdd", "Add box", 0:(num.boxes-1)),
            selectInput("BoxRemove", "Remove box", 0:(num.boxes-1)),
            hr(),
            textInput("TIHeader", "Header text", value = "XXX")
          ),
          mainPanel(width = 10,
            fluidRow(
              column(6, h4("Depth distribution")),
              column(6, h4("Species distribution"))
            ),
           fluidRow(
              column(6, plotOutput("plot.map", height = "375px")),
              column(6, plotOutput("plot.distribution", height = "375px"))
           ),
            fluidRow(h4("Boxes")),
            fluidRow(
             column(6, verbatimTextOutput("txtValid")),
             column(6, verbatimTextOutput("txtDist"))
           ),
           fluidRow(h4("Text to enter into an Atlantis (.prm) input file")),
           fluidRow(
             column(12, verbatimTextOutput("txtDistribution"))
           ),
           hr(),
           fluidRow(h4("Box locations")),
           fluidRow(
             column(12, plotOutput("plot.boxes", height = "750px"))
            )
          )
        )
      ),  
      tabPanel("Help", 
        fluidPage(HTML(txtHelp))
      ),
      tabPanel(actionButton("exitButton", "Exit")) 
  ),
      
  # SERVER FUNCTION
    
  server <- function(input, output) {

    # create a reactive variable containing relevant box information
    values <- reactiveValues()
    values$box.id       <- 0:(num.boxes-1) # box id
    values$valid.depth  <- rep(TRUE, num.boxes) # box is in depth range
    values$distribution <- rep(TRUE, num.boxes) # box is in distribution range
    values$txtAtlantis  <- "" # text that can be cut-pasted into Atlantis prm file

    # Display spatial distribution bounded by prescribed depths        
    output$plot.map <- renderPlot({
      min.depth <- input$DepthMin # minimum depth to display in polygons
      max.depth <- input$DepthMax # maximum depth to display in polygons
      
      df.map$valid.z <- ifelse((df.map$z >= min.depth) & (df.map$z < max.depth), 
        df.map$z, NA) # remove depths not in desired range

      ggplot(data = df.map, aes(x = x, y = y, group = boxid, fill = valid.z)) +
        geom_polygon(colour = "grey90", size = 0.25) +          
        scale_fill_gradient(low = "#9ecae1", high = "#084594", na.value="grey90",
          limits=c(min.depth, max.depth)) +
        labs(fill = "Depth (m)") +
        geom_text(aes(x = x.in, y = y.in, label = boxid), size = 2.5) +
        theme_bw() + xlab("") + ylab("") +
        theme(plot.background = element_blank()) +
        scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL)
    })

    # Display the spatial distribution of the species        
    output$plot.distribution <- renderPlot({
      # set boxes within species range to TRUE
      df.map$distribution <- values$distribution[df.map$boxid+1]

      ggplot(data = df.map, aes(x = x, y = y, group = boxid, fill = distribution)) +
        geom_polygon(colour = "grey40", size = 0.25) +          
        geom_text(aes(x = x.in, y = y.in, label = boxid), size = 2.5) +
        labs(fill = "Present") +
        scale_fill_manual(values=c("tomato", "springgreen")) +
        theme_bw() + xlab("") + ylab("") +
        theme(plot.background = element_blank()) +
        scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL)
    })

    # Display the spatial distribution of boxes        
    output$plot.boxes <- renderPlot({
      ggplot(data = df.map, aes(x = x, y = y, group = boxid)) +
        geom_polygon(colour = "khaki3", size = 0.25, fill = "khaki1") +          
        geom_text(aes(x = x.in, y = y.in, label = boxid), size = 2.5) +
        theme_bw() + xlab("") + ylab("") +
        theme(plot.background = element_blank())
    })

    # Print the text that can be pasted into Atlantis
    output$txtDistribution <- renderText({ 
      values$txtAtlantis # a reactive variable
    })
    
    # Respond to a change in depth range
    observeEvent(c(input$DepthMin, input$DepthMax), {
      min.depth <- input$DepthMin
      max.depth <- input$DepthMax
       
      values$valid.depth <- (box.data$z >= min.depth) & 
        (box.data$z < max.depth)
      values$distribution <- values$valid.depth # causes a response
    })

    # Remove a box from the species' range
    observeEvent(input$BoxRemove, {
      values$distribution[as.numeric(input$BoxRemove)+1] <- FALSE
    })
    
    # Add a box to the species' range
    observeEvent(input$BoxAdd, {
      values$distribution[as.numeric(input$BoxAdd)+1] <- TRUE
    })

    # Respond to changes that affect the Atlantis output
    observeEvent(c(values$distribution, input$TIHeader), {
      # calculate the fractions of cover for the boxes in the species' range
      area.total <- sum(box.data$area[values$box.id[values$distribution]+1])
      frac.short <- box.data$area[values$box.id[values$distribution]+1] /
        area.total
      txt.tmp.2 <- paste(format(round(frac.short, 4), nsmall = 4), collapse = " ")

      # make sure that fractions when rounded to 4 d.p. sum exactly to 1
      largest.box <- which(frac.short == max(frac.short))[1] # index not id
      rounded.vals <- as.numeric(unlist(str_split(txt.tmp.2, pattern = " ")))
      eps <- 1 - sum(rounded.vals)      
      rounded.vals[largest.box] <- rounded.vals[largest.box] + eps
      frac.long <- rep(0, num.boxes)
      frac.long[values$box.id[values$distribution]+1] <- rounded.vals
      txt.tmp.2 <- paste(format(round(frac.long, 4), nsmall = 4), collapse = " ")
      
      # generate the header text
      txt.tmp.1 <- paste(input$TIHeader, " ", as.character(num.boxes), "\n", sep = "")
      # combine the info text and fractions text for printing
      values$txtAtlantis <- paste(txt.tmp.1, txt.tmp.2, sep = "")
    })
    
    # Print the box IDs within the depth range
    output$txtValid <- renderText({ 
      paste(values$box.id[values$valid.depth])
    })
      
    # Print the box IDs within the species' range
    output$txtDist <- renderText({ 
      paste(values$box.id[values$distribution])
    })

    # Respond to pressing the exit button
    observeEvent(input$exitButton, {
      stopApp()
    })
  }
    
  ) # End of shinyApp
}

#' collect data to display the map
#'
#' @param bgm.file 
#'
#' @export
make.dist.object <- function(bgm.file){
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
  return(list(numboxes = numboxes, map.vertices = map.vertices, box.data = box.data))
}
