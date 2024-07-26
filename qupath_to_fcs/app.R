##======================================## Converter QuPath files to FCS ##======================================##
# Version 0.5.5 ## 12SEP23
# Gabriel Ascui
# Based on https://github.com/sydneycytometry/CSV-to-FCS/blob/master/CSV-to-FCS%20v2.0.R
# Using shiny 1.7.4 with shiny modules

##------------------------------------------------------ libraries
library(shiny)
library(flowCore)
library(Biobase)
library(data.table)
library(ggplot2)
library(DT)
library(stringi)

##------------------------------------------------------ shiny modules
# automatically sourced
# R/downloadCSV.R
# R/downloadFCS.R
# R/loadMeasurements.R
# R/plots.R

##------------------------------------------------------ config
# limit of upload file size, set to 300 Mb if needed.
options(shiny.maxRequestSize=100*1024^2)

##------------------------------------------------------ APP UI
ui <- fluidPage(
  ##--------------------------------------------------- Title panel
  titlePanel("QuPath Cell Segmentation Measurements to .fcs file"),
  img(src='qupathlogo.png', align = "right", width  = "50px",height = "50px"),
  img(src='ljilogo.jpg', align = "right", width  = "50px",height = "50px"),
  helpText("Version 0.5.7 - Gabriel Ascui"),
  ##--------------------------------------------------- Side panel
  sidebarLayout(
    sidebarPanel(
      helpText("Hello everyone, this is a new version of this app, please let me know if you have any issues in the issues tab in github, or at gascui@lji.org "),
      helpText("Upload your TXT measurements files from QuPath 0.4.0"),
      helpText("Maximum file size, 100 MB"),
      fileInput("file1", "Choose QuPath TXT File",
                accept = c(
                  "text/txt",
                  "text/comma-separated-values,text/plain",
                  ".txt")
      ),
      hr(),
      ##----------------------------------------------- slider in the sidebar
      helpText("To generate .fcs file, eliminate all columns with names"),
      sliderInput("slider1", label = h3("Columns to be discarted"), 
                  min = 0, 
                  max = 70, # needs to be modified to max length(data_qupath)
                  value = 5), 
      ##----------------------------------------------- download buttons 
      downloadButton("data_download", label = "Download CSV"),
      hr(),
      downloadButton("downloadFCS", label = "Download FCS"), 
      hr(),
      uiOutput("link"),
      hr(),
      varSelectInput("plot_var_x","Select Graph x-axis", data = NULL),
      varSelectInput("plot_var_y","Select Graph y-axis", data = NULL)
    ),
    ##-------------------------------------------------- Main Panel
    mainPanel(
      tabsetPanel(
        id = 'dataset_name',
        tabPanel("Input file", 
                 p("this is your trimmed file"),
                 hr(),
                 dataTableOutput("txt_data")), #txt file from QuPath
        tabPanel("FCS file", 
                 p("this is the head of your fcs frame file"),
                 hr(),
                 dataTableOutput("fcs_table")), #fcs frame to be converted to FCS
        tabPanel("Dot plots", 
                 p("this is an example of the fcs file"),
                 hr(),
                 column(2, align="right",
                        plotOutput(outputId = "graph1", width  = "300px",height = "300px"),
                        plotOutput(outputId = "graph2", width  = "300px",height = "300px")
                 )
        )
      )
    )
  )
)

##------------------------------------------------------- APP Server
server <- function(input, output, session) {
  ##----------------------------------------------------- Slider reactive value
  sliderValues <- reactive({
    input$slider1
  })
  ##----------------------------------------------------- Slider output
  output$value <- renderPrint({ input$slider1 })
  ##----------------------------------------------------- measurement table filter/update reactivity
  tableInput <- reactive({
    req(input$file1)
    # Determine the original encoding of the file and convert to UTF-8
    encoding <- readr::guess_encoding(input$file1$datapath)$encoding[1]
    file_contents <- readLines(input$file1$datapath, encoding = encoding)
    data_qupath <- fread(text = iconv(file_contents, from = encoding, to = "UTF-8"), encoding = "UTF-8")
    data_qupath <- data_qupath[,sliderValues():length(data_qupath)]
    data_qupath <- as.data.frame(data_qupath)
    
  })
  ##----------------------------------------------------- Preview of TXT measurements table from original file
  output$txt_data <- renderDataTable({
    tableInput()
  })
  ##----------------------------------------------------- FCS file reactivity
  fcsInput <- reactive({
    inFile <- input$file1
    if(is.null(input$file1)) return (NULL) else {
       # Determine the original encoding of the file and convert to UTF-8
      encoding <- readr::guess_encoding(inFile$datapath)$encoding[1]
      file_contents <- readLines(inFile$datapath, encoding = encoding)
      file_contents <- iconv(file_contents, from = encoding, to = "UTF-8")

      data_qupath <- fread(text = file_contents, header = TRUE, check.names = FALSE, encoding = "UTF-8")
      data_qupath <- data_qupath[,sliderValues():length(data_qupath)]
      data_qupath <- as.data.frame(data_qupath)
      ## rename centroid parameters 
      colnames(data_qupath)[1] <- "Centroid_X" ; colnames(data_qupath)[2] <- "Centroid_Y"
      ## fcs metadata
      metadata <- data.frame(name=dimnames(data_qupath)[[2]],desc=paste('column',dimnames(data_qupath)[[2]],'from dataset'))
      metadata$range <- apply(apply(data_qupath,2,range),2,diff)
      metadata$minRange <- apply(data_qupath,2,min)
      metadata$maxRange <- apply(data_qupath,2,max)
      data_subset.ff <- new("flowFrame",exprs=as.matrix(data_qupath), parameters=AnnotatedDataFrame(metadata))
      data_subset.ff
      }
  })
  ##------------------------------------------------------ Preview of FCS variables to be inserted
  output$fcs_table <- renderDataTable({
    head(fcsInput())
  })
  ##------------------------------------------------------ Download handler for CSV file
  output$data_download <- downloadHandler(
    filename = function() {
      paste("measurements_", Sys.Date(),".csv", sep = "") 
    },
    content = function(file) {
      write.csv(tableInput(), file, row.names = FALSE)
    }
  )
  ##------------------------------------------------------ Download Server for FCS file
  output$downloadFCS <- downloadHandler(
    filename = function() {
      paste("measurements_", Sys.Date(),".fcs", sep = "") 
    },
    content = function(file) { 
      write.FCS(fcsInput(), file)
      }
  )
  ##------------------------------------------------------ update selected columns
  observeEvent(input$file1, {
    data_qupath <- as.data.frame(tableInput())
    col_n <- names(data_qupath)
    # update choices
    updateSelectInput(session, "plot_var_x", choices = col_n)
    updateSelectInput(session, "plot_var_y", choices = col_n)
  })
  ##----------------------------------------------------- plots
  ##----------------------------------------------------- plot 1 centroids 
  output$graph1 <- renderPlot({
    ggplot(data = as.data.frame(tableInput()), aes(x = `Centroid X µm`, 
                                                   y = `Centroid Y µm`)) + geom_point()
  })
  ##---------------------------------------------------- plot 2
  output$graph2 <- renderPlot({
    ggplot(data = as.data.frame(tableInput()), aes_string(x = input$plot_var_x, 
                                                  y = input$plot_var_y)) + geom_point()
  })
  ##------------------------------------------------- github hyperlink
  url <- a("GitHub", href="https://github.com/gabrielascui/qupath_to_fcs")
  output$link <- renderUI({
    tagList("Source code: ", url)
  })
}


##--------------------------------------------------- Run app
shinyApp(ui, server)