###### Converter QuPath files to FCS ######
# Version 0.3.0 ## 03DEC22
# Gabriel Ascui
# Based on https://github.com/sydneycytometry/CSV-to-FCS/blob/master/CSV-to-FCS%20v2.0.R

###### QuPath TXT to CSV files ######
library(shiny)
library('flowCore')
library('Biobase')
library('data.table')
library('ggplot2')
library('DT')
# Define UI for application that draws a histogram
ui <- fluidPage(
   # Application title
   titlePanel("QuPath Cell Segmentation Measurements to .fcs file"),
   img(src='qupathlogo.png', align = "right", width  = "50px",height = "50px"),
   img(src='ljilogo.jpg', align = "right", width  = "50px",height = "50px"),
   helpText("Version 0.3.0 - Gabriel Ascui"),
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        helpText("Hello everyone, this is a new version of this app, please let me know if you have any issues in the issues tab in github, or at gascui@lji.org "),
        helpText("Upload your TXT measurements files from QuPath 0.4.0"),
        helpText("Maximum file size, 300 MB"),
        fileInput("file1", "Choose QuPath TXT File",
                  accept = c(
                    "text/txt",
                    "text/comma-separated-values,text/plain",
                    ".txt")
                  ),
        hr(),
        ## slider in the sidebar
        helpText("To generate .fcs file, eliminate all columns with names"),
        sliderInput("slider1", label = h3("Columns to be discarted"), 
                    min = 0, 
                    max = 70, # needs to be modified to length(data_qupath)
                    value = 5), #modified by slider
        downloadButton("downloadCSV", "Download CSV"), #download csv botton
        hr(),
        downloadButton("downloadFCS", "Download FCS"), #download fcs botton
        hr(),
        uiOutput("link")
      ),
      # Main Panel
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
                          plotOutput(outputId = "graph1", width  = "300px",height = "200px"),  
                          plotOutput(outputId = "graph2", width  = "300px",height = "200px"),
                          plotOutput(outputId = "graph3", width  = "300px",height = "200px")
                   )
          )
        )
      )
   )
)
options(shiny.maxRequestSize=300*1024^2) # limit of upload file size, set to 300 Mb if needed.
# Define server logic required to draw a histogram
server <- function(input, output) {
  ## Slider Value input to control columns
  sliderValues <- reactive({
    input$slider1
  })
  ## TXT table from original file
  output$txt_data <- renderDataTable({
     inFile <- input$file1
     if (is.null(inFile))
       return(NULL)
     ## Qupath file trimming in R code, adjust with slider1
     data_qupath <- fread(inFile$datapath, header = TRUE, check.names = FALSE)
     data_qupath <- data_qupath[,sliderValues():length(data_qupath)] # get rid of the classes and other informations not stored in FCS files
     DT::datatable(data_qupath)
   })
  ## head of FCS variables to be inserted
  output$fcs_table <- renderDataTable({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    ## Qupath file trimming in R code
    data_qupath <- fread(inFile$datapath, header = TRUE, check.names = FALSE)
    data_qupath <- data_qupath[,sliderValues():length(data_qupath)] # get rid of the classes and other informations not stored in FCS files
    data_qupath <- as.data.frame(data_qupath)
    ## Create FCS file metadata - ranges, min, and max settings
    metadata <- data.frame(name=dimnames(data_qupath)[[2]],desc=paste('column',dimnames(data_qupath)[[2]],'from dataset'))
    metadata$range <- apply(apply(data_qupath,2,range),2,diff)
    metadata$minRange <- apply(data_qupath,2,min)
    metadata$maxRange <- apply(data_qupath,2,max)
    data_subset.ff <- new("flowFrame",exprs=as.matrix(data_qupath), parameters=AnnotatedDataFrame(metadata)) # in order to create a flow frame, data needs to be read as matrix by exprs
    head(data_subset.ff, 10)
  })
  ## Download Handler for CSV file
  output$downloadCSV <- downloadHandler(
    filename = function() {
      paste(input$file1, ".csv", sep = "") 
    },
    content = function(file) {
      inFile <- input$file1
      data_qupath <- fread(inFile$datapath, header = TRUE, check.names = FALSE) #read table
      data_qupath <- data_qupath[,sliderValues():length(data_qupath)] # get rid of the classes and other informations not stored in FCS files
      data_qupath <- as.data.frame(data_qupath) #bind list
      write.csv(data_qupath, file, row.names = FALSE)
    }
  )
  ## Download Handler for FCS file
  output$downloadFCS <- downloadHandler(
    filename = function() {
      paste(input$file1, ".fcs", sep = "") 
    },
    content = function(file) {
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      ## Qupath file trimming in R code
      data_qupath <- fread(inFile$datapath, header = TRUE, check.names = FALSE)
      data_qupath <- data_qupath[,sliderValues():length(data_qupath)] # get rid of the classes and other information not stored in FCS files
      data_qupath <- as.data.frame(data_qupath) ## this works better 
      ### rename centroid parameters 
      colnames(data_qupath)[1] <- "Centroid_X" ; colnames(data_qupath)[2] <- "Centroid_Y"
      #csv_qupath <- write.csv(data_qupath, paste0(input$name, ".csv"))
      ## Create FCS file metadata - ranges, min, and max settings
      metadata <- data.frame(name=dimnames(data_qupath)[[2]],desc=paste('column',dimnames(data_qupath)[[2]],'from dataset')) ### check this 
      metadata$range <- apply(apply(data_qupath,2,range),2,diff)
      metadata$minRange <- apply(data_qupath,2,min)
      metadata$maxRange <- apply(data_qupath,2,max)
      data_subset.ff <- new("flowFrame",exprs=as.matrix(data_qupath), parameters=AnnotatedDataFrame(metadata)) # in order to create a flow frame, data needs to be read as matrix by exprs
      head(data_subset.ff, 10)
      write.FCS(data_subset.ff, file)
    }
  )
  ## github hyperlink
  url <- a("GitHub", href="https://github.com/gabrielascui/qupath_to_fcs")
  output$link <- renderUI({
    tagList("Source code: ", url)
  })
  ## plots on tab 3
  ## plot 1
  output$graph1 <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    data_qupath <- fread(inFile$datapath, header = TRUE, check.names = FALSE)
    data_qupath <- data_qupath[,5:length(data_qupath)]
    coords <- colnames(as.data.frame(data_qupath))[grep("Cell\\:(.*)mean", colnames(as.data.frame(data_qupath)))]
    ggplot(data = as.data.frame(data_qupath), aes(x = .data[[coords[3]]], 
                                                  y = .data[[coords[4]]])) + geom_point()
  })
  ## plot 2
  output$graph2 <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    data_qupath <- fread(inFile$datapath, header = TRUE, check.names = FALSE)
    data_qupath <- data_qupath[,5:length(data_qupath)]
    coords <- colnames(as.data.frame(qupath))[grep("Cell\\:(.*)mean", colnames(as.data.frame(qupath)))]
    ggplot(data = as.data.frame(data_qupath), aes(x = .data[[coords[5]]], 
                                                  y = .data[[coords[3]]])) + geom_point()
  })
  ## plot 3 ## centroids
  output$graph3 <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    data_qupath <- fread(inFile$datapath, header = TRUE, check.names = FALSE)
    data_qupath <- data_qupath[,5:length(data_qupath)]
    ggplot(data = as.data.frame(data_qupath), aes(x =data_qupath$`Centroid X µm`, y =data_qupath$`Centroid Y µm`)) + geom_point()
  })
  ## Slider
  output$value <- renderPrint({ input$slider1 }) #slider handler
}

# Run the application 
shinyApp(ui = ui, server = server)

