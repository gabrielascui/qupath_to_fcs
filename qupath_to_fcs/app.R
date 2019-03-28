###### Converter QuPath files to FCS ######
# Version 0.2.2
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
   titlePanel("QuPath Cell Segmentation Measurements to .fcs file\n Version 0.2.2 - Gabriel Ascui"),
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        helpText("Upload your TXT measurements files from QuPath 0.1.3 or 0.2.m2"),
        helpText("Maximum file size, 100 MB"),
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
        downloadButton("downloadFCS", "Download FCS") #download fcs botton
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
                          plotOutput(outputId = "graph2", width  = "300px",height = "200px")
                   )
          )
        )
      )
   )
)
options(shiny.maxRequestSize=100*1024^2) # limit of upload file size, set to 100 Mb.
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
    data_qupath <- rbindlist(list(data_qupath))
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
      data_qupath <- rbindlist(as.list(data_qupath)) #bind list
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
      data_qupath <- data_qupath[,sliderValues():length(data_qupath)] # get rid of the classes and other informations not stored in FCS files
      data_qupath <- rbindlist(list(data_qupath)) # here it needs to be a list, so list()
      #csv_qupath <- write.csv(data_qupath, paste0(input$name, ".csv"))
      ## Create FCS file metadata - ranges, min, and max settings
      metadata <- data.frame(name=dimnames(data_qupath)[[2]],desc=paste('column',dimnames(data_qupath)[[2]],'from dataset'))
      metadata$range <- apply(apply(data_qupath,2,range),2,diff)
      metadata$minRange <- apply(data_qupath,2,min)
      metadata$maxRange <- apply(data_qupath,2,max)
      data_subset.ff <- new("flowFrame",exprs=as.matrix(data_qupath), parameters=AnnotatedDataFrame(metadata)) # in order to create a flow frame, data needs to be read as matrix by exprs
      head(data_subset.ff, 10)
      write.FCS(data_subset.ff, file)
    }
  )
  ## plots on tab 3
  ## plot 1
  output$graph1 <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    data_qupath <- fread(inFile$datapath, header = TRUE, check.names = FALSE)
    data_qupath <- data_qupath[,5:length(data_qupath)]
    ggplot(data = as.data.frame(data_qupath), aes(x =data_qupath$`Cell: Channel 3 mean`, y =data_qupath$`Cell: Channel 2 mean`)) + geom_point()
  })
  ## plot 2
  output$graph2 <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    data_qupath <- fread(inFile$datapath, header = TRUE, check.names = FALSE)
    data_qupath <- data_qupath[,5:length(data_qupath)]
    ggplot(data = as.data.frame(data_qupath), aes(x =data_qupath$`Cell: Channel 1 mean`, y =data_qupath$`Cell: Channel 2 mean`)) + geom_point()
  })
  ## Slider
  output$value <- renderPrint({ input$slider1 }) #slider handler
}

# Run the application 
shinyApp(ui = ui, server = server)

