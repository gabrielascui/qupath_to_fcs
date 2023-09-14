## new download
downloadCSV_UI <- function(id) {
  ns <- NS(id)
  tagList(
    downloadButton(ns("data_download"), label = "Download CSV")
  )
}

downloadCSV_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      # The selected file, if any
      userFile <- reactive({
        # If no file is selected, don't do anything
        validate(need(input$file, message = FALSE))
        input$file
      })
      
      # The user's data, parsed into a data frame
      dataframe <- reactive({
        read.csv(userFile()$datapath,
                 header = input$heading,
                 quote = input$quote,
                 stringsAsFactors = stringsAsFactors)
      })
      
      # We can run observers in here if we want to
      observe({
        msg <- sprintf("File %s was uploaded", userFile()$name)
        cat(msg, "\n")
      })
      
      # Return the reactive that yields the data frame
      return(dataframe)
    }
  )
}

#
# how to put this in the app ?
# where is the download handlers???
#
#


