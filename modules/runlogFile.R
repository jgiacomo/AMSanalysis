# Module Server function
#
# Server logic for getting the runlog file for AMS data.

library(shiny)
library(DT)
library(dplyr)

runlogFile <- function(input, output, session){
    
    # Turn the result.xls file chooser on/off
    observeEvent(input$resultYN,
                 if(input$resultYN=="Yes"){
                     shinyjs::show(id="resultChooser", anim=TRUE)
                 } else shinyjs::hide(id="resultChooser", anim=TRUE))
    
    observe({
        validate(
            need(input$runlog, "Please choose a runlog file.")
        )
        runlog <- input$runlog$datapath
        result <- input$result$datapath
        df <- NECtoRunData(runlog, result)
        df$he14.12.error <- df$he14.12/sqrt(df$count14C)
        df$he14.13.error <- df$he14.13/sqrt(df$count14C)
        rundata <<- df  # <<- because rundata is outside the server function
    })
    
    # Show run data from choosen runlog.
    output$runDT <- DT::renderDataTable({
        # Check if the runlog has been loaded.
        validate(
            need(input$runlog, "Please choose a runlog file.")
        )
        
        data <- rundata %>%
            group_by(pos, label, smType) %>%
            summarize(runs=n(),he13C=round(mean(he13C)*1e9,1))
        
        data
    }, options=list(pageLength=15), rownames=FALSE)
    
    # Return the rundata from the runlog.
    runD <- reactive({
        validate(
            need(input$runlog, "Please choose a runlog file.")
        )
        rundata
    })
    
    return(runD)
}