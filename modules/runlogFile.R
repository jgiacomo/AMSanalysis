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
    
    rd.all <- eventReactive(c(input$runlog, input$result),{
        validate(
            need(input$runlog, "Please choose a runlog file.")
        )
        runlog <- input$runlog$datapath
        result <- input$result$datapath
        df <- NECtoRunData(runlog, result)  # function loaded in global.R
        df$he14.12.error <- df$he14.12/sqrt(df$count14C)
        df$he14.13.error <- df$he14.13/sqrt(df$count14C)
        df
    })
    # observe({
    #     validate(
    #         need(input$runlog, "Please choose a runlog file.")
    #     )
    #     runlog <- input$runlog$datapath
    #     result <- input$result$datapath
    #     df <- NECtoRunData(runlog, result)  # function loaded in global.R
    #     df$he14.12.error <- df$he14.12/sqrt(df$count14C)
    #     df$he14.13.error <- df$he14.13/sqrt(df$count14C)
    #     rundata <<- df  # <<- because rundata is outside the server function
    # })
    
    # Filter rundata based on selected sample positions
    rd.filter <- eventReactive(input$filterPosButton,{
        samplePos <- numInputToIntegers(input$runRows)  # loaded in global.R
        if(is.integer(samplePos)){
            filter(rd.all(), pos %in% samplePos)
        }
    }, ignoreNULL = FALSE)
    
    # Create the rundata data frame
    rd <- reactive({
        if(is.null(rd.filter())){
            return(rd.all())
        } else {
            return(rd.filter())
        }
    })
    
    # Show run data from choosen runlog.
    output$runDT <- DT::renderDataTable({
        # Check if the runlog has been loaded.
        validate(
            need(input$runlog, "Please choose a runlog file.")
        )
        
        data <- rd() %>%
            group_by(pos, label, smType) %>%
            summarize(runs=n(),he13C=round(mean(he13C)*1e9,1))
        
        data
    }, options=list(pageLength=15), rownames=FALSE)
    
    # Write the rundata to a global variable.
    # Need to rewrite program to remove this dependency.
    observe({
        rundata <<- rd()
    })
    
    # Return the rundata from the runlog.
    # runD <- reactive({
    #     validate(
    #         need(input$runlog, "Please choose a runlog file.")
    #     )
    #     rd()
    # })
    
    return(rd)
}