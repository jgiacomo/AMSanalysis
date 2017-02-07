# Module Server function
#
# Server logic for getting and cleaning the standards data.

library(shiny)
library(dplyr)

standards <- function(input, output, session, rundf){
    # rundf <reactive> the rundata.
    
    observe({validate(need(rundf(),"Please load data first."))})
    
    # Update the sample type checkboxes.
    observe({
        smplTypes <- unique(rundf()$smType)
        updateCheckboxGroupInput(session,"stdPicks",
                                 choices=sort(smplTypes),
                                 inline=TRUE,
                                 selected=if("OX2" %in% smplTypes){
                                     "OX2"
                                 })
    })
    
    # Get the standards from the rundata
    df <- reactive({
        # Get the standard positions.
        standardPos <- numInputToIntegers(input$stdPos)
        
        # Use the checkboxes if no positions are entered.
        if(standardPos==""|standardPos=="all"){
            standardPos <- unique(rundf()[rundf()$smType %in% input$stdPicks,]$pos)
        }
        
        # Filter the data to only the standards
        filter(rundf(), pos %in% standardPos)
    })
    
    output$stdTable <- renderTable({
        unique(select(df(), pos, smType, label))
    })

    return(reactive({df}))
}