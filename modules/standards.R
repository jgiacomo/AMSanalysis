# Module Server function
#
# Server logic for getting and cleaning the standards data.

library(shiny)
library(dplyr)

standards <- function(input, output, session, positions){
    
    # Clean the standards positions input to a numeric vector.
    vals <- reactiveValues()
    
    observeEvent(input$stdPos,{
        
        # Check for rundata.
        # if(!exists("rundata")){ return(NULL)}
        validate(
            need(positions(),"")
        )
        
        # Get the standard positions.
        standardPos <- numInputToIntegers(input$stdPos)
        if(standardPos==""|standardPos=="all"){
            standardPos <- unique(rundata[rundata$smType=="OX2",]$pos)
        }
        
        print(standardPos)
        vals$stdData <- rundata %>% filter(pos %in% standardPos)
    })
    
    

    return(NULL)
}