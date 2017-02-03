# Module Server function
#
# Server logic for creating an interactive plot of rundata for deactivating or
# activating runs. This module will return a data frame with the runs and there
# active status.

library(shiny)
library(dplyr)
library(ggplot2)

runPlot <- function(input, output, session, positions, sample, Yval){
    # positions is for checking that a runlog file has been loaded.
    # sample is the position of the sample to plot.
    # Yval is the data value to plot on the Y axis (14C/13C or 13C/12C).
    
    # Create a reactive value for storing data.
    vals <- reactiveValues()
    
    # Get the data from the rundata global object
    vals$pd <- rundata %>% filter(pos == sample)
    
    # toggle points that are clicked
    observeEvent(input$runPlot_click, {
        res <- nearPoints(vals$pd, input$runPlot_click,
                          threshold=10, allRows=TRUE)
        
        vals$pd$active <- xor(vals$pd$active, res$selected_)
        
        # write active values back to rundata
        # rundata[rundata$pos==vals$pd$pos[1],]$active <<- vals$pd$active
    })
    
    # toggle points that are brushed
    observeEvent(input$runPlot_brush,{
        res <- brushedPoints(vals$pd, input$runPlot_brush, allRows=TRUE)
        vals$pd$active <- xor(vals$pd$active, res$selected_)
        # rundata[rundata$pos==vals$pd$pos[1],]$active <<- vals$pd$active
    })
    
    # reset all points
    observeEvent(input$exclude_reset, {
        vals$pd$active <- TRUE
        
        # write active values back to run.data
        # rundata[rundata$pos==vals$pd$pos[1],]$active <<- vals$pd$active
    })
    
    # deactivate all runs
    observeEvent(input$exclude_all, {
        vals$pd$active <- FALSE
        
        # write active values back to rundata
        # rundata[rundata$pos==vals$pd$pos[1],]$active <<- vals$pd$active
    })
    
    # move to the previous sample in the select box
    observeEvent(input$back,{
        positions <- unique(rundata$pos)
        currentRow <- which(positions==input$samplePicker)
        if(currentRow>1){
            newPosition <- positions[currentRow - 1]
            updateSelectInput(session=session, inputId="samplePicker",
                              selected = newPosition)
        }
    })
    
    # move to next sample in select box
    observeEvent(input$forward,{
        positions <- unique(rundata$pos)
        currentRow <- which(positions==input$samplePicker)
        if(currentRow<length(positions)){
            newPosition <- positions[currentRow + 1]
            updateSelectInput(session=session, inputId="samplePicker",
                              selected = newPosition)
        }
    })
    
    
    output$runPlot <- renderPlot({
        # Plot the run 14C/12C (d13C normalized) data for the chosen sample
        
        # Check if the runlog has been loaded.
        validate(
            need(positions(), "Please choose a runlog file.")
        )
        
        # plot kept and excluded points as two separate data sets
        keep    <- vals$pd[ vals$pd$active, , drop=FALSE]
        exclude <- vals$pd[!vals$pd$active, , drop=FALSE]
        
        # Check if all runs have been deactivated.
        validate(need(nrow(keep)!=0,"All runs deactivated for this sample"))
        
        # set plot parameters
        mnAllY <- mean(vals$pd$he14.13)
        sdAllY <- sd(vals$pd$he14.13)
        mnKeepY <- mean(keep$he14.13)
        sdKeepY <- sd(keep$he14.13)
        if(nrow(keep)==1){
            mnY <- mnKeepY
            sdY <- mnKeepY*0.05
        } else if(sdKeepY<sdAllY/2){
            mnY <- mnKeepY
            sdY <- sdKeepY
        } else {
            mnY <- mnAllY
            sdY <- sdAllY
        }
        maxY <- max(mnY+4*sdY, max(keep$he14.13)*1.02)
        minY <- min(mnY-4*sdY, min(keep$he14.13)*0.98)
        pTitle <- sprintf("Pos: %s - %s | Mean: %0.4e,  SD: %0.2e",
                          vals$pd[1,]$pos, vals$pd[1,]$smType,
                          mnKeepY, sdKeepY)
        
        ggplot(keep, aes(x=run,y=he14.13)) + geom_point(size=3) +
            geom_errorbar(
                aes(ymin=he14.13-he14.13.error,
                    ymax=he14.13+he14.13.error),
                width=0.25
            ) +
            ggtitle(pTitle) +
            scale_y_continuous(name="14C/13C") +
            geom_hline(aes(yintercept = mean(he14.13))) +
            geom_hline(aes(yintercept = mean(he14.13)+2*sd(he14.13)),
                       color = "blue", linetype=2) +
            geom_text(aes(0.75,mean(he14.13)+2*sd(he14.13),
                          label="+2*sigma",vjust=-1,hjust=-0.5),
                      parse=TRUE,color="blue") +
            geom_hline(aes(yintercept = mean(he14.13)-2*sd(he14.13)),
                       color = "blue", linetype=2) +
            geom_text(aes(0.75,mean(he14.13)-2*sd(he14.13),
                          label="-2*sigma",vjust=1.5,hjust=-0.5),
                      parse=TRUE,color="blue") +
            geom_point(data=exclude, shape=21, size=3,
                       fill=NA, color="red", alpha=0.75) +
            coord_cartesian(ylim = c(minY,maxY)) +
            scale_x_discrete(name="Run",
                             breaks=vals$pd$run,
                             label=c(1:nrow(vals$pd)))
        
    })
    
    data <- reactive(vals$pd %>% select(run, active))
    
    return(data)
}