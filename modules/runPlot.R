# Module Server function
#
# Server logic for creating an interactive plot of rundata for deactivating or
# activating runs. This module will return a data frame with the runs and there
# active status.

library(shiny)
library(dplyr)
library(ggplot2)

runPlot <- function(input, output, session, rundf, samPos, Yval){
    # rundf <data frame, reactive> is the rundata.
    # samPos <reactive> is the position of the sample to plot.
    # Yval <char> is the data value to plot on the Y axis (14C/13C or 13C/12C).
    
    # Create a reactive value for storing data.
    vals <- reactiveValues()
    
    # Check if the runlog has been loaded. Then get values.
    observe({
        validate(
            need(rundf(), "There is no rundata.")
        )
        
        # Get just the data for the input sample.
        position <- samPos()
        df <- rundf()
        vals$pd <- filter(df, pos == position)
        
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
        
    })
    
    output$runPlot <- renderPlot({
        # Plot the run 14C/12C (d13C normalized) data for the chosen sample
        
        # Check first if there is rundata to plot.
        validate(
            need(rundf(), "There is no rundata.")
        )
        
        # plot kept and excluded points as two separate data sets
        keep    <- vals$pd[ vals$pd$active, , drop=FALSE]
        exclude <- vals$pd[!vals$pd$active, , drop=FALSE]
        
        # Rename the Y variable in the data frame
        if(Yval=="C14"){
            names(keep)[names(keep)=="he14.13"] <- "y"
            names(keep)[names(keep)=="he14.13.error"] <- "y.error"
            names(exclude)[names(exclude)=="he14.13"] <- "y"
            Ytitle <- "14C/13C"
        } else if(Yval=="C13"){
            names(keep)[names(keep)=="he13.12"] <- "y"
            keep$y.error <- rep(0, nrow(keep))
            names(exclude)[names(exclude)=="he13.12"] <- "y"
            Ytitle <- "13C/12C"
        } else { stop("Error in runPlot.R, you chose a bad Yval.")}
        
        
        # Check if all runs have been deactivated.
        validate(need(nrow(keep)!=0,"All runs deactivated for this sample"))
        
        # set plot parameters
        mnAllY <- mean(rbind(keep,exclude)$y)
        sdAllY <- sd(rbind(keep,exclude)$y)
        mnKeepY <- mean(keep$y)
        sdKeepY <- sd(keep$y)
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
        maxY <- max(mnY+4*sdY, max(keep$y)*1.02)
        minY <- min(mnY-4*sdY, min(keep$y)*0.98)
        pTitle <- sprintf("Pos: %s - %s | Mean: %0.4e,  SD: %0.2e",
                          vals$pd[1,]$pos, vals$pd[1,]$smType,
                          mnKeepY, sdKeepY)
        
        ggplot(keep, aes(x=run,y=y)) + geom_point(size=3) +
            geom_errorbar(
                aes(ymin=y-y.error,
                    ymax=y+y.error),
                width=0.25
            ) +
            ggtitle(pTitle) +
            scale_y_continuous(name=Ytitle) +
            geom_hline(aes(yintercept = mean(y))) +
            geom_hline(aes(yintercept = mean(y)+2*sd(y)),
                       color = "blue", linetype=2) +
            geom_text(aes(0.75,mean(y)+2*sd(y),
                          label="+2*sigma",vjust=-1,hjust=-0.5),
                      parse=TRUE,color="blue") +
            geom_hline(aes(yintercept = mean(y)-2*sd(y)),
                       color = "blue", linetype=2) +
            geom_text(aes(0.75,mean(y)-2*sd(y),
                          label="-2*sigma",vjust=1.5,hjust=-0.5),
                      parse=TRUE,color="blue") +
            geom_point(data=exclude, shape=21, size=3,
                       fill=NA, color="red", alpha=0.75) +
            coord_cartesian(ylim = c(minY,maxY)) +
            scale_x_discrete(name="Run",
                             breaks=vals$pd$run,
                             label=c(1:nrow(vals$pd)))
        
    })
    
    # Good practice to make sure returned results are reactive.
    finaldf <- reactive({vals$pd})
    
    return(finaldf)
}