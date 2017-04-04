# Module Server function
#
# Server logic for creating an interactive plot of rundata for deactivating or
# activating runs. This module will return a data frame with the runs and there
# active status.

library(shiny)
library(dplyr)
library(ggplot2)

runPlot <- function(input, output, session,
                    rundf, samPos, Yval, Yval.error=NULL, Ytitle=NULL){
    # rundf <data frame, reactive> is the rundata.
    # samPos <reactive> is the position of the sample to plot.
    # Yval <char> is the column name, in rundf, of the Y value to plot.
    # Yval.error <char> is the column name of the Y value errors to plot.
    # Ytitle <char> title to use on the Y axis of the plot.
    
    # Create a reactive value to hold data
    
    vals <- reactiveValues()
    
    # Simplify the input data to just the needed values.
    observe({
        rdf <- rundf()
        if(!is.null(Yval.error)){
            rdf <- rdf %>% filter(pos == samPos()) %>%
                select(pos,run,smType, Y=get(Yval), Y.error=get(Yval.error),
                       active) # %>%
                # mutate(active=TRUE)
        } else{
            rdf <- rdf %>% filter(pos == samPos()) %>%
                select(pos, run, smType, Y=get(Yval), active) %>%
                # mutate(Y.error=0, active=TRUE)
                mutate(Y.error=0)
        }
        vals$df <- rdf
    })
    
    # toggle points that are clicked
    observeEvent(input$runPlot_click, {
        res <- nearPoints(vals$df, input$runPlot_click,
                          threshold=10, allRows=TRUE)
        vals$df$active <- xor(vals$df$active, res$selected_)
    })
    
    # toggle points that are brushed
    observeEvent(input$runPlot_brush,{
        res <- brushedPoints(vals$df, input$runPlot_brush, allRows=TRUE)
        vals$df$active <- xor(vals$df$active, res$selected_)
    })
    
    # reactivate all points
    observeEvent(input$exclude_reset, {
        vals$df$active <- TRUE
    })
    
    # deactivate all runs
    observeEvent(input$exclude_all, {
        vals$df$active <- FALSE
    })
    
    output$runPlot <- renderPlot({
        # Plot the run 14C/12C (d13C normalized) data for the chosen sample
        
        # Check first if there is rundata to plot.
        validate(
            need(rundf(), "There is no rundata.")
        )
        
        # plot kept and excluded points as two separate data sets
        keep    <- vals$df[ vals$df$active, , drop=FALSE]
        exclude <- vals$df[!vals$df$active, , drop=FALSE]
        
        # Check if all runs have been deactivated.
        validate(need(nrow(keep)!=0,"All runs deactivated for this sample"))
        
        # set plot parameters
        mnAllY <- mean(vals$df$Y)
        sdAllY <- sd(vals$df$Y)
        mnKeepY <- mean(keep$Y)
        sdKeepY <- sd(keep$Y)
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
        maxY <- max(mnY+4*sdY, max(keep$Y)*1.02)
        minY <- min(mnY-4*sdY, min(keep$Y)*0.98)
        pTitle <- sprintf("Pos: %s - %s | Mean: %0.4e,  SD: %0.2e",
                          vals$df[1,]$pos, vals$df[1,]$smType,
                          mnKeepY, sdKeepY)
        
        ggplot(keep, aes(x=run,y=Y)) + geom_point(size=3) +
            geom_errorbar(
                aes(ymin=Y-Y.error,
                    ymax=Y+Y.error),
                width=0.25
            ) +
            ggtitle(pTitle) +
            scale_y_continuous(name=Ytitle) +
            geom_hline(aes(yintercept = mean(Y))) +
            geom_hline(aes(yintercept = mean(Y)+2*sd(Y)),
                       color = "blue", linetype=2) +
            geom_text(aes(0.75,mean(Y)+2*sd(Y),
                          label="+2*sigma",vjust=-1,hjust=-0.5),
                      parse=TRUE,color="blue") +
            geom_hline(aes(yintercept = mean(Y)-2*sd(Y)),
                       color = "blue", linetype=2) +
            geom_text(aes(0.75,mean(Y)-2*sd(Y),
                          label="-2*sigma",vjust=1.5,hjust=-0.5),
                      parse=TRUE,color="blue") +
            geom_point(data=exclude, shape=21, size=3,
                       fill=NA, color="red", alpha=0.75) +
            coord_cartesian(ylim = c(minY,maxY)) +
            scale_x_discrete(name="Run",
                             breaks=vals$df$run,
                             label=c(1:nrow(vals$df)))
        
    })
    
    # Good practice to make sure returned results are reactive.
    #finaldf <- reactive({vals$df})
    
    return(reactive({vals$df}))
}