library(shiny)
library(DT)
library(ggplot2)
library(dplyr)

# Define the server logic of our shiny app.
shinyServer(function(input, output, session) {
    
    positions <- callModule(runlogFile,"runlog")
    callModule(standards,"standards",positions)
    
    observe({
        validate(
            need(positions(), "No positions")
        )
        # Update the sample selection now that we have rundata.
        updateSelectInput(session, inputId="samplePicker",
                          label="Sample",
                          choices=as.character(positions()))
    })

    # instantiate a reactive values object to hold a sample's run data.
    vals <- reactiveValues()
    
    # get run data for the selected sample (pd, for plot data)
    observeEvent(input$samplePicker,{
        validate(
            need(positions(), "Please choose a runlog file.")
        )
        
        vals$pd <- rundata %>% filter(pos==input$samplePicker)
    })
    
    # toggle points that are clicked
    observeEvent(input$C14Plot_click, {
        res <- nearPoints(vals$pd, input$C14Plot_click,
                          threshold=10, allRows=TRUE)
        
        vals$pd$active <- xor(vals$pd$active, res$selected_)
        
        # write active values back to rundata
        rundata[rundata$pos==vals$pd$pos[1],]$active <<- vals$pd$active
    })
    
    # toggle points that are brushed
    observeEvent(input$C14Plot_brush,{
        res <- brushedPoints(vals$pd, input$C14Plot_brush, allRows=TRUE)
        vals$pd$active <- xor(vals$pd$active, res$selected_)
        rundata[rundata$pos==vals$pd$pos[1],]$active <<- vals$pd$active
    })
    
    # reset all points
    observeEvent(input$exclude_reset, {
        vals$pd$active <- TRUE
        
        # write active values back to run.data
        rundata[rundata$pos==vals$pd$pos[1],]$active <<- vals$pd$active
    })
    
    # deactivate all runs
    observeEvent(input$exclude_all, {
        vals$pd$active <- FALSE
        
        # write active values back to rundata
        rundata[rundata$pos==vals$pd$pos[1],]$active <<- vals$pd$active
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
    
    
    output$C14Plot <- renderPlot({
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
    
    output$stats <- renderPrint({
        # Check if the runlog has been loaded.
        validate(
            need(positions(), "Please choose a runlog file.")
        )
        
        y <- vals$pd[vals$pd$active,]$he14.13
        my_mean <- format(mean(y), digits = 4)
        my_sd <- format(sd(y), digits = 2)
        cat("Mean =", my_mean, "    Standard Deviation =", my_sd, "\n")
    })
    
    output$sampleMetaPlot <- renderPlot({
        # Plot the various currents and transmission for the last clicked on run.
        
        # Check if the runlog has been loaded.
        validate(
            need(positions(), "Please choose a runlog file.")
        )
        
        # Get the run data for the sample and normalize the currents and transmission
        rundf <- vals$pd %>% mutate(norm.he12C=he12C/max(he12C),norm.he13C=he13C/max(he13C),
                                    norm.trans12C=trans12C/max(trans12C),
                                    norm.he13.12=he13.12/max(he13.12))
        C12 <- rundf %>% select(run,norm=norm.he12C) %>% mutate(meta="C12")
        C13 <- rundf %>% select(run,norm=norm.he13C) %>% mutate(meta="C13")
        trans <- rundf %>% select(run,norm=norm.trans12C) %>% mutate(meta="trans12C")
        stableIR <- rundf %>% select(run,norm=norm.he13.12) %>% mutate(meta="he13.12")
        plotdata <- rbind(C12,C13,trans,stableIR)
        plotdata$meta <- factor(plotdata$meta)
        
        p <- ggplot(plotdata, aes(x=run,y=norm,group=meta,color=meta)) + geom_line() +
            scale_color_discrete(name="",
                                 breaks=c("C12","C13","trans12C","he13.12"),
                                 labels=c("he 12C","he 13C","12C Trans","13C/12C")) +
            scale_x_discrete(name="Run",breaks=vals$pd$run,labels=c(1:nrow(vals$pd))) +
            scale_y_continuous(name="Normalized Data")
        p
    })
    
    output$runTable <- DT::renderDataTable({
        # Check if the runlog has been loaded.
        validate(
            need(positions(), "Please choose a runlog file.")
        )
        
        data <- rundata %>%
            filter(pos == input$samplePicker) %>%
            dplyr::mutate(he12C.uA = he12C*1e6, he13C.nA = he13C*1e9) %>%
            dplyr::select("Run"=run, "14C/13C"=he14.13,
                          "error"=he14.13.error, "13C/12C"=he13.12,
                          "12C [uA]"=he12C.uA, "13C [nA]"=he13C.nA,
                          "Transmission [%]"=trans12C
            )
        data
    }, options=list(pageLength=25))
})