library(shiny)
library(DT)
library(ggplot2)
library(dplyr)

# Define the server logic of our shiny app.
shinyServer(function(input, output, session) {
    
    myRD <- callModule(runlogFile,"runlog")
    stdrundata <- callModule(standards,"standards",rundf=myRD)
    
    observe({
        validate(
            need(myRD(), "No positions")
        )
        
        # Update the sample selection now that we have rundata.
        samplePos <- unique(myRD()$pos)
        updateSelectInput(session, inputId="samplePicker",
                          label="Sample",
                          choices=as.character(samplePos))
    })
    
    # Create plot for cleaning 14C/13C runs.
    # inputs passed to modules need to be wrapped in reactive.
    samplePicker <- reactive({input$samplePicker})
    selectedRuns <- callModule(runPlot,"C14",
                             rundf=myRD,
                             samPos=samplePicker,
                             Yval="he14.13",
                             Yval.error="he14.13.error",
                             Ytitle="14C/13C")
    
    # Create plot for cleaning 13C/12C runs.
    selectedD13Cruns <- callModule(runPlot, "C13",
                                   rundf=myRD,
                                   samPos=samplePicker,
                                   Yval="he13.12")
    
    # move to the previous sample in the select box
    observeEvent(input$back,{
        smPos <- unique(myRD()$pos)
        currentRow <- which(smPos==input$samplePicker)
        if(currentRow>1){
            newPosition <- smPos[currentRow - 1]
            updateSelectInput(session=session, inputId="samplePicker",
                              selected = newPosition)
        }
    })
    
    # move to next sample in select box
    observeEvent(input$forward,{
        smPos <- unique(myRD()$pos)
        currentRow <- which(smPos==input$samplePicker)
        if(currentRow<length(smPos)){
            newPosition <- smPos[currentRow + 1]
            updateSelectInput(session=session, inputId="samplePicker",
                              selected = newPosition)
        }
    })
    
    observe({
        validate(
            need(myRD(), "No positions")
        )
        activeData <- selectedRuns()
        rundata[rundata$run %in% activeData$run,]$active <<-
            as.logical(activeData$active)
    })
    
    output$sampleMetaPlot <- renderPlot({
        # Plot the various currents and transmission for the last clicked on run.
        
        # Check if the runlog has been loaded.
        validate(
            need(myRD(), "Please choose a runlog file.")
        )
        
        # Get the run data for the sample and normalize the currents and
        # transmission
        rundf <- myRD()
        
        rundf <- rundf %>% filter(pos==input$samplePicker) %>%
            mutate(norm.he12C=he12C/max(he12C),
                   norm.he13C=he13C/max(he13C),
                   norm.trans12C=trans12C/max(trans12C),
                   norm.he13.12=he13.12/max(he13.12))
        
        C12 <- rundf %>% select(run,norm=norm.he12C) %>% mutate(meta="C12")
        C13 <- rundf %>% select(run,norm=norm.he13C) %>% mutate(meta="C13")
        trans <- rundf %>% select(run,norm=norm.trans12C) %>%
            mutate(meta="trans12C")
        stableIR <- rundf %>% select(run,norm=norm.he13.12) %>%
            mutate(meta="he13.12")
        plotdata <- rbind(C12,C13,trans,stableIR)
        plotdata$meta <- factor(plotdata$meta)
        
        p <- ggplot(plotdata, aes(x=run,y=norm,group=meta,color=meta)) +
            geom_line() +
            scale_color_discrete(name="",
                                 breaks=c("C12","C13","trans12C","he13.12"),
                                 labels=c("he 12C","he 13C","12C Trans","13C/12C")) +
            scale_x_discrete(name="Run",breaks=rundf$run,
                             labels=c(1:nrow(rundf))) +
            scale_y_continuous(name="Normalized Data")
        p
    })
    
    output$runTable <- DT::renderDataTable({
        # Check if the runlog has been loaded.
        validate(
            need(myRD(), "Please choose a runlog file.")
        )
        data <- myRD()
        data <- data %>% filter(pos==input$samplePicker) %>%
            dplyr::mutate(he12C.uA = he12C*1e6, he13C.nA = he13C*1e9) %>%
            dplyr::select("Run"=run, "14C/13C"=he14.13,
                          "error"=he14.13.error, "13C/12C"=he13.12,
                          "12C [uA]"=he12C.uA, "13C [nA]"=he13C.nA,
                          "Transmission [%]"=trans12C
            )
        data
    }, options=list(pageLength=25))
})