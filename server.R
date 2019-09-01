library(shiny)
library(DT)
library(ggplot2)
library(dplyr)

source("helperFunctions/NECtoRunData.R")
source("helperFunctions/KIRAMStoRunData.R")
source("helperFunctions/numInputToIntegers.R")

# Define the server logic of our shiny app.
shinyServer(function(input, output, session) {
    
    # Turn the result.xls file chooser on/off
    observeEvent(input$resultYN,
                 if(input$resultYN=="Yes"){
                     shinyjs::show(id="resultChooser", anim=TRUE)
                 } else shinyjs::hide(id="resultChooser", anim=TRUE))
    
    # Get run data from input files
    # rundata <- reactive({
    #     # Run once a runlog file is chosen
    #     validate(
    #         need(input$runlog, "Please choose a runlog")
    #     )
    #     runlog <- input$runlog$datapath
    #     result <- input$result$datapath
    #     df <- NECtoRunData(runlog, result)
    #     df$he14.12.error <- df$he14.12/sqrt(df$count14C)
    #     df$he14.13.error <- df$he14.13/sqrt(df$count14C)
    #     return(df)
    # })
    observe({
        validate(
            need(input$runlog, "Please choose a runlog file.")
        )
        runlog <- input$runlog$datapath
        result <- input$result$datapath
        if(input$labPick=="KIRAMS"){
            df <- KIRAMStoRunData(runlog, result)
        }
        if(input$labPick=="DirectAMS"){
            df <- NECtoRunData(runlog, result)
        }
        df$he14.12.error <- df$he14.12/sqrt(df$count14C)
        df$he14.13.error <- df$he14.13/sqrt(df$count14C)
        rundata <<- df  # <<- because rundata is outside the server function
        
        # Update the sample selection now that we have rundata.
        updateSelectInput(session, inputId="samplePicker",
                          label="Sample",
                          choices=as.character(unique(rundata$pos)))
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
    
    # instantiate a reactive values object to hold a sample's run data.
    vals <- reactiveValues()
    
    # get run data for the selected sample (pd, for plot data)
    observeEvent(input$samplePicker,{
        validate(
            need(input$runlog, "Please choose a runlog file.")
        )
        
        vals$pd <- rundata %>% filter(pos==input$samplePicker) %>%
                   dplyr::select(pos,label,smType,run,he14.13,
                                 he14.13.error,active)
    })
    
    # toggle points that are clicked
    observeEvent(input$runPlot_click, {
        res <- nearPoints(vals$pd, input$runPlot_click,
                          threshold=10, allRows=TRUE)
        
        vals$pd$active <- xor(vals$pd$active, res$selected_)
        
        # write active values back to rundata
        rundata[rundata$pos==vals$pd$pos[1],]$active <<- vals$pd$active
    })
    
    # toggle points that are brushed
    observeEvent(input$runPlot_brush,{
        res <- brushedPoints(vals$pd, input$runPlot_brush, allRows=TRUE)
        vals$pd$active <- xor(vals$pd$active, res$selected_)
        rundata[rundata$pos==vals$pd$pos[1],]$active <<- vals$pd$active
    })
    
    # reset all points
    observeEvent(input$exclude_reset, {
        vals$pd$active <- TRUE
        
        # write active values back to run.data
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
    
    
    output$runPlot <- renderPlot({
        # Plot the run 14C/12C (d13C normalized) data for the chosen sample
        
        # Check if the runlog has been loaded.
        validate(
            need(input$runlog, "Please choose a runlog file.")
        )
        
        # plot kept and excluded points as two separate data sets
        keep    <- vals$pd[ vals$pd$active, , drop=FALSE]
        exclude <- vals$pd[!vals$pd$active, , drop=FALSE]
        
        # set plot parameters
        mnY <- mean(vals$pd$he14.13)
        sdY <- sd(vals$pd$he14.13)
        maxY <- max(mnY+4*sdY, max(vals$pd$he14.13)*1.02)
        minY <- min(mnY-4*sdY, min(vals$pd$he14.13)*0.98)
        pTitle <- paste("Pos: ", vals$pd[1,]$pos,
                        " - ", vals$pd[1,]$smType,
                        "      Label: ", vals$pd[1,]$label, sep="")
        
        ggplot(keep, aes(x=run,y=he14.13)) + geom_point(size=3) +
            geom_errorbar(
                aes(ymin=he14.13-he14.13.error,
                    ymax=he14.13+he14.13.error),
                width=0.25
            ) +
            ggtitle(pTitle) +
            geom_hline(aes(yintercept = mean(he14.13))) +
            geom_hline(aes(yintercept = mean(he14.13)+2*sd(he14.13)),
                       color = "blue", linetype=2) +
            geom_text(aes(0,mean(he14.13)+2*sd(he14.13),
                          label="+2*sigma",vjust=-1,hjust=-0.5),
                      parse=TRUE,color="blue") +
            geom_hline(aes(yintercept = mean(he14.13)-2*sd(he14.13)),
                       color = "blue", linetype=2) +
            geom_text(aes(0,mean(he14.13)-2*sd(he14.13),
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
            need(input$runlog, "Please choose a runlog file.")
        )
        
        y <- vals$pd[vals$pd$active,]$he14.13
        my_mean <- format(mean(y), digits = 4)
        my_sd <- format(sd(y), digits = 2)
        cat("Mean =", my_mean, "    Standard Deviation =", my_sd, "\n")
    })
    
    output$runTable <- DT::renderDataTable({
        # Check if the runlog has been loaded.
        validate(
            need(input$runlog, "Please choose a runlog file.")
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