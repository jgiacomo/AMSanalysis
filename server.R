library(shiny)
library(ggplot2)
library(dplyr)

# For this to work you must create a data frame called 'rundata' with the
# appropriate columns and column names. 'rundata' has to be an object in the R
# session before this Shiny app will work.

# be careful of any factors in rundata.
# use <<- as rundata is outside of this shinyApp() environment (see R scoping)
run.data <<- rundata

# get the 14C/13C error estimate from counting statistics
run.data$he14.13.error <- run.data$he14.13/sqrt(run.data$count14C)

# Define server logic required to draw a scatterplot of run data
shinyServer(function(input, output, session) {
    
    # instantiate a reactive values object to hold run data
    vals <- reactiveValues()
    
    # get run data for the selected sample (pd)
    observeEvent(input$samplePicker,{
        vals$pd <- run.data %>% filter(label==input$samplePicker) %>%
                   dplyr::select(label,smType,run,he14.13,he14.13.error,active)
    })
    
    # toggle points that are clicked
    observeEvent(input$runPlot_click, {
        res <- nearPoints(vals$pd, input$runPlot_click,
                          threshold=10, allRows=TRUE)
        
        vals$pd$active <- xor(vals$pd$active, res$selected_)
        
        # write active values back to run.data
        run.data[run.data$label==vals$pd$label,]$active <<- vals$pd$active
    })
    
    # toggle points that are brushed
    observeEvent(input$runPlot_brush,{
        res <- brushedPoints(vals$pd, input$runPlot_brush, allRows=TRUE)
        vals$pd$active <- xor(vals$pd$active, res$selected_)
        run.data[run.data$label==vals$pd$label,]$active <<- vals$pd$active
    })
    
    # move to the previous sample in the select box
    observeEvent(input$back,{
        labels <- unique(run.data$label)
        currentRow <- which(labels==input$samplePicker)
        if(currentRow>1){
            newLabel <- labels[currentRow - 1]
            updateSelectInput(session=session, inputId="samplePicker",
                              selected = newLabel)
        }
    })
    
    # move to next sample in select box
    observeEvent(input$forward,{
        labels <- unique(run.data$label)
        currentRow <- which(labels==input$samplePicker)
        if(currentRow<length(labels)){
            newLabel <- labels[currentRow + 1]
            updateSelectInput(session=session, inputId="samplePicker",
                              selected = newLabel)
        }
    })
    
    # reset all points
    observeEvent(input$exclude_reset, {
        vals$pd$active <- TRUE
        
        # write active values back to run.data
        run.data[run.data$label==vals$pd$label,]$active <<- vals$pd$active
    })
    
    
    
    output$runPlot <- renderPlot({
        
        # Plot the run 14C/12C (d13C normalized) data for the chosen sample
        
        # plot kept and excluded points as two separate data sets
        keep    <- vals$pd[ vals$pd$active, , drop=FALSE]
        exclude <- vals$pd[!vals$pd$active, , drop=FALSE]
        
        # set plot parameters
        mnY <- mean(vals$pd$he14.13)
        sdY <- sd(vals$pd$he14.13)
        maxY <- max(mnY+4*sdY, max(vals$pd$he14.13)*1.02)
        minY <- min(mnY-4*sdY, min(vals$pd$he14.13)*0.98)
        pTitle <- paste(vals$pd[1,]$label, vals$pd[1,]$smType, sep=" - ")
        
        ggplot(keep, aes(x=run,y=he14.13)) + geom_point(size=3) +
            geom_errorbar(
                aes(ymin=he14.13-he14.13.error,
                    ymax=he14.13+he14.13.error),
                width=0.25
            ) +
            ggtitle(pTitle) +
            geom_hline(aes(yintercept = mean(he14.13))) +
            geom_hline(aes(yintercept = mean(he14.13)+2*sd(he14.13)),
                       color = "red", linetype=2) +
            geom_text(aes(0,mean(he14.13)+2*sd(he14.13),
                          label="+2*sigma",vjust=-1,hjust=-0.5),
                      parse=TRUE,color="red") +
            geom_hline(aes(yintercept = mean(he14.13)-2*sd(he14.13)),
                       color = "red", linetype=2) +
            geom_text(aes(0,mean(he14.13)-2*sd(he14.13),
                          label="-2*sigma",vjust=1.5,hjust=-0.5),
                      parse=TRUE,color="red") +
            geom_point(data=exclude, shape=21, size=3,
                       fill=NA, color="black", alpha=0.25) +
            coord_cartesian(ylim = c(minY,maxY))
        
    })
    
    output$stats <- renderPrint({
        
        y <- vals$pd[vals$pd$active,]$he14.13
        my_mean <- format(mean(y), digits = 4)
        my_sd <- format(sd(y), digits = 2)
        cat("Mean =", my_mean, "    Standard Deviation =", my_sd, "\n")
    })
    
    output$runTable <- renderDataTable({
        
        data <- run.data %>%
            filter(label == input$samplePicker) %>%
            dplyr::mutate(he12C.uA = he12C*1e6, he13C.nA = he13C*1e9) %>%
            dplyr::select("Run"=run, "14C/13C"=he14.13,
                          "error"=he14.13.error, "13C/12C"=he13.12,
                          "12C [uA]"=he12C.uA, "13C [nA]"=he13C.nA,
                          "Transmission [%]"=trans12C
            )
        data
    })
})