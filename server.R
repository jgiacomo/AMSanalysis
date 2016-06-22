library(shiny)
library(ggplot2)
library(dplyr)

# source script for getting run data
source("RadiocarbonDating_a1-machine_and_process-blank_dynaC13correction_FOR_SHINYAPP_newRawdata.r")

# watch what you put in render functions so you don't re-render things that
# that don't need to be (keeps your shiny app efficient and fast).

# get rid of factors from run.data
# use <<- as run.data is outside of this shinyApp() environment (see R scoping)
run.data$LABEL    <<- as.character(run.data$LABEL)
run.data$RUN      <<- as.character(run.data$RUN)
run.data$samptype <<- as.character(run.data$samptype)

# Define server logic required to draw a scatterplot of run data
shinyServer(function(input, output) {
    
    # instantiate a reactive values object to hold run data
    vals <- reactiveValues()
    
    # get run data the selected sample (pd)
    observeEvent(input$samplePicker,{
        vals$pd <- run.data %>% filter(LABEL==input$samplePicker) %>%
                   dplyr::select(LABEL,RUN,d25C14toC12,errd25C14toC12,active)
    })
    
    # toggle points that are clicked
    observeEvent(input$runPlot_click, {
        res <- nearPoints(vals$pd, input$runPlot_click, allRows=TRUE)
        
        vals$pd$active <- xor(vals$pd$active, res$selected_)
        
        # write active values back to run.data
        run.data[run.data$LABEL==vals$pd$LABEL,]$active <<- vals$pd$active
    })
    
    # reset all points
    observeEvent(input$exclude_reset, {
        vals$pd$active <- TRUE
        
        # write active values back to run.data
        run.data[run.data$LABEL==vals$pd$LABEL,]$active <<- vals$pd$active
    })
    
    
    
    output$runPlot <- renderPlot({
        
        # Plot the run 14C/12C (d13C normalized) data for the chosen sample
        
        # plot kept and excluded points as two separate data sets
        keep    <- vals$pd[ vals$pd$active, , drop=FALSE]
        exclude <- vals$pd[!vals$pd$active, , drop=FALSE]
        
        # set plot parameters
        mnY <- mean(vals$pd$d25C14toC12)
        sdY <- sd(vals$pd$d25C14toC12)
        maxY <- max(mnY+4*sdY, max(vals$pd$d25C14toC12)*1.02)
        minY <- min(mnY-4*sdY, min(vals$pd$d25C14toC12)*0.98)
        
        ggplot(keep, aes(x=RUN,y=d25C14toC12)) + geom_point(size=3) +
            geom_errorbar(
                aes(ymin=d25C14toC12-errd25C14toC12,
                    ymax=d25C14toC12+errd25C14toC12),
                width=0.25
            ) +
            ggtitle(isolate(input$samplePicker)) +
            geom_hline(aes(yintercept = mean(d25C14toC12))) +
            geom_hline(aes(
                yintercept = mean(d25C14toC12)+2*sd(d25C14toC12)),
                color = "red", linetype=2) +
            geom_hline(aes(
                yintercept = mean(d25C14toC12)-2*sd(d25C14toC12)),
                color = "red", linetype=2) +
            geom_point(data=exclude, shape=21, size=3,
                       fill=NA, color="black", alpha=0.25) +
            coord_cartesian(ylim = c(minY,maxY))
        
    })
    
    output$stats <- renderPrint({
        
#         my_data <- run.data[run.data$LABEL==input$samplePicker,]
#         y <- my_data[my_data$active,]$d25C14toC12
        y <- vals$pd[vals$pd$active,]$d25C14toC12
        my_mean <- format(mean(y), digits = 4)
        my_sd <- format(sd(y), digits = 2)
        cat("Mean =", my_mean, "\nStandard Deviation =", my_sd, "\n")
    })
    
    output$runTable <- renderDataTable({
        
        data <- run.data %>%
            filter(LABEL == input$samplePicker) %>%
            dplyr::select("Run"=RUN, "14C/12C"=d25C14toC12,
                          "error"=errd25C14toC12, "13C/12C"=normC13toC12,
                          "12C_uA"=C12, "13C_uA"=C13, "trans"=transmission
            )
        data
    })
})