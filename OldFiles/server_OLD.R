library(shiny)
library(ggplot2)
library(dplyr)
source("Functions/helperFunctions.R")

# watch what you put in render functions so you don't re-render things that
# that don't need to be (keeps your shiny app efficient and fast).

# Define server logic required to draw a scatterplot of run data
shinyServer(function(input, output) {
  
  output$RUNlist <- renderUI({
    chkBoxList <- as.character(
                    run.data$RUN[run.data$LABEL==input$samplePicker]
                  )
    activeRuns <- filter(run.data, RUN %in% chkBoxList & active) %>%
                    dplyr::select(RUN)
    
    checkboxGroupInput("chkBoxGrp", "Run List",
                         chkBoxList,
                         selected = activeRuns$RUN  # Selects all, need to select only active runs.
                      )
  })
  
  output$runPlot <- renderPlot({

    # Plot the run 14C/12C (d13C normalized) data for the chosen sample
    
    # Collect the data needed to draw the plot
    plot.data <- run.data %>%
                  filter(LABEL==input$samplePicker) %>%
                  dplyr::select(LABEL, RUN, d25C14toC12, errd25C14toC12)
    
    plot.data$RUN <- as.character(plot.data$RUN)  # convert from factor
    stat.y <- filter(plot.data, RUN %in% input$chkBoxGrp)$d25C14toC12
    my_mean <- mean(stat.y)
    my_sd <- sd(stat.y)
    
    # generate plot with helper function
    plotRuns(input$samplePicker, plot.data$RUN, plot.data$d25C14toC12,
             plot.data$errd25C14toC12, my_mean, my_sd)
    
    # deactivated run plot coordinates
    deact.x <- which(!(plot.data$RUN %in% input$chkBoxGrp))
    deact.y <- filter(plot.data, !(RUN %in% input$chkBoxGrp))$d25C14toC12
    
    # cross out deactivated run points in the plot
    points(deact.x, deact.y, pch=4, cex=2, col="red")
    
    # set run.data$active to FALSE for all runs of the selected sample
    # then set active to TRUE for all runs selected.
    # use <<- operator because run.data is outside of the local environment
    run.data[run.data$LABEL==input$samplePicker,]$active <<- FALSE
    # and set run.data$active to TRUE for non-deactivated runs
    run.data[run.data$RUN %in% input$chkBoxGrp, ]$active <<- TRUE
  })
  
  output$stats <- renderPrint({
    
    my_data <- run.data[run.data$LABEL==input$samplePicker,]
    y <- my_data[my_data$RUN %in% input$chkBoxGrp,]$d25C14toC12
    my_mean <- format(mean(y), digits = 4)
    my_sd <- format(sd(y), digits = 2)
    cat("Mean =", my_mean, "\nStandard Deviation =", my_sd, "\n")
  })
  
  output$runTable <- renderDataTable({
    
    data <- run.data %>%
              filter(LABEL == input$samplePicker) %>%
              dplyr::select(Run=RUN, "14C/12C"=d25C14toC12,
                            error=errd25C14toC12, "13C/12C"=normC13toC12,
                            "12C_uA"=C12, "13C_uA"=C13, trans=transmission
                            )
    data
  })
})