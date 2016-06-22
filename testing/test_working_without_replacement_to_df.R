library(shiny)
library(ggplot2)
library(dplyr)

df <- data.frame("sample"=c(rep("A",10),rep("B",10),rep("C",10)),
                 "RUN"=seq(from=1, to=30, by=1),
                 "Y" = rnorm(30), "active"=rep(TRUE,30),
                 stringsAsFactors=FALSE
)

ui <- fluidPage(
    sidebarPanel(
        selectInput("samplePicker", "Sample",
                    c("A","B","C")
        )
    ),
    
    mainPanel(
        plotOutput("runPlot", click = "runPlot_click"),
        actionButton("exclude_reset", "Reset")
    )
)

server <- function(input, output) {
    
    # intstantiate a reactive values object
    vals <- reactiveValues()
    
    # create plot data and a variable for keeping track of which rows we keep
    # for the plot as reactive values in our reactive values object. These are
    # created in an observe event so that they react whenever a new sample is
    # chosen by the user.
    observeEvent(input$samplePicker, {
        vals$plotdf <- df[df$sample==input$samplePicker,]
        vals$keeprows <- vals$plotdf$active
    })
    
    # on a click event update keeprows and the active field of the original data
    # frame.
    observeEvent(input$runPlot_click, {
        # get all rows of dataframe with a new logical column 'selected_'
        res <- nearPoints(vals$plotdf, input$runPlot_click, allRows=TRUE)
        # update keeprows from the clicked data
        vals$keeprows <- xor(vals$keeprows, res$selected_)
        # update 'active' field of original sample data to match keeprows
        vals$plotdf$active <<- vals$keeprows
    })
    
    # reset all keeprows to TRUE. This keeps all data
    observeEvent(input$exclude_reset, {
        vals$keeprows <- rep(TRUE, nrow(vals$plotdf))
        vals$plotdf$active <<- vals$keeprows
    })
    
    # render the plot with a kept data set and an excluded data set
    output$runPlot <- renderPlot({
        keep <- vals$plotdf[vals$keeprows, , drop=FALSE]
        exclude <- vals$plotdf[!vals$keeprows, , drop=FALSE]
        
        ggplot(keep, aes(x=RUN, y=Y)) +
            geom_point() +
            geom_point(data=exclude, shape=21, fill=NA, color="black",
                       alpha=0.25) +
            coord_cartesian(ylim = c(-2,2))
    })
}

# to run, type this at prompt: >shinyApp(ui, server)