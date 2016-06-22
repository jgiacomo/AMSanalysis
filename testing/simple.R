library(shiny)
library(ggplot2)
library(dplyr)

myMPG <- mpg
myMPG$manufacturer <- as.character(myMPG$manufacturer)
myMPG$volCyl <- myMPG$displ/myMPG$cyl
myMPG$active <- TRUE

ui <- fluidPage(
    sidebarPanel(
        selectInput("manuf", "Manufacturer", unique(myMPG$manufacturer)),
    ),

    mainPanel(
        plotOutput("plot1", click="plot_click"),
        tableOutput("table")
    )
)
server <- function(input, output) {
    
    vals <- reactiveValues()
    
    observeEvent(input$manuf,{
        vals$df <- myMPG[myMPG$manufacturer==input$manuf,]
        vals$keeprows <- rep(TRUE, nrow(vals$df))
        cat("keeprows",vals$keeprows,"\n")
    })

    observeEvent(input$plot_click, {
        cat("exclude", which(!vals$keeprows), "\n")
        res <- nearPoints(vals$df, input$plot_click, allRows = TRUE)
        cat("clicked", which(res$selected_),"\n")
        vals$keeprows <- xor(vals$keeprows, res$selected_)
    })    
    
    output$plot1 <- renderPlot({
        keep    <- vals$df[vals$keeprows,  , drop=FALSE]
        exclude <- vals$df[!vals$keeprows, , drop=FALSE]
        
        ggplot(keep, aes(x=volCyl, y=hwy)) +
            geom_point(size=3) +
            ggtitle(paste(isolate(input$manuf),
                          "Gas Mileage vs. Volume per Cylinder")
                    ) +
            geom_point(data=exclude, shape=21, fill=NA, color="black",
                       alpha=0.25) +
            coord_cartesian(ylim = c(0,50))
    })
    
    output$table <- renderTable({
        vals$df[,c(2,3,4,5,8,9)]
    })
}