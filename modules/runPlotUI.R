# Module UI function
#
# UI for interactive plot of run data.

library(shiny)

runPlotUI <- function(id, label="runPlot"){
    ns <- NS(id)  # set the namespace for the module
    
    tagList(
        plotOutput(ns("runPlot"),
                   click = ns("runPlot_click"),
                   brush = brushOpts(id=ns("runPlot_brush"),
                                     resetOnNew = TRUE)
        ),
        
        actionButton(ns("exclude_reset"), "Reactivate All"),
        actionButton(ns("exclude_all"), "Deactivate All")
        
        
    )
}