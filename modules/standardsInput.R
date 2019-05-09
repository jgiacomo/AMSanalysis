# Module UI function
#
# UI for cleaning the standards.

library(shiny)

standardsInput <- function(id, label="Standards"){
    ns <- NS(id)  # set the namespace for the module
    
    tagList(
        sidebarLayout(
            sidebarPanel(
                h3("Select Standards"),
                h4("by sample type"),
                checkboxGroupInput(inputId=ns("stdPicks"),
                                   label="Available sample types",
                                   choices=c("OXI","OX1","OXII","OX2","C7","C2",
                                             "ANU (C6)","C3","C5","C8"),
                                   selected="OXII",
                                   inline = TRUE),
                
                h3("Or"),
                
                h4("by position"),
                h5("e.g. '0-34, 38, 40' or 'all' for all standards selected above"),
                textInput(ns("stdPos"), label="",
                          placeholder = "all")
            ),
            
            mainPanel(
                tableOutput(ns("stdTable")),
                hr(),
                fluidRow(
                    h3("Adjust nominal values if necessary"),
                    column(2,h4("Sample",br(),"Type")),
                    column(3,h4("Nominal",br(),"pMC")),
                    column(3,h4("Nominal",br(),"d13C"))
                )
            )
        )
    )
}