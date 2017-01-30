# Module UI function
#
# UI for cleaning the standards.

library(shiny)

standardsInput <- function(id, label="Standards"){
    ns <- NS(id)  # set the namespace for the module
    
    tagList(
        sidebarLayout(
            sidebarPanel(
                h4("Enter positions of the standards"),
                h5("e.g. '0-34, 38, 40' or 'all' for all standards"),
                textInput(ns("stdPos"), label="",
                          placeholder = "")
            ),
            
            mainPanel(
                h1("Nothing to see here... Yet.")
            )
        )
    )
}