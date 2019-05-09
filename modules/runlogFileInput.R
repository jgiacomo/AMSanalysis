# Module UI function
#
# UI for getting the runlog file for AMS data.

library(shiny)
library(shinyjs)

runlogFileInput <- function(id, label="Runlog File"){
    ns <- NS(id)  # set the namespace for this ui module
    
    tagList(
        useShinyjs(),
        sidebarLayout(
            sidebarPanel(
                radioButtons(ns("lab"), "Choose Lab",
                             choices=c("KIRAMS","NEC"),
                             selected="KIRAMS",inline=TRUE),
                fileInput(ns("runlog"), "Choose the runlog file"),
                radioButtons(ns("resultYN"), "Is there a result.xls file?",
                             choices=c("Yes", "No"),
                             selected="No", inline=TRUE),
                shinyjs::hidden(div(id=ns("resultChooser"),
                        fileInput(ns("result"), "Choose the NEC result.xls file")
                )),
                br(),
                h4("Enter positions you want to analyze"),
                h5("e.g. '0-34, 38, 40' or 'all' for all samples"),
                textInput(ns("runRows"), label="",
                          placeholder = "0-40, 50, 55"),
                actionButton(ns("filterPosButton"),"Apply")
            ),
            
            mainPanel(
                DT::dataTableOutput(ns("runDT"))
            )
        )
    )
}