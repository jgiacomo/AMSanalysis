library(shiny)
library(shinyjs)

#sampleITNlist <- as.character(unique(rundata$label))


# Define UI for application
shinyUI(tagList(
  navbarPage("NEC Data Analysis Program",
    tabPanel("Data",
         runlogFileInput("runlog", "Runlog File")
    ),
    
    tabPanel("Outlier Removal",
        sidebarLayout(
            sidebarPanel(
                selectInput("samplePicker", "Sample", c("sample 1","sample 2")),
                width = 2  # adjust sidebar panel width
            ),
            
            # Main Panel
            mainPanel(
                plotOutput("runPlot",
                           click = "runPlot_click",
                           brush = brushOpts(id="runPlot_brush",
                                             resetOnNew = TRUE)),
                
                fluidRow(
                    column(4,
                           actionButton("back", "Previous Sample"),
                           actionButton("forward","Next Sample")
                    ),
                    column(6,
                           actionButton("exclude_reset", "Reactivate All Runs"),
                           actionButton("exclude_all", "Deactivate All Runs")
                    )
                ),
                
                h4("Statistics"),
                p(verbatimTextOutput("stats")),
                DT::dataTableOutput("runTable")
            )
        ))
)))