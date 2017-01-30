library(shiny)
library(shinyjs)
library(shinythemes)


# Define UI for application
shinyUI(tagList(
  navbarPage(title=div(img(src="DirectAMS_logo.png",height="35",width="80"),
                       "NEC Data Analysis Program"),
             theme=shinytheme("spacelab"),
    tabPanel("Data",
         runlogFileInput("runlog", "Runlog File")
    ),
    
    tabPanel("Standards",
        standardsInput("standards", "Standard UI")
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