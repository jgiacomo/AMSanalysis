library(shiny)
library(shinyjs)
library(shinythemes)


# Define UI for application
shinyUI(tagList(
  navbarPage(title=div(img(src="DirectAMS_logo.png",height="35",width="80"),
                       "AMS Data Analysis Program"),
             theme=shinytheme("cerulean"),
    tabPanel("Data",
         runlogFileInput("runlog", "Runlog File")
    ),
    
    tabPanel("Standards",
        standardsInput("standards", "Standard UI")
    ),
    
    tabPanel("Blanks"),
    
    tabPanel("Unknowns",
        sidebarLayout(
            sidebarPanel(
                selectInput("samplePicker", "Sample", c("sample 1","sample 2")),
                width = 2  # adjust sidebar panel width
            ),
            
            # Main Panel
            mainPanel(
                fluidRow(
                    column(6,
                           wellPanel(
                               h5("Run 14C/13C Plot"),
                               runPlotUI("C14","Plot 14C/13C")
                           )
                    ),
                    column(6,
                           wellPanel(
                               h5("Run 13C/12C Plot"),
                               runPlotUI("C13","Plot 13C/12C")
                           )
                    )
                ),
                
                fluidRow(
                    column(3,offset=5,
                           actionButton("back", "Previous"),
                           actionButton("forward","Next")
                    )
                ),
                
                # h4("Statistics"),
                # p(verbatimTextOutput("stats")),
                plotOutput("sampleMetaPlot"),
                DT::dataTableOutput("runTable")
            )
        )),
    
    tabPanel("Report")
)))