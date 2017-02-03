library(shiny)
library(shinyjs)
library(shinythemes)


# Define UI for application
shinyUI(tagList(
  navbarPage(title=div(img(src="DirectAMS_logo.png",height="35",width="80"),
                       "AMS Data Analysis Program"),
             theme=shinytheme("spacelab"),
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
                               plotOutput("C14Plot",
                                          click = "C14Plot_click",
                                          brush = brushOpts(id="C14Plot_brush",
                                                            resetOnNew = TRUE))
                           )
                    ),
                    column(6,
                           h4("d13C plot to go here.")
                    )
                ),
                
                fluidRow(
                    column(6,
                           actionButton("back", "Previous"),
                           actionButton("forward","Next"),
                           actionButton("exclude_reset", "Reactivate All"),
                           actionButton("exclude_all", "Deactivate All")
                    ),
                    column(6,
                           h4("Buttons")
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