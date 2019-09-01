library(shiny)
library(shinyjs)

#sampleITNlist <- as.character(unique(rundata$label))


# Define UI for application
shinyUI(tagList(
  useShinyjs(),
  navbarPage("NEC Data Analysis Program",
    tabPanel("Data",
         sidebarLayout(
             sidebarPanel(
                 radioButtons("labPick","Which Lab?",
                              choices=c("KIRAMS","DirectAMS"),
                              inline=TRUE),
                 fileInput("runlog", "Choose the runlog file"),
                 radioButtons("resultYN", "Is there a result.xls file?",
                              choices=c("Yes", "No"),
                              selected="No", inline=TRUE),
                 shinyjs::hidden(div(id="resultChooser",
                     fileInput("result", "Choose the NEC result.xls file")
                 )),
                 br(),
                 h4("Enter positions you want to analyze"),
                 h5("e.g. '0-34, 38, 40' or 'all' for all samples"),
                 textInput("runRows", label="",
                           placeholder = "0-40, 50, 55")
             ),
             
             mainPanel(
                 DT::dataTableOutput("runDT")
             )
         )
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
                           actionButton("exclude_reset", "Re-activate All Runs")
                    )
                ),
                
                h4("Statistics"),
                p(verbatimTextOutput("stats")),
                DT::dataTableOutput("runTable")
            )
        ))
)))