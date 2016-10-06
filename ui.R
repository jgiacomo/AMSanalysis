library(shiny)

sampleITNlist <- as.character(unique(rundata$label))


# Define UI for application
shinyUI(fluidPage(
    
    # Application title
    titlePanel(p(img(src="Accium_Full_Logo.png", height=80),
                 "NEC Run Outlier Removal")
    ),
    
    # Sidebar
    sidebarLayout(
        sidebarPanel(
            selectInput("samplePicker", "Sample", sampleITNlist),
            
            #uiOutput("RUNlist"),  # use the ui element described in server.R
            
            width = 2  # adjust sidebar panel width
        ),
        
        # Main Panel
        mainPanel(
            plotOutput("runPlot", click = "runPlot_click"),
            
            actionButton("exclude_reset", "Reset"),
            
            h4("Statistics"),
            p(verbatimTextOutput("stats")),
            dataTableOutput("runTable")
        )
    )
))