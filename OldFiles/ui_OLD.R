library(shiny)

sampleITNlist <- as.character(unique(run.data$LABEL))


# Define UI for application
shinyUI(fluidPage(

  # Application title
  titlePanel(p(img(src="EZVS_Logo.png", height=50),
               "BioMICADAS Run Outlier Removal")
  ),

  # Sidebar
  sidebarLayout(
    sidebarPanel(
      textInput("magazine", "Magazine:"),
      selectInput("samplePicker", "Sample", sampleITNlist),
      
      uiOutput("RUNlist"),  # use the ui element described in server.R
      
      width = 2  # adjust sidebar panel width
    ),

    # Main Panel
    mainPanel(
      plotOutput("runPlot"),
      
      h4("Statistics"),
      p(verbatimTextOutput("stats")),
      dataTableOutput("runTable")
    )
  )
))