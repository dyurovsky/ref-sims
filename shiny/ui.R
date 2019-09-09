library(shinyBS)
library(shinythemes)
library(shinyWidgets)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  sidebarLayout(
    
    sidebarPanel(
    checkboxGroupInput("modelSelect",
                       label = "Select Model",
                       choices = c("Teaching" = "teach",
                                   "Communication" = "com",
                                   "Propose but Verify" = "pbv"),
                       selected = "teach"),
    uiOutput("learningParameter"),
    uiOutput("comParameters")),
    
    mainPanel(
      plotOutput("learnPlot")
    )
  )
)
