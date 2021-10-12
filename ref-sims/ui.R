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
                                   "Talking" = "talk"),
                       selected = "teach"),
    uiOutput("trialRange"),
    uiOutput("threshold"),
    uiOutput("learningParameter"),
    uiOutput("pointCost"),
    uiOutput("speakCost"),
    uiOutput("alpha"),
    uiOutput("gamma"),
    uiOutput("nguesses"),
    uiOutput("C"),
    uiOutput("M")),
    
    mainPanel(
      tabsetPanel(id = "plotSelector",
                  tabPanel("Learning", plotOutput("learnPlot")),
                  tabPanel("Events to Threshold", plotOutput("threshPlot")))
      
    )
  )
)
