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
    uiOutput("trialRange"),
    uiOutput("learningParameter"),
    uiOutput("pointCost"),
    uiOutput("speakCost"),
    uiOutput("nguesses"),
    uiOutput("C"),
    uiOutput("M")),
    
    mainPanel(
      tabsetPanel(id = "plotSelector",
                  tabPanel("Learning", plotOutput("learnPlot")),
                  tabPanel("AUC", plotOutput("aucPlot")))
      
    )
  )
)
