library(shinyBS)
library(shinythemes)
library(shinyWidgets)

ui <- fluidPage(
  theme = shinytheme("cosmo"),
  
  titlePanel(title= "Modeling language learning in different settings", 
             windowTitle = "Modeling language learning in different settings"),
  
  fluidRow(
    
    column(4,
           
    h4("Simulation Parameters"),
    wellPanel(
      checkboxGroupInput("modelSelect",
                         label = "Parent Model",
                         choices = c("Teaching" = "teach",
                                     "Communication" = "com",
                                     "Talking" = "talk"),
                         selected = "teach"),
      uiOutput("C"),
      uiOutput("M"),
      uiOutput("trialRange"),
      uiOutput("threshold")),
    
    h4("Child"),
    conditionalPanel(
        condition = "input.modelSelect.length > 0",
        wellPanel(
          conditionalPanel(condition = "input.plotSelector == 'Learning'",
                           uiOutput("learningParameter")),
          uiOutput("nguesses"))),
    
    conditionalPanel(
      h4("Parent"),
      condition = "input.modelSelect.includes('com')",
      wellPanel(uiOutput("pointCost"),
        uiOutput("speakCost"),
        uiOutput("alpha"),
        uiOutput("gamma")))),
    
    column(8,
      tabsetPanel(id = "plotSelector",
                  tabPanel("Learning", plotOutput("learnPlot")),
                  tabPanel("Events to Threshold", plotOutput("threshPlot")))
      
    )
  )
)
