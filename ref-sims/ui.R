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
                         label = "Select Model",
                         choices = c("Teaching" = "teach",
                                     "Communication" = "com",
                                     "Talking" = "talk"),
                         selected = "teach"),
      uiOutput("trialRange"),
      uiOutput("threshold")),
  
    conditionalPanel(
        condition = "(input.modelSelect.length > 0) & (input.plotSelector == 'Learning')",
        h4("All models"),
        wellPanel(
          uiOutput("learningParameter"))),
    
    conditionalPanel(
      condition = "input.modelSelect.includes('com')",
      h4("Communication"),
      wellPanel(
        uiOutput("pointCost"),
        uiOutput("speakCost"),
        uiOutput("alpha"),
        uiOutput("gamma"))),
    
    conditionalPanel(
      condition = "input.modelSelect.includes('talk')",
      h4("Talking"),
      wellPanel(
        uiOutput("nguesses"),
        uiOutput("C"),
        uiOutput("M")))
    ),
    
    column(8,
      tabsetPanel(id = "plotSelector",
                  tabPanel("Learning", plotOutput("learnPlot")),
                  tabPanel("Events to Threshold", plotOutput("threshPlot")))
      
    )
  )
)
