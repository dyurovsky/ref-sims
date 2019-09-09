library(tidyverse)
library(ggthemes)
library(here)
library(directlabels)
library(forcats)

teach_props <- read_csv(here("cached_data/teach_props.csv"))
teach_aucs <- read_csv(here("cached_data/teach_aucs.csv"))
 
com_props <- read_csv(here("cached_data/com_props.csv"))
com_aucs <- read_csv(here("cached_data/com_aucs.csv"))

com_params <- com_props %>%
  distinct(P, S)

all_models <- mutate(teach_props, model = "teach") %>%
  bind_rows(mutate(com_props, model = "com")) %>%
  mutate(print_model = factor(model, levels = c("teach", "com"), 
                        labels = c("Teaching", "Communication")))


theme_set(theme_few(base_size = 16))

server <- function(input, output){
  
  learn_p <- reactive({
    input$learningParameter
  })
  
  chosen_models <- reactive({
    input$modelSelect
  })
  
  point_cost <- reactive({
    input$pointCost
  })
  
  speak_cost <- reactive({
    input$speakCost
  })
  
  
  output$learnPlot = renderPlot({
    req(learn_p())
    req(chosen_models())
    
    selected_models <- all_models %>%
      filter(p == learn_p(), model %in% chosen_models())
    
    if("com" %in% chosen_models()) {
      selected_models <- selected_models %>%
      filter(is.na(P) | P == point_cost(),
             is.na(S) | S == speak_cost())
      }
    
   selected_models %>%
      ggplot(aes(x = trial, y = prob, color = model, label = print_model)) + 
      geom_line() +
      geom_dl(method = "smart.grid") +
      scale_x_continuous(name = "Event number", limits = c(1, 100)) + 
      scale_y_continuous(name = "Probability of learning", limits = c(0, 1)) +
      scale_color_ptol() +
      theme(legend.position = "none")
  })
  
  output$learningParameter <- renderUI({
    sliderInput("learningParameter",
                label = "Learning parameter",
                value = median(all_models$p),
                step =  1 / distinct(all_models, p) %>% pull() %>% length(), 
                min = min(all_models$p),
                max = max(all_models$p))
  })
  
  output$comParameters <- renderUI({
    req(chosen_models())
    
    conditionalPanel(condition = "input.modelSelect.includess('com')",
                     selectInput("pointCost",
                                 label = "Cost of pointing",
                                 choices = com_params$P,
                                 selected = first(com_params$P)),
                     selectInput("speakCost",
                                 label = "Cost of speaking",
                                 choices = com_params$S,
                                 selected = first(com_params$S)))
  })
}
