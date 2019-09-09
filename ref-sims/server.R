library(tidyverse)
library(ggthemes)
library(here)
library(directlabels)
library(forcats)

LOCAL <- FALSE

if(LOCAL) {
  teach_props <- read_csv(here("cached_data/teach_props.csv"))
  teach_aucs <- read_csv(here("cached_data/teach_aucs.csv"))
   
  com_props <- read_csv(here("cached_data/com_props.csv"))
  com_aucs <- read_csv(here("cached_data/com_aucs.csv"))
  
  pbv_props <- read_csv(here("cached_data/pbv_props.csv"))
  pbv_aucs <- read_csv(here("cached_data/pbv_aucs.csv"))
  
} else{
  teach_props <- read_csv("https://raw.githubusercontent.com/dyurovsky/ref-sims/master/cached_data/teach_props.csv")
  teach_aucs <- read_csv("https://raw.githubusercontent.com/dyurovsky/ref-sims/master/cached_data/teach_aucs.csv")
  
  com_props <- read_csv("https://raw.githubusercontent.com/dyurovsky/ref-sims/master/cached_data/com_props.csv")
  com_aucs <- read_csv("https://raw.githubusercontent.com/dyurovsky/ref-sims/master/cached_data/com_aucs.csv")
  
  pbv_props <- read_csv("https://raw.githubusercontent.com/dyurovsky/ref-sims/master/cached_data/pbv_props.csv")
  pbv_aucs <- read_csv("https://raw.githubusercontent.com/dyurovsky/ref-sims/master/cached_data/pbv_aucs.csv")
}

com_params <- com_props %>%
  distinct(P, S)

pbv_params <- pbv_props %>%
  distinct(M, C, nguesses)


all_models <- mutate(teach_props, model = "teach") %>%
  bind_rows(mutate(com_props, model = "com"),
            mutate(pbv_props, model = "pbv")) %>%
  mutate(print_model = factor(model, levels = c("teach", "com", "pbv"), 
                        labels = c("Teaching", "Communication", 
                                   "Propose but Verify")))


all_aucs <- mutate(teach_aucs, model = "teach") %>%
  bind_rows(mutate(com_aucs, model = "com"),
            mutate(pbv_aucs, model = "pbv")) %>%
  mutate(print_model = factor(model, levels = c("teach", "com", "pbv"), 
                              labels = c("Teaching", "Communication", 
                                         "Propose but Verify")))


theme_set(theme_classic(base_size = 16))

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
  
  selected_plot <- reactive({
    input$plotSelector
  })
  
  M <- reactive({
    input$M
  })
  
  C <- reactive({
    input$C
  })
  
  nguesses <- reactive({
    input$nguesses
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
    
    if("pbv" %in% chosen_models()) {
      selected_models <- selected_models %>%
        filter(is.na(M) | M == M(),
               is.na(C) | C == C(),
               is.na(nguesses) | nguesses == nguesses())
    }
    
   selected_models %>%
      ggplot(aes(x = trial, y = prob, color = print_model, 
                 label = print_model)) + 
      geom_smooth(se = FALSE) +
      geom_dl(method = "smart.grid") +
      scale_x_continuous(name = "Event number", limits = c(1, 100)) + 
      scale_y_continuous(name = "Probability of learning", limits = c(0, 1)) +
      scale_color_ptol() +
      theme(legend.position = "none")
  })
  
  output$aucPlot = renderPlot({
    req(chosen_models())
    
    selected_models <- all_aucs %>%
      filter( model %in% chosen_models())
    
    if("com" %in% chosen_models()) {
      selected_models <- selected_models %>%
        filter(is.na(P) | P == point_cost(),
               is.na(S) | S == speak_cost())
    }
    
    if("pbv" %in% chosen_models()) {
      selected_models <- selected_models %>%
        filter(is.na(M) | M == M(),
               is.na(C) | C == C(),
               is.na(nguesses) | nguesses == nguesses())
    }
    
    selected_models %>%
      ggplot(aes(x = p, y = auc, color = print_model, 
                 label = print_model)) + 
      geom_smooth(se = FALSE) +
      geom_dl(method = "smart.grid") +
      scale_x_continuous(name = "Learning parameter", limits = c(0, 1),
                         breaks = seq(0, 1, .1)) + 
      scale_y_continuous(name = "Area under the curve", limits = c(50, 100)) +
      scale_color_ptol() +
      theme(legend.position = "none")
  })
  
  output$learningParameter <- renderUI({
    req(selected_plot())
    
    conditionalPanel(condition = "input.plotSelector == `Learning`",
      sliderInput("learningParameter",
                  label = "Learning parameter",
                  value = median(all_models$p),
                  step =  1 / distinct(all_models, p) %>% pull() %>% length(), 
                  min = min(all_models$p),
                  max = max(all_models$p))
      )
  })
  
  output$pointCost <- renderUI({
    req(chosen_models())
    
    conditionalPanel(condition = "input.modelSelect.includes('com')",
                     sliderInput("pointCost",
                                 label = "Cost of pointing",
                                 min = min(com_params$P),
                                 max = max(com_params$P),
                                 step = max(com_params$P) - min(com_params$P),
                                 value = first(com_params$P)))
  })
  
  output$speakCost <- renderUI({
    req(chosen_models())
    req(point_cost())
    
    options <- com_params %>%
      filter(P == point_cost())
    
    conditionalPanel(condition = "input.modelSelect.includes('com')",
                     sliderInput("speakCost",
                                 label = "Cost of speaking",
                                 min = min(options$S),
                                 max = max(options$S),
                                 step = max(options$S) - min(options$S),
                                 value = first(options$S)))
    })
  
  output$nguesses <- renderUI({
    req(chosen_models())
    
    conditionalPanel(condition = "input.modelSelect.includes('pbv')",
                     sliderInput("nguesses",
                                 label = "Number of Hypotheses",
                                 min = min(pbv_params$nguesses),
                                 max = max(pbv_params$nguesses),
                                 step = 1,
                                 value = 1))
  })
  
  output$C <- renderUI({
    req(chosen_models())
    req(nguesses())
    
    options <- pbv_params %>%
      filter(nguesses == nguesses())
    
    conditionalPanel(condition = "input.modelSelect.includes('pbv')",
                     sliderInput("C",
                                 label = "Objects per Event",
                                 min = min(options$C),
                                 max = max(options$C),
                                 step = 1,
                                 value = 4))
  })
  
  output$M <- renderUI({
    req(chosen_models())
    
    conditionalPanel(condition = "input.modelSelect.includes('pbv')",
                     sliderInput("M",
                                 label = "Total Vocabulary Size",
                                 min = min(pbv_params$M),
                                 max = max(pbv_params$M),
                                 step = max(pbv_params$M) - min(pbv_params$M),
                                 value = 16))
  })
  
 
  
  
}