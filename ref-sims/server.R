library(tidyverse)
library(ggthemes)
library(here)
library(directlabels)
library(data.table)
library(forcats)
library(DescTools)
library(latex2exp)
library(glue)

LOCAL <- FALSE

if(LOCAL) {
  teach_props <- fread(here("cached_data/teach_props.csv")) %>%
    as_tibble()
                          
  com_props <- fread(here("cached_data/com_props.csv")) %>%
    as_tibble() %>%
    rename(gamma = lambda) %>%
    filter(P <= 70, S <= 20)
  
  talk_props <- fread(here("cached_data/talk_props.csv")) %>%
    as_tibble()

  
} else{
  teach_props <- fread("https://raw.githubusercontent.com/dyurovsky/ref-sims/master/cached_data/teach_props.csv") %>%
    as_tibble()
  
  com_props <- fread("https://raw.githubusercontent.com/dyurovsky/ref-sims/master/cached_data/com_props.csv") %>%
    as_tibble() %>%
    rename(gamma = lambda) %>%
    filter(P <= 70, S <= 20)
  
  talk_props <- fread("https://raw.githubusercontent.com/dyurovsky/ref-sims/master/cached_data/talk_props.csv") %>%
    as_tibble()
}

com_params <- com_props %>%
  distinct(P, S, alpha, gamma)

talk_params <- talk_props %>%
  distinct(M, C, nguesses)

all_models <- mutate(teach_props, model = "teach") %>%
  bind_rows(mutate(com_props, model = "com"),
            mutate(talk_props, model = "talk")) %>%
  mutate(print_model = factor(model, levels = c("teach", "com", "talk"), 
                        labels = c("Teaching", "Communication", 
                                   "Talking"))) %>%
  filter(trial >= 1)


theme_set(theme_few(base_size = 18))

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
  
  alpha <- reactive({
    input$alpha
  })
  
  gamma <- reactive({
    input$gamma
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
  
  trial_range <- reactive({
    input$trialRange
  })
  
  threshold <- reactive({
    input$threshold / 100
  })
  
  output$learnPlot = renderPlot({
    req(learn_p())
    req(chosen_models())
    req(trial_range())
    
    selected_models <- all_models %>%
      filter(p == learn_p(), model %in% chosen_models())
    
    if("com" %in% chosen_models()) {
      req(point_cost())
      req(speak_cost())
      req(alpha())
      req(gamma())
      
      selected_models <- selected_models %>%
        filter(is.na(P) | P == point_cost(),
               is.na(S) | S == speak_cost(),
               is.na(alpha) | alpha == alpha(),
               is.na(gamma) | gamma == gamma())
    }
    
    if("talk" %in% chosen_models()) {
      req(M())
      req(C())
      req(nguesses())
      
      selected_models <- selected_models %>%
        filter(is.na(M) | M == M(),
               is.na(C) | C == C(),
               is.na(nguesses) | nguesses == nguesses())
    }
  
    to_plot <- selected_models %>%
      ggplot(aes(x = trial, y = prob, color = print_model, 
                 label = print_model)) + 
      geom_smooth(se = FALSE, method = "loess", formula = "y ~ x") +
      geom_dl(method = list("smart.grid", cex = 1.3)) +
      scale_x_continuous(name = "Event number", 
                         limits = c(first(trial_range()), 
                                    last(trial_range()))) + 
      scale_y_continuous(name = "Probability of learning", limits = c(0, 1)) +
      scale_color_ptol(drop = FALSE) +
      theme(legend.position = "none")
    
    suppressWarnings(print(to_plot))
  })
  
  output$threshPlot = renderPlot({
    req(chosen_models())
    req(threshold())
    
    selected_models <- all_models %>%
      filter(model %in% chosen_models()) 
    
    if("com" %in% chosen_models()) {
      req(point_cost())
      req(speak_cost())
      req(alpha())
      req(gamma())
      
      selected_models <- selected_models %>%
        filter(is.na(P) | P == point_cost(),
               is.na(S) | S == speak_cost(),
               is.na(alpha) | alpha == alpha(),
               is.na(gamma) | gamma == gamma())
    }
    
    if("talk" %in% chosen_models()) {
      req(M())
      req(C())
      req(nguesses())
      
      selected_models <- selected_models %>%
        filter(is.na(M) | M == M(),
               is.na(C) | C == C(),
               is.na(nguesses) | nguesses == nguesses())
    }
    

    threshs <- selected_models %>%
      group_by_at(vars(-trial, -prob)) %>%
      filter(prob >= threshold()) %>%
      slice(1)
    
    to_plot <- threshs %>%
      ggplot(aes(x = p, y = trial, color = print_model, 
                 label = print_model)) + 
      geom_smooth(se = FALSE, method = "loess", formula = "y ~ x") +
      geom_dl(method = list("smart.grid", cex = 1.3)) +
      scale_x_continuous(limits = c(.1, 1),
                          breaks = seq(.1, 1, .1)) + 
      labs(x = TeX("Learning rate ($p$)"),
           y = glue("Events to {threshold() * 100}% learning")) + 
      scale_color_ptol() +
      theme(legend.position = "none")
    
    suppressWarnings(print(to_plot))
  })
  
  output$learningParameter <- renderUI({
    req(selected_plot())
    
    conditionalPanel(condition = "input.plotSelector == `Learning`",
                     sliderInput("learningParameter",
                                 label = "Learning Parameter",
                                 value = median(all_models$p),
                                 step =  1 / distinct(all_models, p) %>% pull() %>% length(), 
                                 min = min(all_models$p),
                                 max = max(all_models$p)))
  })
  
  
  output$trialRange <- renderUI({
    req(selected_plot())
    
    conditionalPanel(condition = "input.plotSelector == `Learning`",
                     sliderInput("trialRange",
                                 label = "Trial Range",
                                 step = 1, value = c(1, 50), 
                                 min = min(all_models$trial),
                                 max = max(all_models$trial)))
  })
  
  output$threshold <- renderUI({
    req(selected_plot())
    
    conditionalPanel(condition = "input.plotSelector == `Events to Threshold`",
                     sliderInput("threshold",
                                 label = "Percent learners succeed",
                                 value = 75,
                                 step =  1, 
                                 min = 1,
                                 max = 100)
    )
  })
  
  output$pointCost <- renderUI({
    req(chosen_models())
    
    conditionalPanel(condition = "input.modelSelect.includes('com')",
                     sliderInput("pointCost",
                                 label = "Cost of pointing",
                                 min = min(com_params$P),
                                 max = max(com_params$P),
                                 step = (max(com_params$P) - 
                                           min(com_params$P)) /
                                   (length(com_params %>% pull(P) %>% 
                                             unique()) - 1),
                                 value = 70))
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
                                 step = (max(options$S) - 
                                           min(options$S)) /
                                   (length(options %>% pull(S) %>% 
                                             unique()) - 1),
                                 value = first(options$S)))
    })
  
  output$alpha <- renderUI({
    req(chosen_models())
    
    conditionalPanel(condition = "input.modelSelect.includes('com')",
                     sliderInput("alpha",
                                 label = "Rationality",
                                 min = min(com_params$alpha),
                                 max = max(com_params$alpha),
                                 step = (max(com_params$alpha) - 
                                           min(com_params$alpha)) /
                                   (length(com_params %>% pull(alpha) %>% 
                                             unique()) - 1),
                                 value = 2))
  })
  
  output$gamma <- renderUI({
    req(chosen_models())
    
    conditionalPanel(condition = "input.modelSelect.includes('com')",
                     sliderInput("gamma",
                                 label = "Discount rate",
                                 min = min(com_params$gamma),
                                 max = max(com_params$gamma),
                                 step = (max(com_params$gamma) - 
                                           min(com_params$gamma)) /
                                   (length(com_params %>% pull(gamma) %>% 
                                             unique()) - 1),
                                 value = .5))
  })
  
  output$nguesses <- renderUI({
    req(chosen_models())
    
    conditionalPanel(condition = "input.modelSelect.includes('talk')",
                     sliderInput("nguesses",
                                 label = "Number of Hypotheses",
                                 min = min(talk_params$nguesses),
                                 max = max(talk_params$nguesses),
                                 step = 1,
                                 value = 1))
  })
  
  output$C <- renderUI({
    req(chosen_models())
    req(nguesses())
    
    options <- talk_params %>%
      filter(nguesses == nguesses())
    
    conditionalPanel(condition = "input.modelSelect.includes('talk')",
                     sliderInput("C",
                                 label = "Objects per Event",
                                 min = min(options$C),
                                 max = max(options$C),
                                 step = 1,
                                 value = 4))
  })
  
  output$M <- renderUI({
    req(chosen_models())
    
    conditionalPanel(condition = "input.modelSelect.includes('talk')",
                     sliderInput("M",
                                 label = "Total Vocabulary Size",
                                 min = min(talk_params$M),
                                 max = max(talk_params$M),
                                 step = max(talk_params$M) - min(talk_params$M),
                                 value = 16))
  })
  
 
  
  
}