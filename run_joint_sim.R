#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)



if (length(args)==0) {
  stop("Need to specify the rows to simulate", call.=FALSE)
} 

suppressPackageStartupMessages(library(tidyverse))
library(tidyboot)
library(DescTools)
suppressPackageStartupMessages(library(here))
library(glue)
library(memoise)

U <- 100
EPSILON <- .001

DEFAULT_P <- 70
DEFAULT_S <- 0
DEFAULT_ALPHA <-2
DEFAULT_LAMBDA <- .5

MAX_DELAY <- 3
MAX_TRIAL <- 100

p <- .6

teach_outcome <- function(n = 1, p = .6) {
  1 - (1-p) ^ n
}

# this can't contain ps if it's going to get logged
speak_success_u <- function(S = 0) {
  log(max(U - S, 0) + EPSILON)
}

speak_fail_u <- function(S) {
  log(max(0 - S, EPSILON))
}

point_u <- function(P = 30) {
  log(max(U - P, 0) + EPSILON)
}

teach_u <- function(P = 30, S = 0) {
  log(max(U - P - S, 0) + EPSILON)
}

discount <- function(delay, lambda = .5) {
  lambda ^ delay
}

act_u <- function(delay, k, n, p = .6, P = DEFAULT_P, S = DEFAULT_S, 
                  alpha = DEFAULT_ALPHA, lambda = DEFAULT_LAMBDA) {
  
  if(delay > MAX_DELAY) {
    expected_util <- 0
    teach_p <- NA
    speak_p <- NA
  }
  
  else {
    if(k == 0) {
      
      speak_util <- speak_fail_u(S) +
        discount(delay + 1, lambda) * 
        (act_u(delay + 1, k = 0, n = 0, p, P, S, 
               alpha, lambda) %>% pull(eu))
      
      point_util <- point_u(P) + 
        discount(delay + 1, lambda) * 
        (act_u(delay + 1, k = 0, n = 0, p, P, S, 
               alpha, lambda) %>% pull(eu))
      
      #teach_outcome(1, p) instead of 1
      teach_util <- teach_u(P, S) + 
        discount(delay + 1, lambda) * 
        (act_u(delay + 1, k = 1, n = 1, p, P, S, 
               alpha, lambda) %>% pull(eu))
      
    } else if(k == 1) {
      speak_util <- speak_success_u(S) + 
        discount(delay + 1, lambda) * 
        (act_u(delay + 1, k = 1, n = 0, p, P, S, 
               alpha, lambda) %>% pull(eu))
      
      point_util <- point_u(P) + 
        discount(delay + 1, lambda) * 
        (act_u(delay + 1, k = 1, n = 0, p, P, S, 
               alpha, lambda) %>% pull(eu))
      
      teach_util <- teach_u(P, S) + 
        discount(delay + 1, lambda) * 
        (act_u(delay + 1, k = 1, n = n + 1, p, P, S, alpha, lambda) %>%
           pull(eu))
      
    } else {
      speak_util <- (k * (speak_success_u(S) +
                            discount(delay + 1, lambda) * 
                            (act_u(delay + 1, k = 1, n = 0, p, P, S, 
                                   alpha, lambda) 
                             %>% pull(eu)))) +
        ((1-k) * (speak_fail_u(S) + discount(delay+1, lambda) * 
                    (act_u(delay + 1, k = 0, n = 0, p, P, S, 
                           alpha, lambda) %>% pull(eu))))
      
      point_util <- point_u(P) + discount(delay + 1, lambda) * 
        (act_u(delay + 1, k, n, p, P, S, alpha, lambda) %>% pull(eu))
      
      # teach_outcome(n + 1, p) instead of 1
      teach_util <- teach_u(P, S) + discount(delay + 1, lambda) * 
        (act_u(delay + 1, k = 1, n = n + 1, p, P, S, 
               alpha, lambda) 
         %>% pull(eu))
    }
    
    teach_p <- exp(alpha * teach_util) / (
      exp(alpha * teach_util) + (exp(alpha * speak_util) + 
                                   (exp(alpha * point_util))))
    
    speak_p <- exp(alpha * speak_util) / (
      exp(alpha * speak_util) + (exp(alpha * point_util) + 
                                   (exp(alpha * teach_util))))
    
    expected_util <- teach_p * teach_util + (speak_p) * speak_util +
      (1 - speak_p - teach_p) * point_util
    
    #  print(expected_util)
  }

  tibble(delay = delay,
         n = n,
         eu = expected_util,
         pspeak = speak_p,
         ppoint = 1 - speak_p - teach_p,
         pteach = teach_p, 
         P = P,
         S = S,
         alpha = alpha,
         lambda = lambda)
  
}
m_act_u <- memoise(act_u)

generate_propose_stims <- function(C = 4, M = 16, n = 100) {
  replicate(n, enframe(sample(2:M, C - 1)), simplify = F) %>%
    bind_rows(.id = "trial") %>%
    select(-name) %>%
    rename(object = value) %>%
    mutate(trial = as.numeric(trial)) %>%
    bind_rows(tibble(trial = 1:n, object = 1))
}


joint_model<- function(model = "communicate", p = .6, C = 4, M = 16, n = 100, nguesses = 4,
                       P = DEFAULT_P, S = DEFAULT_S, alpha = DEFAULT_ALPHA, 
                       lambda = DEFAULT_LAMBDA) {
  stims <- generate_propose_stims(C, M, n)
  
  guesses <- 0
  trial <- 0
  n_taught <- 0
  
  for(i in 1:n) {
    
    
    if(model == "communicate") {
      caregiver_probs <- m_act_u(delay = 0, teach_outcome(n_taught, p), 
                                 n = n_taught, p, P = P, S = S, 
                                 alpha = alpha, lambda = lambda) 
      
      chosen_modality <- sample(c("pspeak", "ppoint", "pteach"), 1,
                                p = as.list(caregiver_probs[c("pspeak", "ppoint", 
                                                              "pteach")]))
    } else if(model == "teach") {
      chosen_modality <- "pteach"
    } else {
      chosen_modality <- "pspeak"
    }
    
    if(length(guesses) == 1 && guesses == 1) {
      trial <- i
      break
    }
    
    if(chosen_modality == "pteach") {
      trial_stims <- 1
      n_taught <- n_taught + 1
    } else if(chosen_modality == "pspeak") {
      trial_stims <- stims %>%
        filter(trial == i) %>%
        pull(object)
      n_taught <- 0
    } else { #pointing doesn't help
      next
    }
    matches <- intersect(guesses, trial_stims)
    if(is_empty(matches)) {
      selected <- sample(trial_stims, min(nguesses, length(trial_stims)))
      
      kept <- selected[as.logical(rbinom(length(selected), 1, p))]
      if(is_empty(kept))
        guesses <- 0
      else
        guesses <- kept
    } else {
      guesses <- matches
    }
    
  }
  return(trial)
}

joint_samples <- function(model = "communicate", reps = 1000, p = .6, C = 4, M = 16, n = 100, 
                          nguesses = 1, P = DEFAULT_P, S = DEFAULT_S, 
                          alpha = DEFAULT_ALPHA, lambda = DEFAULT_LAMBDA) {
  # print(glue("model = {model}, p = {p}, C = {C}, M = {M}, nguesses = {nguesses} 
  #            P = {P}, S = {S}, alpha = {alpha}, lambda = {lambda}"))
  replicate(reps, joint_model(model, p, C, M, n, nguesses, 
                              P, S , alpha, lambda)) %>%
    enframe() %>%
    select(-name) %>%
    mutate(model = model, p = p, C = C, M = M, nguesses = nguesses, P = P, S = S, 
           alpha = alpha, lambda = lambda) %>%
    rename(n = value)
}

joint_params <- expand_grid(p = seq(.2, 1, .2), P = c(50, 60, 70), 
                            S = c(0, 10, 20), alpha = seq(.5, 3, .5),
                            lambda = seq(.2, 1, .2),
                            C = c(1, 2, 4, 8), 
                            nguesses = c(1, 2, 3, 4), 
                            M = c(8, 32, 128)) %>%
  filter(P > S, (P + S) <= 100) %>%
  filter(nguesses <= C)

join_props <- map_df((as.numeric(args[1])-99):as.numeric(args[1]), 
                     ~joint_samples(p = joint_params %>% slice(.x) %>% pull(p), 
                                    C = joint_params %>% slice(.x) %>% pull(C),
                                    M = joint_params %>% slice(.x) %>% pull(M),
                                    nguesses = joint_params %>% slice(.x) %>%
                                      pull(nguesses),
                                    P = joint_params %>% slice(.x) %>% pull(P),
                                    S = joint_params %>% slice(.x) %>% pull(S),
                                    alpha = joint_params %>% slice(.x) %>% 
                                      pull(alpha),
                                    lambda = joint_params %>% slice(.x) %>% 
                                      pull(lambda)))


write_csv(join_props, here(glue("cached_data/joint{args[1]}.csv")))
