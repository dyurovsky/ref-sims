#!/usr/bin/env Rscript
library(data.table)
library(tidyverse)
library(feather)
library(glue)

args = commandArgs(trailingOnly=TRUE)

if (length(args) != 1) {
  stop("Need to specify the model", call.=FALSE)
} 

model <- args[1]

files <- list.files(glue("cached_data/joint_sim_splits/{model}"), pattern = "*.csv",
                    full.names = TRUE)

joint_sims <- map_dfr(files, fread) %>%
  group_by(model, M, C, nguesses, p, P, S, alpha, lambda, n) %>%
  summarise(prob = n()) %>%
  complete(n = full_seq(1:100, 1), p, M, C, nguesses, P, S, alpha, lambda,
           fill = list(prob =0)) %>%
  arrange(M, C, nguesses, p,  P, S, alpha, lambda, n) %>%
  mutate(prob = prob/sum(prob),
         prob = cumsum(prob)) %>%
  rename(trial = n)

write_feather(joint_sims, glue("cached_data/joint_{model}_props.feather"))
