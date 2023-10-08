# Setup

rm(list = ls())
library(here)
library(logger)
i_am("Code/R_main.R")
config <- config::get(here("config.yml"))
log_info("Hi There")
args <- commandArgs(TRUE)
parameters <- as.numeric(args)
n = parameters[1]
m = parameters[2]
seed = parameter[3]
set.seed(seed)

log_info("Seed value: {seed}")
log_info("N: {n}, M: {m}, config values: {config")