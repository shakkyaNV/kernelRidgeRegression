# Setup

rm(list = ls())
args <- commandArgs(TRUE)
parameters <- as.numeric(args)
n = parameters[1]
m = parameters[2]
seed = parameters[3]

library(logger)
library(here)
f <- glue::glue
config <- config::get(file=here("Code", "config.yml"))
log_appender(appender_file(here(config$path_to_logs, f("test_r_{seed}_{n}_{m}.log"))))
log_info("Hi There")

set.seed(seed)
log_info("Seed value: {seed}")
log_info("N: {n}, M: {m}, config values: {config$path_to_logs}")
log_info("See you !!")