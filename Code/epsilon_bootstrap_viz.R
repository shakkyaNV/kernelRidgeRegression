here::i_am("Code/epsilon_bootstrap_viz.R")
library(tidyverse)
library(here)
source(here("Code", "utils.R"))

file_name = "SleepToken.csv"


print(f("scp sranasin@quanah.hpcc.ttu.edu:/home/sranasin/kernelRidgeRegression/Data/{file_name} ."))

## --Pull the file in terminal -- ##

df = readr::read_csv(here("Data", file_name), 
                     col_names = c("seed", "phi_n_original", "phi_n_star", "ptile.t", "p.value", "status", "b"))
df %>% 
  head()

####### Clean

df %>% filter(!is.na(b)) -> dfnew

dfnew %>% filter(status %in% c("Rejected", "Not_Rejected")) -> dfnew

####### Test

dfnew %>% group_by(status) %>% summarise(count = n())

dfnew %>% filter(b != 0) %>% group_by(status) %>% summarise(count = n())

####### viz

dfnew %>% head()

dfnew %>% select(p.value, b) %>% 
  group_by(b) %>% 
  summarise(p.value = mean(p.value)) %>% 
  ggplot(aes(x = b, y = p.value)) + 
  geom_point()

### WORKING


















