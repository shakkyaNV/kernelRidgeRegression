here::i_am("Code/epsilon_bootstrap_viz.R")
library(tidyverse)
library(here)
source(here("Code", "utils.R"))

file_name = "Charitha.csv"


print(f("scp sranasin@quanah.hpcc.ttu.edu:/home/sranasin/kernelRidgeRegression/Data/{file_name} ."))

## --Pull the file in terminal -- ##

df = readr::read_csv(here("Data", file_name), 
                     col_names = c("seed", "phi_n_original", "phi_n_star", "ptile.t", "p.value", "status", "b"), 
                     col_types = c("d", "d", "d", "d", "d", "c", "d"), 
                     )

df %>% 
  head()

####### Clean

df %>% filter(!is.na(b)) -> dfnew

dfnew %>% filter(status %in% c("Rejected", "Not_Rejected")) -> dfnew

# dfnew %>% transmute(b = as.numeric(b), p.value = as.numeric(p.value)) -> dfnew

####### Test

dfnew %>% group_by(status) %>% summarise(count = n())

dfnew %>% filter(b != 0) %>% group_by(status) %>% summarise(count = n())

####### viz

dfnew %>% head()

dfnew %>% select(p.value, b) %>% 
  group_by(b) %>% 
  summarise(p.value = mean(p.value, na.rm=TRUE)) %>% 
  ggplot(aes(x = b, y = p.value)) + 
  geom_path() + 
  geom_point()

### WORKING

### Charitha starting curve at -0.0035
















