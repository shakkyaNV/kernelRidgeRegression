here::i_am("Code/multiDvisualize.R")
library(here)
source(here("Code", "utils.R"))
library(ggplot2)
library(dplyr)

file_name = "woDGP2Int1.csv" 
file_for = "with First Order Interactions DGP2"

print(f("scp sranasin@quanah.hpcc.ttu.edu:/home/sranasin/kernelRidgeRegression/Data/{file_name} ."))

## --Pull the file in terminal -- ##

df = readr::read_csv(here("Data", file_name), 
                     col_names = c("n", "sd", "seed", "rmse"))
df %>% 
  head()


# Plot

df |> 
  select(n, rmse) |> 
  mutate( n = factor(n)) |> 
  ggplot(aes(y = rmse, fill = n)) + 
  geom_boxplot() + 
  ggtitle(file_for) + 
  ylim(0, 10)

ggsave(filename = here("Data", f("{file_for}.jpeg")))

# mean of items
df |> 
  select(n, rmse) |> 
  mutate(n = factor(n)) |> 
  group_by(n) |> 
  summarise(mean(rmse))














