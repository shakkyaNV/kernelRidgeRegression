here::i_am("Code/multiDvisualize.R")
library(here)
source(here("Code", "utils.R"))
library(ggplot2)
library(dplyr)

file_name = "HPCC_DGP1.csv" 
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



#############################
########### DGP2

df1 = readr::read_csv(here("Data", "HPCC_DGP2.csv"), 
                     col_names = c("id", "rmse", "V1", "V2", "V3", "V4", "V5", "V6", "n", "sd", "seed"), 
                     col_types = list("c", "d", "c","c","c","c","c","c","n","d","n"))
df1 |> 
  glimpse()

df1 |> 
  select(id, starts_with("V"), rmse) |> 
  tidyr::unite("degree", V1:V6) -> df1_c

df1_c |> 
  group_by(id, degree) |> 
  summarise(mean = mean(rmse)) |> 
  arrange(mean) -> df1_m

df1_m


df1 |> 
  left_join(y = df1_m, by = "id") |> 
  select(-starts_with("V")) |> 
  mutate(id = forcats::fct_reorder(id, rmse, .fun = 'mean'))  |> 
  arrange(mean) |> 
  head(318*10) |> 
  ggplot() + 
  geom_boxplot(aes(x = reorder(degree, mean), y = rmse, fill = id)) + 
  # ylim(0, 5) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))



########### DGP1



df2 = readr::read_csv(here("Data", "HPCC_DGP1.csv"), 
                     col_names = c("id", "rmse", "V1", "V2", "V3", "n", "sd", "seed"), 
                     col_types = list("c", "d", "c","c","c","c","c","c","n","d","n"))
df2 |> 
  glimpse()

df2 |> 
  select(id, starts_with("V"), rmse) |> 
  tidyr::unite("degree", V1:V3) -> df2_c

df2_c |> 
  group_by(id, degree) |> 
  summarise(mean = mean(rmse)) |> 
  arrange(mean) -> df2_m

df2_m


df2 |> 
  left_join(y = df2_m, by = "id") |>
  select(-starts_with("V")) |> 
  mutate(id = forcats::fct_reorder(id, rmse, .fun = 'mean'))  |> 
  arrange(mean) |> 
  head(318*4) |> 
  ggplot() + 
  geom_boxplot(aes(x = reorder(degree, mean), y = rmse, fill = id)) + 
  ylim(0, 5) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
