here::i_am("Code/multiDvisualize.R")
library(here)
source(here("Code", "utils.R"))
library(ggplot2)

## Execute following in terminal while in Data folder
## scp sranasin@quanah.hpcc.ttu.edu:/home/sranasin/kernelRidgeRegression/Data/datamultiD.csv .

file_name = "dataMultiDOct11.csv"
df = readr::read_csv(here("Data", file_name), 
                     col_names = c("n", "sd", "seed", "rmse"))
df %>% 
  head()

df %>% 
  select(rmse) %>% summary()
  ggplot(aes(rmse)) + 
  geom_boxplot()
