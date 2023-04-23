#### importando dados de fatores:
#### Author: Matheus Carrijo de Brito
#### Data: 23/04/2023 

library(readxl)
library(tidyquant)
library(quantmod) 
library(tidyverse)
library(data.table)
library(stringr)
library(kableExtra)

files_fators <- list.files("../fatores")[str_ends(string = list.files("../fatores"), pattern = "xls")]

factors <- read_xls(path = paste("../fatores", files_fators[1],sep = '/')) |> 
  mutate(data = as.Date(paste(year, month, day, sep = "-"))) |>
  select(5, 4) 

for(i in 2:6){

  factors <<- left_join(factors, 
    read_xls(path = paste("../fatores", files_fators[i],sep = '/')) |> 
    mutate(data = as.Date(paste(year, month, day, sep = "-"))) |>
    select(5, 4), 
    by = "data")  

}

factors < setDT(factors)


factors_xts  <- xts(factors[, 2:ncol(factors)], order.by = factors$data)


factors_monthly <- do.call("cbind", lapply(factors_xts, monthlyReturn)) |> 
  `colnames<-`(c(names(factors_xts)))

factors_monthly <- as.data.frame(factors_monthly) |> 
  mutate(data = index(factors_monthly))

factors_monthly <- factors_monthly |>  mutate(ym = paste(year(data), month(data), sep = "_"))


rm(factors_xts)
