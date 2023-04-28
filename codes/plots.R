#### Author: Matheus Carrijo de Brito 
#### Construção de Previsão 
#### Data: 27/04/2023

rm(list = ls())

library(readxl)
library(tidyquant)
library(quantmod) 
library(tidyverse)
library(data.table)
library(stringr)
library(kableExtra)
library(fixest)


# importando os dados de fatores 

source("codes/importando_fatores.R")


# Importando dados de fatores 


dt_cot <- read_csv("../cotação.csv", 
                   locale = locale(encoding = "latin1"), 
                   col_names = c("ticker", "data", "close", "open", "min", "max",
                                 "mean", "quant", "quant_tit", "vol"), skip = 1) |> 
  select(ticker, data, close, quant, vol) |> 
  mutate(ticker = str_remove(ticker, pattern = "<XBSP>")) |>
  mutate(close = as.numeric(close),
         quant = as.numeric(quant), 
         vol = as.numeric(vol))

dt_cot <- dt_cot |> 
  mutate(ym = paste(year(data), month(data), sep = "_")) |> 
  group_by(ym) |> 
  mutate(data_da_competencia = last(data)) |> 
  ungroup() |> 
  group_by(ticker) %>%
  mutate(index = match(ym, unique(ym))) %>%
  ungroup() |> 
  na.omit()


dt_cot <- setDT(dt_cot) %>%
  .[order(ticker, data), ]


# Making the return
dt_cot[ ,`:=`(ret = c(NA, diff(log(close)))), by = .(ticker)]

dt_cot <- factors[dt_cot, on = "data"]

dt_cot[, `:=`(ret_exc = ((ret - Risk_free) - Rm_minus_Rf))]

# Construction of the future returns 

dt_cot[, `:=`(ret_21 = frollapply(x = ret, n = 21, FUN = function(x) prod(1 + x, na.rm = T) - 1 , fill = NA, align = "right"),
              ret_63 = frollapply(x = ret, n = 63, FUN = function(x) prod(1 + x, na.rm = T) - 1 , fill = NA, align = "right"),
              ret_156 = frollapply(x = ret, n = 126, FUN = function(x) prod(1 + x, na.rm = T) - 1 , fill = NA, align = "right"),
              ret_252 = frollapply(x = ret, n = 252, FUN = function(x) prod(1 + x, na.rm = T) - 1 , fill = NA, align = "right"), 
              ret_21_exc = frollapply(x = ret_exc, n = 21, FUN = function(x) prod(1 + x, na.rm = T) - 1 , fill = NA, align = "right"), 
              ret_63_exc = frollapply(x = ret_exc, n = 63, FUN = function(x) prod(1 + x, na.rm = T) - 1 , fill = NA, align = "right"),
              ret_156_exc = frollapply(x = ret_exc, n = 126, FUN = function(x) prod(1 + x, na.rm = T) - 1 , fill = NA, align = "right"),
              ret_252_exc = frollapply(x = ret_exc, n = 252, FUN = function(x) prod(1 + x, na.rm = T) - 1 , fill = NA, align = "right")
), by = .(ticker)]

# Manipulação de dados 

df_grouped <- dt_cot %>% 
  select(ticker, data, ret_252) |> 
  filter(dplyr::between(data, as.Date("2018-01-01"), as.Date("2023-01-01"))) |> 
  rename(return = ret_252) |>
  #filter(ticker == "ITUB4") |> 
  group_by(ticker, end_of_month = floor_date(data, "month")) %>% 
  arrange(ticker, data) %>% 
  ungroup()

# Construction of the future return index data frame
dt_temp_2 <- matrix(NA_real_, nrow = nrow(df_grouped), ncol = 252) |> 
  data.frame() 

# binding the 
dt_temp_2 <- cbind(df_grouped, dt_temp_2) |> setDT()

# Name of the columns 
cols <- sprintf("X%s", seq(1, 252, 1))

# Special functions 
special_subset <- function(x, dt_frame){
  
  vec_252 <- dt_frame[x:(x+252), .SD[c(1:252), .(return)]] |> t()
  
  if(length(vec_252) < 252){
    
    vec_252 <- c(vec_252, rep(NA_real_, 252 - length(vec_252)))
    
  }
  
  return(as.list(vec_252))
}

special_reshape <- function(dt_frame){
  
  # Name of the columns 
  cols <- sprintf("X%s", seq(1, 252, 1))
  
  
  
  for(i in 1:nrow(dt_frame)){
    
    dt_frame[i, (cols) := special_subset(x = i, dt_frame = dt_frame)]

  }
}

# Split the data frame in by tickers
df_list <- split(dt_temp_2, dt_temp_2$ticker)


# Passing the function in a list of data frames 
for(j in 1:length(df_list)){
  
  special_reshape(dt_frame = df_list[[j]]) 
  print(j)
  
}

# Test 

dt_cot_future <- do.call("rbind", df_list) 


readr::write_csv(x = dt_cot_future, file = '../dados/dt_cot_future.csv')

