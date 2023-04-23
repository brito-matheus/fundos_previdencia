#### Importando os dados 
#### Data: 17/04 
#### Author: Matheus Carrijo de Brito


# Importando os pacotes

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


# Importando os dados 
dt <- readxl::read_excel(path = "../Fundos Previdênciários - Amostra - Dados Completos.xlsx") 

names(dt) <- names(dt) |> 
  str_to_lower() |> 
  str_replace_all(pattern = "\n", replacement = "_") |>
  str_to_lower() |>
  str_replace_all(pattern = "[áàâäã]",  replacement = "a") |> 
  str_replace_all(pattern = "[éèêë]",  replacement = "e") |> 
  str_replace_all(pattern = "[íìîï]",  replacement = "i") |> 
  str_replace_all(pattern = "[óòôö]",  replacement = "o") |> 
  str_replace_all(pattern = "[úùûü]",  replacement = "u") |> 
  str_replace_all(pattern = "[ç]",     replacement = "c") |> 
  str_replace_all(pattern = " ",     replacement = "_") |>
  str_replace_all(pattern = "__",     replacement = "_") |> 
  str_replace_all(pattern = "__",     replacement = "_") |> 
  str_remove_all(pattern = "\\.") |>
  str_remove_all(pattern = '\\(') |> 
  str_remove_all(pattern = "\\)") |>
  str_remove_all(pattern = "_$")



# Mudança de classe em algumas colunas  

dt <- dt |> 
  mutate(across(.cols = c("data_da_competencia", 'data_da_divulgacao'), .fns = as.Date)) |> 
  mutate(across(.cols = c("tipo_do_ativo", "tipo_de_aplicacao"), .fns = as.factor)) 



dt[, c("codigo", "nome", "data_da_competencia", "tipo_do_ativo")] |> 
  summary()

table_1 <- dt |> 
  mutate(year = lubridate::year(data_da_competencia)) |> 
  group_by(year, tipo_do_ativo) |> 
  summarise(N = n_distinct(nome_do_ativo), 
            Valor = sum(valor_do_ativo_mil, na.rm = T),
            Porcentagem_pl = (sum(valor_do_ativo_mil, na.rm = T)/sum(pl_mil, na.rm = T)) * 100) |> 
  na.omit() 
  
table_1_out  <- rbind(
  data.table::dcast(table_1, year ~ tipo_do_ativo, value.var = c("N")), 
  data.table::dcast(table_1, year ~ tipo_do_ativo, value.var = c("Valor")),
  data.table::dcast(table_1, year ~ tipo_do_ativo, value.var = c("Porcentagem_pl"))) 
  

kbl(table_1_out, format = "html", digits = 2) |> 
  kable_styling("hover") |>
  pack_rows(index = c("N" = 3, "Valor" = 3, "Porcentagem" = 3))

  
#### Selecionando apenas ações ---- 

dt_compra <- dt |> 
  select(codigo,  data_da_competencia, cod_ativo, tipo_de_aplicacao,  
         valor_do_ativo_mil, participacao_do_ativo, pl_mil, 
         quantidade_aquisicao, valor_aquisicao_mil, quantidade_total) |> 
  filter(tipo_de_aplicacao == "Ações") |> 
  filter(quantidade_aquisicao != 0) |> 
  mutate(ym = paste(year(data_da_competencia), month(data_da_competencia), sep = "_"))
  


dt_venda <- dt |> 
  select(codigo,  data_da_competencia, cod_ativo, tipo_de_aplicacao,
         valor_do_ativo_mil, participacao_do_ativo, pl_mil, 
         quantidade_venda, valor_venda_mil, quantidade_total) |> 
  filter(tipo_de_aplicacao == "Ações") |> 
  filter(quantidade_venda != 0) |> 
  mutate(ym = paste(year(data_da_competencia), month(data_da_competencia), sep = "_"))


# Numero de ações 

dt_compra$cod_ativo |> unique()

dt_venda$cod_ativo |> uniqueN()

# Importando dados de cotações:

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

# ordering  

#dt_cot[order(ticker, data), ] |> View()

# Selecting only ticker in the data set 

tickers <- data.frame(ticker = c(dt_compra$cod_ativo, dt_venda$cod_ativo)) |> 
  distinct()


# dt_cot |>
#   summarise(N_ticker = n_distinct(ticker), 
#             N_ticker_dt= n_distinct(!ticker %in% tickers$ticker))
# 
# 
# dt_cot |> 
#   filter(ticker %in% tickers$ticker) |> 
#   summarise(N_ticker = n_distinct(ticker))

# Making the subsequent returns  

dt_cot[, `:=`(ret_21 = frollapply(x = ret, n = 21, FUN = function(x) prod(1 + x, na.rm = T) - 1 , fill = NA, align = "right"),
              ret_63 = frollapply(x = ret, n = 63, FUN = function(x) prod(1 + x, na.rm = T) - 1 , fill = NA, align = "right"),
              ret_156 = frollapply(x = ret, n = 126, FUN = function(x) prod(1 + x, na.rm = T) - 1 , fill = NA, align = "right"),
              ret_252 = frollapply(x = ret, n = 252, FUN = function(x) prod(1 + x, na.rm = T) - 1 , fill = NA, align = "right"), 
              ret_21_exc = frollapply(x = ret_exc, n = 21, FUN = function(x) prod(1 + x, na.rm = T) - 1 , fill = NA, align = "right"), 
              ret_63_exc = frollapply(x = ret_exc, n = 63, FUN = function(x) prod(1 + x, na.rm = T) - 1 , fill = NA, align = "right"),
              ret_156_exc = frollapply(x = ret_exc, n = 126, FUN = function(x) prod(1 + x, na.rm = T) - 1 , fill = NA, align = "right"),
              ret_252_exc = frollapply(x = ret_exc, n = 252, FUN = function(x) prod(1 + x, na.rm = T) - 1 , fill = NA, align = "right")
              ), by = .(ticker)]

# Selecting only end of the month 

dt_month <- dt_cot[, .SD[.N], by = .(ticker, ym)] %>% 
  .[ticker %in% tickers$ticker] 


# Bind the data set of buys and sell with the return 

dt_compra_temp  <- left_join(x = dt_compra |> 
            rename(ticker = cod_ativo, 
                   data_competencia = data_da_competencia), 
            y = dt_month, by = c("ym", "ticker")) |> setDT()
  

dt_venda_temp <-  left_join(x = dt_venda |> 
                              rename(ticker = cod_ativo, 
                                     data_competencia = data_da_competencia),
                            y = dt_month, by = c("ym", "ticker")) |> setDT()


# Returns

dt_buy <- dt_compra_temp[, .(
  buy_ret_21_vw   = sum(participacao_do_ativo*ret_21, na.rm = T), 
  buy_ret_63_vw   = sum(participacao_do_ativo*ret_63, na.rm = T), 
  buy_ret_156_vw = sum(participacao_do_ativo*ret_156, na.rm = T), 
  buy_ret_252_vw  = sum(participacao_do_ativo*ret_252, na.rm = T), 
  buy_ret_21_eq   = mean(ret_21, na.rm = T), 
  buy_ret_63_eq   = mean(ret_63, na.rm = T), 
  buy_ret_156_eq   = mean(ret_156, na.rm = T), 
  buy_ret_252_eq   = mean(ret_252, na.rm = T)), 
  by = .(codigo, ym )] 


summary(dt_buy)

dt_sell <- dt_venda_temp[, .(
  sell_ret_21_vw   = sum(participacao_do_ativo*ret_21, na.rm = T),
  sell_ret_63_vw   = sum(participacao_do_ativo*ret_63, na.rm = T),
  sell_ret_156_vw = sum(participacao_do_ativo*ret_156, na.rm = T), 
  sell_ret_252_vw  = sum(participacao_do_ativo*ret_252, na.rm = T), 
  sell_ret_21_eq   = mean(ret_21, na.rm = T), 
  sell_ret_63_eq   = mean(ret_63, na.rm = T), 
  sell_ret_156_eq   = mean(ret_156, na.rm = T), 
  sell_ret_252_eq   = mean(ret_252, na.rm = T)), 
  by = .(codigo, ym )] 


# Analysis of Buys and Sells 

port_buy <- dt_buy[,
                   .(buy_ret_21_vw = mean(buy_ret_21_eq, na.rm = T), 
                     buy_ret_63_vw = mean(buy_ret_63_eq, na.rm = T), 
                     buy_ret_156_vw = mean(buy_ret_156_eq, na.rm = T),
                     buy_ret_252_vw = mean(buy_ret_252_eq, na.rm = T) 
                     ), by = ym]

port_sell <-  dt_sell[, 
                      .(sell_ret_21_vw = mean(sell_ret_21_eq, na.rm = T), 
                        sell_ret_63_vw = mean(sell_ret_63_eq, na.rm = T), 
                        sell_ret_156_vw = mean(sell_ret_156_eq, na.rm = T),
                        sell_ret_252_vw = mean(sell_ret_252_eq, na.rm = T)), by = ym]

port <- left_join(port_buy, port_sell, by = "ym")

port <- left_join(port, factors_monthly, by = "ym")

port <- port |> 
  mutate(port_21  =  (buy_ret_21_vw - sell_ret_21_vw) * 100, 
         port_63  =  (buy_ret_63_vw - sell_ret_63_vw) * 100, 
         port_156 =  (buy_ret_156_vw - sell_ret_156_vw) * 100, 
         port_252 =  (buy_ret_252_vw - sell_ret_252_vw) * 100)

# Only Alpha 
reg <- feols(c(port_21, port_63, port_156, port_252) ~ 1, data = port, vcov = "hetero")

etable(reg)

# Alpha and Beta 
reg <- feols(c(port_21, port_63, port_156, port_252) ~ Rm_minus_Rf, data = port, vcov = "hetero")

etable(reg)

# Alpha and Beta 
reg <- feols(c(port_21, port_63, port_156, port_252) ~ Rm_minus_Rf + HML + SMB, data = port, vcov = "hetero")

etable(reg)



# Excess of Returns ----


dt_buy_exc <- dt_compra_temp[, .(
  buy_ret_21_vw_exc  = sum(participacao_do_ativo*ret_21_exc, na.rm = T), 
  buy_ret_63_vw_exc   = sum(participacao_do_ativo*ret_63_exc, na.rm = T), 
  buy_ret_156_vw_exc = sum(participacao_do_ativo*ret_156_exc, na.rm = T), 
  buy_ret_252_vw_exc  = sum(participacao_do_ativo*ret_252_exc, na.rm = T), 
  buy_ret_21_eq_exc   = mean(ret_21_exc, na.rm = T), 
  buy_ret_63_eq_exc   = mean(ret_63_exc, na.rm = T), 
  buy_ret_156_eq_exc   = mean(ret_156_exc, na.rm = T), 
  buy_ret_252_eq_exc   = mean(ret_252_exc, na.rm = T)), 
  by = .(codigo, ym )] 


summary(dt_buy_exc)

dt_sell_exc <- dt_venda_temp[, .(
  sell_ret_21_vw_exc   = sum(participacao_do_ativo*ret_21_exc, na.rm = T),
  sell_ret_63_vw_exc   = sum(participacao_do_ativo*ret_63_exc, na.rm = T),
  sell_ret_156_vw_exc = sum(participacao_do_ativo*ret_156_exc, na.rm = T), 
  sell_ret_252_vw_exc  = sum(participacao_do_ativo*ret_252_exc, na.rm = T), 
  sell_ret_21_eq_exc   = mean(ret_21_exc, na.rm = T), 
  sell_ret_63_eq_exc   = mean(ret_63_exc, na.rm = T), 
  sell_ret_156_eq_exc   = mean(ret_156_exc, na.rm = T), 
  sell_ret_252_eq_exc   = mean(ret_252_exc, na.rm = T)), 
  by = .(codigo, ym )] 


summary(dt_sell_exc)


port_buy_exc <- dt_buy_exc[,
                   .(buy_ret_21_vw_exc = mean(buy_ret_21_eq_exc, na.rm = T), 
                     buy_ret_63_vw_exc = mean(buy_ret_63_eq_exc, na.rm = T), 
                     buy_ret_156_vw_exc = mean(buy_ret_156_eq_exc, na.rm = T),
                     buy_ret_252_vw_exc = mean(buy_ret_252_eq_exc, na.rm = T) 
                   ), by = ym]

port_sell_exc <-  dt_sell_exc[, 
                      .(sell_ret_21_vw_exc = mean(sell_ret_21_eq_exc, na.rm = T), 
                        sell_ret_63_vw_exc = mean(sell_ret_63_eq_exc, na.rm = T), 
                        sell_ret_156_vw_exc = mean(sell_ret_156_eq_exc, na.rm = T),
                        sell_ret_252_vw_exc = mean(sell_ret_252_eq_exc, na.rm = T)), by = ym]

port_exc <- left_join(port_buy_exc, port_sell_exc, by = "ym")

port_exc <- left_join(port_exc, factors_monthly, by = "ym")

port_exc <- port_exc |> 
  mutate(port_21_exc  =  (buy_ret_21_vw_exc - sell_ret_21_vw_exc) * 100, 
         port_63_exc  =  (buy_ret_63_vw_exc - sell_ret_63_vw_exc) * 100, 
         port_156_exc =  (buy_ret_156_vw_exc - sell_ret_156_vw_exc) * 100, 
         port_252_exc =  (buy_ret_252_vw_exc - sell_ret_252_vw_exc) * 100)

# Only Alpha 
reg <- feols(c(port_21_exc, port_63_exc, port_156_exc, port_252_exc) ~ 1, data = port_exc, vcov = "hetero")

etable(reg)

# Alpha and Beta 
reg <- feols(c(port_21_exc, port_63_exc, port_156_exc, port_252_exc) ~ Rm_minus_Rf, data = port_exc, vcov = "hetero")

etable(reg)

# Alpha and Beta 
reg <- feols(c(port_21_exc, port_63_exc, port_156_exc, port_252_exc) ~ Rm_minus_Rf + HML + SMB, data = port_exc, vcov = "hetero")

etable(reg)










