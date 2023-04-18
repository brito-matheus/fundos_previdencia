#### Importando os dados 
#### Data: 17/04 
#### Author: Matheus Carrijo de Brito


# Importando os pacotes

library(readxl)
library(tidyquant)
library(quantmod) 
library(tidyverse)
library(data.table)
library(stringr)
library(kableExtra)


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


dt$tipo_do_ativo |> levels()


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

  
  
  





