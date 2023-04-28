


df_grouped <- dt_cot %>% 
  select(ticker, data, ret_252) |> 
  filter(dplyr::between(data, as.Date("2018-01-01"), as.Date("2023-01-01"))) |> 
  rename(return = ret_252) |>
  #filter(ticker == "ITUB4") |> 
  group_by(ticker, end_of_month = floor_date(data, "month")) %>% 
  arrange(ticker, data) %>% 
  ungroup()


dt_temp_2 <- matrix(NA_real_, nrow = nrow(df_grouped), ncol = 252) |> 
  data.frame() 


dt_temp_2 <- cbind(df_grouped, dt_temp_2) |> setDT()


special_subset <- function(x, dt_frame){
  
  vec_252 <- dt_frame[x:(x+252), .SD[c(1:252), .(return)]] |> t()
  
  if(length(vec_252) < 252){
    
    vec_252 <- c(vec_252, rep(NA_real_, 252 - length(vec_252)))
    
  }
  
  return(as.list(vec_252))
}

special_reshape <- function(dt_frame){

  for(i in 1:nrow(dt_frame)){
    
    dt_frame[i, (cols) := special_subset(x = i, dt_frame = dt_frame)]

  }
}


df_list <- split(dt_temp_2, dt_temp_2$ticker)
        
              
for(j in 1:length(df_list)){
  
  special_reshape(dt_frame = df_list[[j]]) 
  print(j)
  
}

# Test 

do.call("rbind", df_list) |> setDT() %>% .[, .SD[c(1, .N)], by = .(ticker)] |> View()


