team_name_fct = function(){
  team_name <- c("Andretti Formula E" = "Andretti",
                 "TAG Heuer Porsche Formula E Team" = "Porsche",
                 "MAHINDRA RACING" = "Mahindra",
                 "NEOM McLaren Formula E Team" = "McLaren",
                 "Envision Racing" = "Envision",
                 "Jaguar TCS Racing" = "Jaguar",
                 "DS PENSKE" = "Penske",
                 "Maserati MSG Racing" = "Maserati",
                 "LOLA YAMAHA ABT Formula E Team" = "Abt",
                 "Nissan Formula E Team" = "Nissan",
                 "Cupra Kiro" = "Kiro")
  
  return(team_name)
}

team_color_fct = function(){
  team_color <- c("Andretti" = "#ed3124",
                  "Porsche" = "#551ca7",
                  "Mahindra" = "#dd052b",
                  "McLaren" = "#ff8000",
                  "Envision" = "#00be26",
                  "Jaguar" = "#343434",
                  "Penske" = "#cba65f",
                  "Maserati" = "#001489",
                  "Abt" = "#194997",
                  "Nissan" = "#fac7dc",
                  "Kiro" = "#effe03")
  
  return(team_color)
}

laps_read_list_fct = function(){
  laps_read_list <- data.frame(c("R01" = 
                                   "data/Season11/R01_SaoPaulo/23_R01 Analysis_Race.csv",
                                 "R02" = 
                                   "data/Season11/R02_Mexico/23_R02 Analysis_Race.csv"))
  colnames(laps_read_list) <- c("path_laps")
  
  return(laps_read_list)
}

result_read_list_fct = function(){
  result_read_list <- data.frame(c("R01" =
                "data/Season11/R01_SaoPaulo/03_R01 Classification_Race.csv",
                "R02" =
                  "data/Season11/R02_Mexico/03_R02 Classification_Race.csv"))
  colnames(result_read_list) <- c("path_result")
  
  return(result_read_list)
}

wdc_read_list_fct = function(){
  wdc_read_list <- data.frame(c("R01" =
                                     "data/Season11/R01_SaoPaulo/Drivers Championship.csv",
                                "R02" =
                                  "data/Season11/R02_Mexico/Drivers Championship.csv"))
  colnames(wdc_read_list) <- c("path_wdc")
  
  return(wdc_read_list)
}

sector_len_list_fct = function(){
  S1_len = c("R01" =  980, "R02" =  947)
  S2_len = c("R01" = 2160, "R02" =  1846)
  S3_len = c("R01" = 2642, "R02" =  2630)
  sector_len_list <- data.frame(S1_len, S2_len, S3_len)
  
  return(sector_len_list)
}

race_data_fct = function(){
  laps_read_list <- laps_read_list_fct()
  result_read_list <- result_read_list_fct()
  sector_len_list <- sector_len_list_fct()
  wdc_read_list <- wdc_read_list_fct()
  
  round_number <- c("R01" = "R01_", "R02" = "R02_")
  folder_name <- c("R01" = "R01_SaoPaulo", "R02" = "R02_Mexico")
  race_name <- c("R01" = "SaoPaulo", "R02" = "Mexico")
  
  race_data <- data.frame(round_number, folder_name, race_name,
                          laps_read_list, result_read_list, wdc_read_list)
  
  return(race_data)
}