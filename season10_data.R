team_name_fct = function(){
  team_name <- c("Andretti Formula E" = "Andretti",
                 "TAG Heuer Porsche Formula E Team" = "Porsche",
                 "MAHINDRA RACING" = "Mahindra",
                 "NEOM McLaren Formula E Team" = "McLaren",
                 "Envision Racing" = "Envision",
                 "Jaguar TCS Racing" = "Jaguar",
                 "DS PENSKE" = "Penske",
                 "Maserati MSG Racing" = "Maserati",
                 "ABT CUPRA FORMULA E TEAM" = "Abt",
                 "Nissan Formula E Team" = "Nissan",
                 "ERT Formula E Team" = "ERT")
  
  return(team_name)
}

team_color_fct = function(){
  team_color <- c("Andretti" = "#de3b3a",
                  "Porsche" = "#cc001b",
                  "Mahindra" = "#d50428",
                  "McLaren" = "#f57a01",
                  "Envision" = "#00b826",
                  "Jaguar" = "#FFFFFF",
                  "Penske" = "#c2a15c",
                  "Maserati" = "#011284",
                  "Abt" = "#b90302",
                  "Nissan" = "#bb012c",
                  "ERT" = "#dbdb13")
  
  return(team_color)
}

laps_read_list_fct = function(){
  laps_read_list <- data.frame(c("R01" = 
                                   "data/Season10/R01_MexicoCity/23_R01 Analysis_Race.csv",
                                 "R02" =
                                   "data/Season10/R02_Diriyah1/23_R02 Analysis_Race.csv",
                                 "R03" =
                                   "data/Season10/R03_Diriyah2/23_R03 Analysis_Race.csv",
                                 "R04" =
                                   "data/Season10/R04_SaoPaulo/23_R04 Analysis_Race.csv",
                                 "R05" =
                                   "data/Season10/R05_Tokyo/23_R05 Analysis_Race.csv",
                                 "R06" =
                                   "data/Season10/R06_Misano1/23_R06 Analysis_Race.csv",
                                 "R07" =
                                   "data/Season10/R07_Misano2/23_R07 Analysis_Race.csv",
                                 "R08" =
                                   "data/Season10/R08_Monaco/23_R08 Analysis_Race.csv",
                                 "R09" =
                                   "data/Season10/R09_Berlin1/23_R09 Analysis_Race.csv",
                                 "R10" =
                                   "data/Season10/R10_Berlin2/23_R10 Analysis_Race.csv",
                                 "R11" =
                                   "data/Season10/R11_Shanghai1/23_R11 Analysis_Race.csv",
                                 "R12" =
                                   "data/Season10/R12_Shanghai2/23_R12 Analysis_Race.csv",
                                 "R13" =
                                   "data/Season10/R13_Portland1/23_R13 Analysis_Race.csv",
                                 "R14" =
                                   "data/Season10/R03_Diriyah2/23_R03 Analysis_Race.csv",
                                 "R15" =
                                   "data/Season10/R15_London1/23_R15 Analysis_Race.csv",
                                 "R16" =
                                   "data/Season10/R16_London2/23_R16 Analysis_Race.csv"))
  colnames(laps_read_list) <- c("path_laps")
  
  return(laps_read_list)
}

result_read_list_fct = function(){
  result_read_list <- data.frame(c("R01" =
                "data/Season10/R01_MexicoCity/03_R01 Classification_Race.csv",
               "R02" = 
                 "data/Season10/R02_Diriyah1/03_R02 Classification_Race.csv",
               "R03" =
                 "data/Season10/R03_Diriyah2/03_R03 Classification_Race.csv",
               "R04" = 
                 "data/Season10/R04_SaoPaulo/03_R04 Classification_Race.csv",
               "R05" = 
                 "data/Season10/R05_Tokyo/03_R05 Classification_Race.csv",
               "R06" =
                 "data/Season10/R06_Misano1/03_R06 Classification_Race.csv",
               "R07" = 
                 "data/Season10/R07_Misano2/03_R07 Classification_Race.csv",
               "R08" =
                 "data/Season10/R08_Monaco/03_R08 Classification_Race.csv",
               "R09" = 
                 "data/Season10/R09_Berlin1/03_R09 Classification_Race.csv",
               "R10" =
                 "data/Season10/R10_Berlin2/03_R10 Classification_Race.csv",
               "R11" = 
                  "data/Season10/R11_Shanghai1/03_R11 Classification_Race.csv",
               "R12" =
                 "data/Season10/R12_Shanghai2/03_R12 Classification_Race.csv",
               "R13" = 
                 "data/Season10/R13_Portland1/03_R13 Classification_Race.csv",
               "R14" =
                 "data/Season10/R14_Portland2/03_R14 Classification_Race.csv",
               "R15" = 
                 "data/Season10/R15_London1/03_R15 Classification_Race.csv",
               "R16" =
                 "data/Season10/R16_London2/03_R16 Classification_Race.csv"))
  colnames(result_read_list) <- c("path_result")
  
  return(result_read_list)
}

wdc_read_list_fct = function(){
  wdc_read_list <- data.frame(c("R01" =
                                     "data/Season10/R01_MexicoCity/Drivers Championship.csv",
                                   "R02" = 
                                  "data/Season10/R04_SaoPaulo/Drivers Championship.csv",
                                   "R03" =
                                  "data/Season10/R04_SaoPaulo/Drivers Championship.csv",
                                   "R04" = 
                                     "data/Season10/R04_SaoPaulo/Drivers Championship.csv",
                                   "R05" = 
                                     "data/Season10/R05_Tokyo/Drivers Championship.csv",
                                   "R06" =
                                     "data/Season10/R06_Misano1/Drivers Championship.csv",
                                   "R07" = 
                                     "data/Season10/R07_Misano2/Drivers Championship.csv",
                                   "R08" =
                                     "data/Season10/R08_Monaco/Drivers Championship.csv",
                                   "R09" = 
                                     "data/Season10/R09_Berlin1/Drivers Championship.csv",
                                   "R10" =
                                     "data/Season10/R10_Berlin2/Drivers Championship.csv",
                                   "R11" = 
                                     "data/Season10/R11_Shanghai1/Drivers Championship.csv",
                                   "R12" =
                                     "data/Season10/R12_Shanghai2/Drivers Championship.csv",
                                   "R13" = 
                                     "data/Season10/R13_Portland1/Drivers Championship.csv",
                                   "R14" =
                                     "data/Season10/R14_Portland2/Drivers Championship.csv",
                                   "R15" = 
                                     "data/Season10/R15_London1/Drivers Championship.csv",
                                   "R16" =
                                    "data/Season10/R16_London2/Drivers Championship.csv"))
  colnames(wdc_read_list) <- c("path_wdc")
  
  return(wdc_read_list)
}

sector_len_list_fct = function(){
  S1_len = c("R01" =  945, "R02" =  772, "R03" =  772, "R04" =  980, "R05" =  853,
             "R06" = 1210, "R07" = 1210, "R08" = 1089, "R09" =  680, "R10" =  680,
             "R11" = 1020, "R12" = 1020, "R13" = 1093, "R14" = 1093, "R15" =  680,
             "R16" =  680)
  S2_len = c("R01" = 1844, "R02" = 1486, "R03" = 1486, "R04" = 2160, "R05" = 1688,
             "R06" = 2210, "R07" = 2210, "R08" = 2419, "R09" = 1600, "R10" = 1600,
             "R11" = 2095, "R12" = 2095, "R13" = 1760, "R14" = 1760, "R15" = 1542,
             "R16" = 1542)
  S3_len = c("R01" = 2628, "R02" = 2495, "R03" = 2495, "R04" = 2642, "R05" = 2585,
             "R06" = 3382, "R07" = 3382, "R08" = 3337, "R09" = 2345, "R10" = 2345,
             "R11" = 3051, "R12" = 3051, "R13" = 3190, "R14" = 3190, "R15" = 2080,
             "R16" = 2080)
  sector_len_list <- data.frame(S1_len, S2_len, S3_len)
  
  return(sector_len_list)
}

race_data_fct = function(){
  laps_read_list <- laps_read_list_fct()
  result_read_list <- result_read_list_fct()
  sector_len_list <- sector_len_list_fct()
  wdc_read_list <- wdc_read_list_fct()
  
  round_number <- c("R01" = "R01_", "R02" = "R02_", "R03" = "R03_", "R04" = "R04_",
                    "R05" = "R05_", "R06" = "R06_", "R07" = "R07_", "R08" = "R08_",
                    "R09" = "R09_", "R10" = "R10_", "R11" = "R11_", "R12" = "R12_",
                    "R13" = "R13_", "R14" = "R14_", "R15" = "R15_", "R16" = "R16_")
  folder_name <- c("R01" = "R01_MexicoCity",
                   "R02" = "R02_Diriyah1",
                   "R03" = "R03_Diriyah2",
                   "R04" = "R04_SaoPaulo",
                   "R05" = "R05_Tokyo",
                   "R06" = "R06_Misano1",
                   "R07" = "R07_Misano2",
                   "R08" = "R08_Monaco",
                   "R09" = "R09_Berlin1",
                   "R10" = "R10_Berlin2",
                   "R11" = "R11_Shanghai1",
                   "R12" = "R12_Shanghai2",
                   "R13" = "R13_Portland1",
                   "R14" = "R14_Portland2",
                   "R15" = "R15_London1",
                   "R16" = "R16_London2")
  race_name <- c("R01" = "MexicoCity",
                 "R02" = "Diriyah1",
                 "R03" = "Diriyah2",
                 "R04" = "SaoPaulo",
                 "R05" = "Tokyo",
                 "R06" = "Misano1",
                 "R07" = "Misano2",
                 "R08" = "Monaco",
                 "R09" = "Berlin1",
                 "R10" = "Berlin2",
                 "R11" = "Shanghai1",
                 "R12" = "Shanghai2",
                 "R13" = "Portland1",
                 "R14" = "Portland2",
                 "R15" = "London1",
                 "R16" = "London2")
  
  race_data <- data.frame(round_number, folder_name, race_name,
                          laps_read_list, result_read_list, wdc_read_list)
  
  return(race_data)
}