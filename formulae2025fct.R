watermark_fct = function(){
  m <- readPNG("watermark_plot.PNG")
  wl <- system.file("extdata", "watermark_plot.PNG", package = "cowplot")
  wm <- matrix(rgb(m[,,1],m[,,2],m[,,3], m[,,4] * 0.05), nrow=dim(m)[1])
  
  return(wm)
}


laps_use_fct = function(laps, session_name = 'Race'){
  if(session_name == 'Race'){
    laps <- laps[c('DRIVER_NAME', 'HOUR', 'NUMBER',
                            'LAP_NUMBER', 'LAP_TIME', 'PIT_TIME',
                            'S1_LARGE', 'S2_LARGE', 'S3_LARGE')]
  }else{
    laps <- laps[c('DRIVER_NAME', 'HOUR', 'NUMBER', 'GROUP',
                   'LAP_NUMBER', 'LAP_TIME', 'PIT_TIME',
                   'S1_LARGE', 'S2_LARGE', 'S3_LARGE')]
  }
  
  laps <- laps %>% 
    rename_at('HOUR', ~'Time') %>% 
    rename_at('LAP_NUMBER', ~'Lap') %>% 
    rename_at('DRIVER_NAME', ~'Name') %>% 
    rename_at('LAP_TIME', ~'Laptime')
  
  laps$Time <-   strptime(laps$Time, format = "%H:%M:%OS")
  laps$LapTimeSeconds <- period_to_seconds(ms(laps$Laptime))
  laps$NUMBER <- factor(laps$NUMBER)
  
  if(session_name == 'Race'){
    laps <- laps %>% 
      filter(is.na(PIT_TIME)) %>% 
      group_by(Name) %>% 
      mutate(min_lap = min(LapTimeSeconds)) %>% 
      ungroup() %>% 
      group_by(Name) %>% 
      filter(LapTimeSeconds < min_lap*1.04) %>% 
      ungroup() %>% 
      filter(LapTimeSeconds < min(min_lap)*1.07) %>%
      arrange(min_lap)
  }else{
    laps <- laps %>% 
      filter(is.na(PIT_TIME)) %>% 
      group_by(Name) %>% 
      filter(LapTimeSeconds == min(LapTimeSeconds)) %>% 
      ungroup() %>% 
      group_by(GROUP) %>%
      mutate(min_pace = min(LapTimeSeconds), na.rm = TRUE)  %>%
      ungroup %>%
      arrange(min_pace)
  }
  
  return(laps)
}

result_fct = function(result){
  result <- result[c('POSITION', 'NUMBER', 'DRIVER_SHORTNAME',
                          'DRIVER_FIRSTNAME', 'DRIVER_SECONDNAME',
                          'TEAM')] %>% 
    rename_at('DRIVER_SHORTNAME', ~'Driver') %>% 
    rename_at('DRIVER_FIRSTNAME', ~'Firstname') %>% 
    rename_at('DRIVER_SECONDNAME', ~'Surname')
  
  result$NUMBER <- factor(result$NUMBER)
  
  result$team <- team_name[match(result$TEAM, names(team_name))]
  
  return(result)
}

laps_pace_fct = function(laps, result, session_name = 'Race'){
  laps_use <- laps_use_fct(laps, session_name)
  result <- result_fct(result)
  
  laps_pace <- full_join(result, laps_use, by = c('NUMBER')) %>% 
    arrange(POSITION, Driver)
  
  return(laps_pace)
}

sectors_fct = function(laps_pace, s1, s2, s3){
  sectors <- laps_pace %>% 
    pivot_longer(c('S1_LARGE', 'S2_LARGE', 'S3_LARGE'),
                 names_to = "sector",
                 values_to = "sectortimes")
  
  sectors$sectortimes <- period_to_seconds(ms(sectors$sectortimes))
  sectors$sector <- factor(sectors$sector)
  
  sector_length <- c("S1_LARGE" = s1,
                         "S2_LARGE" = (s2-s1),
                         "S3_LARGE" = (s3-s2))
  
  sectors$sector_length <- sector_length[match(sectors$sector,
                            names(sector_length))]
  
  return(sectors)
}

wdc_pts_fct = function(wdc_read){
  wdc_points <- wdc_read[c('Participant', 'Pos', 'SHORTNAME',
                                   'Points', 'NUMBER', 'TEAM')] %>% 
    rename_at('SHORTNAME', ~'Driver') %>%
    rename_at('Participant', ~'Name')
  
  wdc_points$team <- team_name[match(wdc_points$TEAM,
                                         names(team_name))]
  
  return(wdc_points)
}

avg_laptime_fct = function(laps_pace, group_unit, session_name ='Race'){
  if(session_name == "Race"){
    if(group_unit == "Driver"){
      avg_laptime <- laps_pace[c('Driver', 'LapTimeSeconds', 'team')] %>%
        group_by(Driver, team) %>%
        summarise(mean_pace = mean(LapTimeSeconds, na.rm = TRUE),
                  median_pace = median(LapTimeSeconds, na.rm = TRUE),
                  sd_pace = sd(LapTimeSeconds, na.rm = TRUE),
                  .groups = "keep") %>%
        arrange(mean_pace, median_pace)
    }else{ # Driver or Team, both race pace
      avg_laptime <- laps_pace[c('team', 'LapTimeSeconds')] %>%
        group_by(team) %>%
        summarise(mean_pace = mean(LapTimeSeconds, na.rm = TRUE),
                  median_pace = median(LapTimeSeconds, na.rm = TRUE),
                  sd_pace = sd(LapTimeSeconds, na.rm = TRUE),
                  .groups = "keep") %>%
        arrange(mean_pace, median_pace)
    }
  }else{ # Race or Quali
    if(group_unit == "Driver"){
      avg_laptime <- laps_pace[c('Driver', 'LapTimeSeconds',
                                 'team', 'GROUP')] %>%
        group_by(Driver, team) %>%
        summarise(mean_pace = mean(LapTimeSeconds, na.rm = TRUE),
                  GROUP,
                    .groups = "keep") %>%
      arrange(mean_pace)
    }else{ # Driver or Team, both quali pace
      avg_laptime <- laps_pace[c('team', 'LapTimeSeconds')] %>%
        group_by(team) %>%
        summarise(mean_pace = mean(LapTimeSeconds, na.rm = TRUE),
                  median_pace = median(LapTimeSeconds, na.rm = TRUE),
                  sd_pace = sd(LapTimeSeconds, na.rm = TRUE),
                  .groups = "keep") %>%
        arrange(mean_pace, median_pace)
    }
  }
  
  return(avg_laptime)
}

min_pace_fct = function(avg_laptime){
  min_pace <- min(avg_laptime$mean_pace, na.rm = TRUE)
  
  return(min_pace)
}

avg_lapgap_rel_fct = function(laps_pace, group_unit, session_name = 'Race'){
  avg_laptime <- avg_laptime_fct(laps_pace, group_unit, session_name)
  min_pace <- min_pace_fct(avg_laptime)
  
  if(session_name == 'Race'){
    if(group_unit == 'Driver'){
      avg_lapgap_rel <- avg_laptime %>%
        group_by(Driver, team) %>%
        summarise(pace_gap = mean_pace/min_pace,
                  .groups = "keep") %>%
        arrange(pace_gap)
    }else{
      avg_lapgap_rel <- avg_laptime %>%
        group_by(team) %>%
        summarise(pace_gap = mean_pace/min_pace,
                  .groups = "keep") %>%
        arrange(pace_gap)
    }
  }else{
    if(group_unit == 'Driver'){
      avg_lapgap_rel <- avg_laptime %>%
        group_by(GROUP) %>%
        mutate(min_group = min(mean_pace)) %>% 
        ungroup() %>% 
        group_by(Driver, team) %>% 
        summarise(pace_gap = mean_pace/min_group,
                  GROUP,
                  .groups = "keep") %>%
        arrange(pace_gap)
    }else{
      avg_lapgap_rel <- avg_laptime %>%
        group_by(team) %>%
        summarise(pace_gap = mean_pace/min_pace,
                  .groups = "keep") %>%
        arrange(pace_gap)
    }
  }
  
  return(avg_lapgap_rel)
}

avg_sectortime_t_fct = function(sectors){
  avg_sectortime_t <- sectors[c('team',
                                        'sector', 'sectortimes')] %>%
    group_by(team, sector) %>%
    summarise(mean_pace = mean(sectortimes, na.rm = TRUE),
              median_pace = median(sectortimes, na.rm = TRUE),
              .groups = "keep") %>%
    arrange(mean_pace, median_pace)

  avg_sectortime_tt <- avg_sectortime_t %>% 
    group_by(sector) %>% 
    summarise(team, sector, mean_pace, median_pace,
              R01_min_pace = min(mean_pace, na.rm = TRUE),
              .groups = "keep") %>% 
    ungroup()
  
  return(avg_sectortime_tt)
}

avg_sectorgap_rel_t_fct = function(sectors){
  avg_sectortime_t <- avg_sectortime_t_fct(sectors)
  
  avg_sectorgap_rel_t <- avg_sectortime_t %>%
    group_by(team, sector) %>%
    summarise(pace_gap = mean_pace/R01_min_pace,
              .groups = "keep") %>%
    arrange(pace_gap)
  
  return(avg_sectorgap_rel_t)
}



avg_lapplot_rel_fct = function(avg_lapgap_rel, race_name,
                               group_unit, session_name = 'Race'){
  # This function takes the dataset with all the gaps in average race pace,
  # and returns a bar plot of them. For that it also takes a few more variables,
  # the "race_name" is used for the plot title only, while "group_unit" and
  # "session_name" are deciders that are used for if-statements,
  # deciding on the specific grouping, columns used and title statements
  if(group_unit == 'Driver'){
    avg_lapplot <- ggplot(avg_lapgap_rel,
                          aes(fct_reorder(Driver, pace_gap),
                              (pace_gap - 1)*100,
                              fill = team))
  } else {
    avg_lapplot <- ggplot(avg_lapgap_rel,
                          aes(fct_reorder(team, pace_gap),
                              (pace_gap - 1)*100,
                              fill = team))
  }
  
  if(session_name == 'Race'){
    avg_lapplot <- avg_lapplot +
      labs(title = str_c("Race Pace Data - ", race_name, " ePrix 2025")) +
      xlab("Driver")
  } else{
    avg_lapplot <- avg_lapplot +
      labs(title = str_c("Quali Pace Data - ", race_name, " ePrix 2025")) +
      xlab("Team")
  }
  
  if(group_unit == 'Driver' && session_name == 'Quali'){
    avg_lapplot <- avg_lapplot +
      facet_wrap(~GROUP, ncol = 1, scales = "free")
  }
  
  avg_lapplot <- avg_lapplot +
    geom_col(show.legend = FALSE) +
    scale_fill_manual(values = team_color) +
    annotation_custom(rasterGrob(wm), -Inf, Inf, -Inf, Inf) +
    ylab("average gap to fastest (%)") +
    dark_theme_gray() +
    theme(legend.position = "bottom", text=element_text(size=8))
  
  return(avg_lapplot)
}

paceplot_fct = function(laps_pace, race_name){
  paceplot <- ggplot(laps_pace,
         aes(fct_reorder(Driver, POSITION),
             LapTimeSeconds,
             color = team)) +
    geom_boxplot() +
    scale_color_manual(values = team_color) +
    annotation_custom(rasterGrob(wm), -Inf, Inf, -Inf, Inf) +
    labs(title = str_c("Race Pace Data - ", race_name, " ePrix 2025")) +
    xlab("Driver") +
    ylab("Laptime (s)") +
    dark_theme_gray() +
    theme(legend.position = "none", text=element_text(size=8))
  
  return(paceplot)
}

violinplot_fct = function(laps_pace, race_name){
  paceplot <- ggplot(laps_pace,
                       aes(fct_reorder(Driver, POSITION),
                           LapTimeSeconds,
                           color = team)) +
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), scale = "width") +
    geom_jitter(size = 0.3, alpha = 0.3) +
    scale_color_manual(values = team_color) +
    annotation_custom(rasterGrob(wm), -Inf, Inf, -Inf, Inf) +
    labs(title = str_c("Race Pace Data - ", race_name, " ePrix 2025")) +
    xlab("Driver") +
    ylab("Laptime (s)") +
    dark_theme_gray() +
    theme(legend.position = "none", text=element_text(size=8))
  
  return(paceplot)
}

avg_sectorplot_t_fct = function(avg_sectorgap_rel_t, race_name){
  avg_sectorplot_t <- ggplot(avg_sectorgap_rel_t,
                                 aes(fct_reorder(team, pace_gap),
                                     (pace_gap - 1)*100,
                                     fill = team)) +
    geom_col(show.legend = FALSE) +
    scale_fill_manual(values = team_color) +
    annotation_custom(rasterGrob(wm), -Inf, Inf, -Inf, Inf) +
    facet_wrap(~sector, ncol = 1, scales = "free") +
    labs(title = str_c("Sector Pace Data - ", race_name, " ePrix 2025")) +
    xlab("Team") +
    ylab("average gap to fastest (%)") +
    dark_theme_gray() +
    theme(legend.position = "none", text=element_text(size=8))
  
  return(avg_sectorplot_t)
}

daytimeplot_fct = function(laps_pace, race_name){
  daytimeplot <- ggplot(laps_pace,
                          aes((as.numeric(Time)-as.numeric(Time[1]))/60,
                              LapTimeSeconds)) +
    geom_smooth(method = "lm", se=FALSE, fullrange=FALSE,
                size = 0.3, alpha = 0.5) +
    geom_point(aes(color = team), size = 0.3, alpha = 0.9) +
    scale_color_manual(values = team_color) +
    annotation_custom(rasterGrob(wm), -Inf, Inf, -Inf, Inf) +
    labs(title = str_c("Lap Time Data - ", race_name, " ePrix 2025")) +
    xlab("Minutes since Race Start") +
    ylab("Laptime (s)") +
    labs(color = "Teams") +
    dark_theme_gray() +
    theme(legend.position = "bottom",
          legend.text = element_text(size=5),
          legend.key.size = unit(0.2, 'cm'),
          text=element_text(size=8))
  
  return(daytimeplot)
}

wdcplot_fct = function(wdc_points, race_name){
  wdcplot <- ggplot(wdc_points,
                        aes(Points,
                            fct_reorder(Driver, Points),
                            fill = team)) +
    geom_col(show.legend = FALSE) +
    geom_text(aes(label = Points, x = Points + 4)) +
    scale_fill_manual(values = team_color) +
    annotation_custom(rasterGrob(wm), -Inf, Inf, -Inf, Inf) +
    labs(title = str_c("WDC standings - ", race_name, " ePrix 2025")) +
    xlab("Points") +
    ylab("Driver") +
    dark_theme_gray() +
    theme(legend.position = "none", text=element_text(size=8))
  
  return(wdcplot)
}

quali_dumbbellplot_fct = function(laps_pace, race_name){
  dumbbellplot <- ggplot(laps_pace,
                          aes(LapTimeSeconds,
                            fct_reorder(team, LapTimeSeconds),
                          color = GROUP)) +
    geom_line(aes(group = team)) +
    geom_point(size = 2, alpha = 0.9) +
    geom_text(aes(label = Driver, x = LapTimeSeconds + 0.05),
              size = 2) +
    annotation_custom(rasterGrob(wm), -Inf, Inf, -Inf, Inf) +
    labs(title = str_c("Quali Pace Data - ", race_name, " ePrix 2025")) +
    xlab("Laptime") +
    ylab("Team") +
    dark_theme_gray() +
    theme(legend.position = "bottom", text=element_text(size=8))
  
  return(dumbbellplot)
}



formulae_draw_fct = function(plot_name, round_folder, round_number,
                             file_name, race_name){
  # This function draws the given plot, with watermark and logo, as well as
  # saving it to memory, using the rest of the input to create the correct file name
  plot_full <- ggdraw(plot_name) +
    draw_image("watermark_plot.png",  x = 1, y = 1,
               hjust = .55, vjust =.55, scale = .09)
  ggsave(filename = str_c("plots/Season11/", round_folder, "/FE_",
                          round_number, file_name, "_", race_name, "_2025.png"),
         device = "png", plot = plot_full, width = 5.5, height = 3.4)
  plot_full
}
