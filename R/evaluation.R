#' Reconciles DR Event
#'
#' @param event A DR event - output of dr_event()
#' eval_event(event)
#' @export
#'
eval_event <- function(event, prod, grace = minutes(5)){

  #Consolidate any sub meters------
  event$energy_data <- event$energy_data%>%
    gather(meter, energy, -time)%>%
    group_by(time)%>%
    summarise(energy = sum(energy, na.rm = TRUE))

  #Curtailment Interval
  curtailment_interval <- (int_start(event$event_interval)+grace)%--%
    int_end(event$event_interval)
  
  curtailment_day = floor_date(int_start(curtailment_interval),"day")

  #Baselines ------------

  #10 day average --------
  # This approach averages the curtailment period (not an hourly average)
  base_10d.df <- NULL
  for(i in 1:10){
    base_10d.df<- event$energy_data%>%
      filter(time %within% int_shift(curtailment_interval, -days(i)))%>%
      mutate(baseline.day = paste("day", i))%>%
      bind_rows(base_10d.df)
  }
  base_10d <- base_10d.df%>%
    summarise(energy = mean(energy, na.rm = TRUE))
  
#   10 day adjusted average.-------------- Place holder for more complicated
#   model adjustment.  Calculates a simple linear model from daily production and
#   energy.  The coefficient of the linear model is used to adjust the daily averages. 
#   
#   energy.prod.df <- event$energy_data%>%
#     mutate(time = as.factor(floor_date(time, "day")))%>%
#     group_by(time)%>%
#     summarise(energy = sum(energy, na.rm = TRUE))%>%
#     inner_join(prod%>%mutate(time = as.factor(time)),
#                by = time)%>%
#     mutate(time = ymd(time))
#   
#   #Simple linear model (forces intercept to 0)
#   lin.model <- lm(energy ~ 0 + flow, data = energy.prod.df)
#   adj.coef <- coef(lin.model)[1] # adjustment coefficient
#   
#   curtailment_day.df <- energy.prod.df%>%
#     dplyr::filter(time == curtailment_day)
#   
#   #Constructs a data.frame of daily adjustment factors
#   daily_adj <-base_10d.df%>%
#     mutate(day = floor_date(time, "day"))%>%
#     group_by(day)%>%
#     summarise(energy.adj = sum(energy, na.rm = TRUE))%>%
#     ungroup()%>%
#     mutate(time = as.factor(day))%>%
#     dplyr::select(-day)
#     left_join(energy.prod.df%>%mutate(time = as.factor(time)), by = time)%>%
#     mutate(time = ymd(time))
#     
#     energy.prod.df%>%
#     dplyr::filter(time %within% ((curtailment_day - days(10))%--%(curtailment_day - days(1))))%>%
#     mutate(adj = (curtailment_day.df$flow-flow)*adj.coef,
#            energy_adj = energy + adj)%>%
#     summarise(energy = mean(energy_adj, na.rm = TRUE))

  #2hr pre baseline ---------
  int_2hr <- (int_start(event$event_interval) - hours(2))%--%
    int_start(event$event_interval)

  base_2hr <- event$energy_data%>%
    filter(time %within% int_2hr)%>%
    dplyr::arrange(energy)%>%
    dplyr::slice(1)

  #30min pre baseline ---------
  int_30min <- (int_start(event$event_interval) - minutes(30))%--%
    int_start(event$event_interval)

  base_30min <- event$energy_data%>%
    filter(time %within% int_30min)%>%
    dplyr::arrange(energy)%>%
    dplyr::slice(1)

  #5 min pre baseline ---------
  # Possible, but needs granular data.
  int_5min <- (int_start(event$event_interval) - minutes(5))%--%
     int_start(event$event_interval)

   base_5min <- event$energy_data%>%
     filter(time %within% int_5min)%>%
     dplyr::arrange(energy)%>%
     dplyr::slice(1)

  #Curtailment max

  curtailment <- event$energy_data%>%
    filter(time %within% curtailment_interval)%>%
    dplyr::arrange(desc(energy))%>%
    dplyr::slice(1)
  
  results.df <- rbind(data_frame(method = "2 hour prior",
                             value = base_2hr$energy,
                             event.stamp = base_2hr$time),
                      data_frame(method = "30 minute prior",
                                 value = base_30min$energy,
                                 event.stamp = base_30min$time),
                      data_frame(method = "5 minute prior",
                                 value = base_5min$energy,
                                 event.stamp = base_5min$time),
                      data_frame(method = "10 day average",
                                 value = base_10d$energy,
                                 event.stamp = int_end(curtailment_interval)),
                      data_frame(method = "Curtailment",
                                 value = curtailment$energy,
                                 event.stamp = curtailment$time))
  
  results = list(df = results.df,
                 curt_int = curtailment_interval)
 
  plot <- plot_event(event$energy_data, results)

  return(plot)


}

plot_event <- function(energy.df, dr_results){

  #Generate an interval of the day containing the DR event
  event.start <- int_start(dr_results$curt_int)
  event.day.start <- ymd_h(paste(year(event.start),
                           month(event.start),
                           day(event.start),
                           0))
  event.day <- event.day.start %--% (event.start + days(1))
  
  #Pull methods data.frame
  dr_methods <- dr_results$df%>%
    group_by(value, event.stamp)%>%
    summarise(method = paste(method, collapse = ","))

  energy.df%>%
    dplyr::filter(time %within% event.day)%>%
    ggplot2::ggplot(aes(x = time, y = energy))+
    geom_area(alpha = 0.2)+
    geom_line()+
    geom_hline(data = dr_methods,
               aes(yintercept = value),
               color = "grey40",
               linetype = "dashed")+
    geom_point(data = dr_methods,
               aes(x = event.stamp, y = value),
               color = "grey40")+
    geom_text(data = dr_methods,
              aes(event.stamp, value,
                  label = paste(method), hjust = 0, vjust = -0.25))+
    scale_x_datetime(expand = c(0,0))+
    theme_bw()
}
