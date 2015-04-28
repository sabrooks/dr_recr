#' Reconciles DR Event
#'
#' @param event A DR event - output of dr_event()
#' eval_event(event)
#' @export
#'
eval_event <- function(event, grace = minutes(5)){

  #Consolidate any sub meters------
  event$energy_data <- event$energy_data%>%
    gather(meter, energy, -time)%>%
    group_by(time)%>%
    summarise(energy = sum(energy, na.rm = TRUE))

  #Curtailment Interval
  curtailment_interval <- (int_start(event$event_interval)+grace)%--%
    int_end(event$event_interval)

  #Baselines ------------

  #10 day average --------
  # This approach averages the curtailment period (not an hourly average)
  base_10d <- NULL
  for(i in 1:10){
    base_10d<- event$energy_data%>%
      filter(time %within% int_shift(curtailment_interval, -days(i)))%>%
      mutate(baseline.day = paste("day", i))%>%
      bind_rows(base_10d)
  }
  base_10d <- base_10d%>%
    summarise(energy = mean(energy, na.rm = TRUE))


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


  results = list(base_10d = base_10d,
                 base_2hr = base_2hr,
                 base_30min = base_30min,
                 base_5min = base_5min,
                 event_interval = event$event_interval,
                 curtailment_interval = curtailment_interval,
                 curtailment_max = curtailment)

  plot <- plot_event(event$energy_data, results)

  return(plot)


}

plot_event <- function(energy.df, dr_results){

  #Generate an interval of the day containing the DR event
  event.start <- int_start(dr_results$event_interval)
  event.day.start <- ymd_h(paste(year(event.start),
                           month(event.start),
                           day(event.start),
                           0))
  event.day <- event.day.start %--% (event.start + days(1))

  energy.df%>%
    dplyr::filter(time %within% event.day)%>%
    ggplot2::ggplot(aes(x = time, y = energy))+
    geom_area(alpha = 0.2)+
    geom_line()+
    geom_hline(data = dr_results$curtailment_max,
               aes(yintercept = energy))+
    geom_hline(data = dr_results$base_2hr,
               aes(yintercept = energy))+
    geom_hline(data = dr_results$base_30min,
               aes(yintercept = energy))+
    geom_hline(data = dr_results$base_5min,
               aes(yintercept = energy))+
    geom_hline(data = dr_results$base_10d,
             aes(yintercept = energy))+
    scale_x_datetime(expand = c(0,0))+
    theme_bw()
}
