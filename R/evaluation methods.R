#' Reconciles DR Event
#'
#' @param dr.event A DR event - output of dr_event()
#' eval_event(event)
#' @noRd
within.day.baseline <- function(dr.event, window){
  
  baseline.int <- (int_start(dr.event$event_interval) - window)%--%
    int_start(dr.event$event_interval)
  
  baseline<-dr.event$energy_data%>%
    filter(time %within% baseline.int)%>%
    dplyr::arrange(energy)%>%
    dplyr::slice(1)
    
  return(baseline)
}
