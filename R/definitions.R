#' Defines a DR Event
#'
#' @param start The DR event start time
#' @param dur The duration of the DR event
#' @param energy.data Data frame of DR event energy data
#' @param time Column name of the column containing time stamp
#' dr_event(start, dur, energy.data, time)
#' @export
#'
dr_event <- function(start, dur, energy.data, time){
  assert_that(is.time(start))
  assert_that(is.period(dur)|is.duration(dur))

  #Convert Time column name to "time" ------------
  time.col <- lazyeval::lazy(time)

  energy.data <- energy.data%>%
    dplyr::rename_(time = time.col)

  end <- start + dur
  event.interval <- start%--%end

  #Check to see if supplied data contains DR event-----
  data.interval <- min(energy.data$time)%--%max(energy.data$time)

  assert_that(event.interval%within%data.interval)

  event <- list(event_interval = event.interval, energy_data = energy.data)
  
  class(event) <- "DRevent"
  
  return(event)

}



