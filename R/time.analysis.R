#' Framework for time series analysis
#'
#' @param df DataFrame of time series data
#' @param time.col Column name of the column containing time stamp
#' times(df, time.col)
#' @export
times <- function(df, time.col){
  
  assert_that(is.data.frame(df))
  #Does not check that df$time.col is time  
  
  time.col <- lazyeval::lazy(time.col)
  
  df <- df%>%
    dplyr::rename_(time = time.col)
  
  ts <-xts(df%>%
             dplyr::select(-time),
           df$time) 
  
  return(ts)
}

#' Start of a function for completing a data frame
#'
#' @param df DataFrame of time series data
#' @param time.col Column name of the column containing time stamp
#' complete_ts(df, time.col)
#' @export
complete_ts <- function(df, time.col){
  
  assert_that(is.data.frame(df))
  #Does not check that df$time.col is time  
  
  time.col <- lazyeval::lazy(time.col)
  
  df <- df%>%
    dplyr::rename_(time = time.col)
  
  #currently defaults to five minutes
  df.start.end <- df%>%
    summarise(start = min(time),
              end = max(time))
  df.int <- df.start.end$start[1]%--%df.start.end$end[1]
  
  time.points <- int_start(df.int)+
    minutes(seq(0, df.int%/%minutes(1), by = 5 ))# 5 minutes
  
  #finds times missing values
  dplyr::anti_join(data_frame(time.points),
                   df%>%
                     filter(complete.cases(.))%>%
                     select(time), 
                   by = c("time.points" = "time"))
    
}