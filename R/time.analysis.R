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