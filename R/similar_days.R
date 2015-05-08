library(dplyr)
library(assertthat)
library(caret)
library(e1071)
library(lubridate)

library(tidyr)

#' Classifies similars days for DR baseline.
#'
#' 1. Daily summaries are created.
#' 2. Attempt to classify day of week "d_week" or workweek/weekend "work_end".
#' 3. If neither grouping produces a meaningful classification, "none" is returned.
#'
#'
#' @param df A dataframe (training data hourly or subhourly)
#' @param time.col the column name of column containing Posixct
#' similar_days(df, time.col)
#' @export
#'
similar_days <- function(df, time.col){
  assert_that(is.data.frame(df))
  #Does not check that df$time.col is time

  time.col <- lazyeval::lazy(time.col)

  #Assign  name "time" to time.col for easy manipulation
  df <- df%>%
    dplyr::rename_(time = time.col)%>%
    dplyr::mutate(yr = year(time),
           mn = month(time),
           dy = day(time),
           hr = hour(time))%>%
    dplyr::select(-time)%>%
    dplyr::group_by(yr, mn, dy, hr)%>%
    summarise_each(funs(sum(., na.rm = TRUE)))%>%
    tidyr::gather(meter, read, -yr, -mn, -dy,-hr)%>%
    mutate(meter = paste(meter, "_", hr))%>%
    dplyr::select(-hr)%>%
    spread(meter, read)%>%
    mutate(dofw = wday(ymd(paste(yr, mn, dy)), label = TRUE),
           day.type = as.factor(ifelse(dofw == "Sun"|dofw == "Sat",
                                       "weekend", "workweek")))

  #Day of week (dofw) analysis -----------------
  dofw_wo_time <- df%>%
    dplyr::select(-yr, -mn, -dy, -day.type)

  model_dofw <- train(dofw ~., data = dofw_wo_time,
                 'nb', trControl=trainControl(method='cv',number=10))

  results_dofw <-predict(model_dofw)

  #Day type (day.type - weekend, workweek) analysis------

  day.type_no.time <- df%>%
    dplyr::select(-yr, -mn, -dy, -dofw)

  model_day.type <- train(day.type ~., data = day.type_no.time,
                      'nb', trControl=trainControl(method='cv',number=10))

  results_day.type <-predict(model_day.type)

  #Exclude Sunday type (Sunday - True, False) analysis------
  sunday_no.time <- dofw_wo_time%>%
    dplyr::mutate(sunday = as.factor(ifelse(dofw == "Sun", TRUE, FALSE)))%>%
    dplyr::select(-dofw)

  model_sunday <- train(sunday ~., data = sunday_no.time,
                          'nb', trControl=trainControl(method='cv',number=10))

  results_sunday <-predict(model_sunday)

  #bind results----------------
  results <- bind_cols(data_frame(results_dofw),
                       data_frame(results_day.type),
                       data_frame(results_sunday),
                       df%>%
                          transmute(date.stamp = ymd(paste(yr, mn, dy)))%>%
                          mutate(day.type = wday(date.stamp, label = TRUE)))


  return(results)

}


#' Plots the results from similar days
#'
#' @param results from similar_days
#' @export
#'
similar_days_plot_results <- function(results){

  results%>%
    group_by(day.type, results_dofw)%>%
    summarise(tot = n())%>%
    ggvis(~ day.type, ~results_dofw)%>%
    layer_points(size  = ~ tot)%>%
    add_axis("x", title = "Actual Day")%>%
    add_axis("y", title = "Predicted Day")

  results%>%
    group_by(day.type, results_day.type)%>%
    summarise(tot = n())%>%
    ggvis(~ day.type, ~results_day.type)%>%
    layer_points(size  = ~ tot)%>%
    add_axis("x", title = "Actual Day")%>%
    add_axis("y", title = "Predicted Day Type")

  results%>%
    group_by(day.type, results_sunday)%>%
    summarise(tot = n())%>%
    ggvis(~ day.type, ~results_sunday)%>%
    layer_points(size  = ~ tot)%>%
    add_axis("x", title = "Actual Day")%>%
    add_axis("y", title = "Sunday")
}

#' Function to summarize a dataframe with a time column.
#'
#'
#' @param df A dataframe 
#' @param time.col the column name of column containing Posixct
#' interval(df, time.col)
#' @export
#'
interval <- function(df, time.col){
    
  assert_that(is.data.frame(df))
    #Does not check that df$time.col is time
    
  time.col <- lazyeval::lazy(time.col)
  
  df <- df%>%
    dplyr::rename_(time = time.col)
  
  time.span <- df%>%
    dplyr::summarize(min = min(time),
                     max = max(time))%>%
    dplyr::transmute(min%--%max)
    
  df <-df%>%
    dplyr::mutate(int = (lag(time)%--%time)/dminutes(1))%>%
    dplyr::count(int)
  
  results <- c(time.span = time.span,
               df = df)
  
  return(results)
  
}



