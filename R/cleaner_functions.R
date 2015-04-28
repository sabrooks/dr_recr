
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


#' Model with daily energy and hourly production this keeps hourly granularity.
#' The alternative is keep the meters seperate and use daily data.
#'
#' @param prod.df A dataframe
#' @param energy.df A dataframe
#' model_d_h(prod.df, energy.df)
#' @export
model_d_h <- function(prod.df, energy.df){
  assert_that(is.data.frame(prod.df))
  assert_that(is.data.frame(energy.df))

  #loses all granularity less than an hour
  prod.hr <- bin_h(prod.df)%>%
    gather(meter, reading, -Time)%>%# combines all meters, could keep all separate
    group_by(Time)%>%
    summarise(production = sum(reading, na.rm = TRUE))%>%
    spread_hd()


  #energy.day <- bin_d(energy.df)

  #data.set <- inner_join(energy.day, prod.hr)# want energy data first for CI


}

#takes submetered production values and energy data
#returns daily data set with column for each sub mete
model_d_mt <- function(prod.df, energy.df){
  assert_that(is.data.frame(prod.df))
  assert_that(is.data.frame(energy.df))

  prod.dy <- prod.df%>%
    mutate(dy = cut(.$Time, breaks = "day"))%>%
    dplyr::select(-Time)%>%
    group_by(dy)%>%
    summarise_each(., funs(sum(., na.rm = TRUE)))%>%
    ungroup()%>%
    mutate(Time = ymd(dy))%>%
    dplyr::select(-dy)

  energy.day <- bin_d(energy.df)

  data.set <- inner_join(energy.day, prod.dy)

}

#Bin by hour - Need to figure out how to pass a column name as a variable
#Column Name of Time values assumed date time group
bin_h <- function(df, hrs = 1){

  df%>%
    mutate(bin = cut(.$Time, breaks = "hour"))%>%
    dplyr::select(-Time)%>%
    group_by(bin)%>%
    summarise_each(., funs(sum(., na.rm = TRUE)))%>%
    mutate(Time = bin)%>%
    select(-bin)
}

bin_d <- function(df, dys = 1){

  df%>%
    mutate(bin = (origin%--%Time)%/%days(dys))%>%
    group_by(bin)%>%
    summarise_each(., funs(sum(., na.rm = TRUE)))%>%
    mutate(Time = origin+ddays(bin))%>% #Clean up
    select(-bin)
}

