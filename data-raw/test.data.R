short <-(mdy("3/19/2015")-days(11))%--%mdy("3/20/2015")

energy.test <-energy%>%filter(Time %within% short)
event.test <- devtools::use_data(energy.test, event.test, internal = TRUE, overwrite = TRUE)

#Saves data for internal use.
devtools::use_data(energy.test, event.test, internal = TRUE, overwrite = TRUE)