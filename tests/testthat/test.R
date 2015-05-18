library(drrecr)

test_that("dr_event",{
  
  dr.test <- dr_event(mdy_hm("3/19/2015 8:44"), hours(3), energy.test, Time)
  
  expect_equal(dr.test$event_interval,
               event.test$event_interval)
})
