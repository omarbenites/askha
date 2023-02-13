test_that("Test calculation of rcf index match number of rows", {
  
  library(askha)
  data("samdata")
  
  out <- rcf(samdata, "variety" ,"code_farmer", "number_tuber_by_sampling", "cu_community", "ADM3_Name", shorten = FALSE) 
  expect_equal(nrow(out), nrow(samdata))
  # a2 <- 
})

test_that("Test calculation of ocf index match number of rows", {
  
  library(askha)
  data("samdata")
  
  out <- ocf(dfr = samdata , vname="variety", hh="code_farmer", community = "cu_community", location = "ADM3_Name",shorten = FALSE)
  expect_equal(nrow(out), nrow(samdata))
  # a2 <- ocf(dfr = samdata , vname="variety", hh="code_farmer", community = "cu_community", location = "ADM3_Name",shorten = FALSE)
})



