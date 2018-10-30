library(testthat)
library(geometa)
test_check("geometa", filter = "^(?!.*abstract)", perl = TRUE, ignore.case = TRUE)
