library(testthat)
library(geometa)
test_check("geometa", filter = "^(?!.*abstract)(?!.*gml)", perl = TRUE, ignore.case = TRUE)
