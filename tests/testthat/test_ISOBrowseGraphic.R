# test_ISOBrowseGraphic.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOBrowseGraphic.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOBrowseGraphic")

test_that("encoding",{
  
  #encoding
  md <- ISOBrowseGraphic$new(
    fileName = "http://wwww.somefile.org/png",
    fileDescription = "Map Overview",
    fileType = "image/png"
  )
  xml <- md$encode()
  expect_is(xml, "XMLNode")
})