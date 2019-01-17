# test_INSPIREMetadataValidator.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for INSPIREMetadataValidator.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("INSPIREMetadataValidator")

require(XML)
xmlfile <- system.file("extdata/examples", "metadata.xml", package = "geometa")
xml <- xmlParse(xmlfile)
md <- ISOMetadata$new(xml = xml)

test_that("inspire - metadata validator",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  
  inspireValidator <- INSPIREMetadataValidator$new()
  inspireReport <- inspireValidator$getValidationReport(obj = md, raw = TRUE)
  expect_is(inspireReport, "list")
  expect_equal(names(inspireReport), c("url","creationDate","lastUpdateDate","validity", "raw"))
})

test_that("inspire - metadata validator 'encode' shortcut",{
  xml <- md$encode(inspire = TRUE)
})

test_that("inspire - metadata validator 'save' shortcut",{
  md$save("my-metadata.xml", inspire = TRUE)
})