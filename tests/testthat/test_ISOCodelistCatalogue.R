# test_ISOCodelistCatalogue.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOCodelistCatalogue.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)
require(XML)

context("ISOCodelistCatalogue")

test_that("encoding",{
  testthat::skip_on_cran()
  #TODO performance!!!!!!
  system.time(cat <- ISOCodelistCatalogue$new(xml = XML::xmlRoot(XML::xmlParse("inst/extdata/codelists/gmxCodelists.xml"))))
  system.time(mlcat <- ISOCodelistCatalogue$new(xml = XML::xmlParse("inst/extdata/codelists/ML_gmxCodelists.xml")))
  system.time(newcat <- ISOCodelistCatalogue$new(xml = XML::xmlParse("inst/extdata/schemas/19115/resources/Codelists/cat/codelists.xml")))
})