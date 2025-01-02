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
  
  apiKey <- Sys.getenv("INSPIRE_API_KEY")
  if(nzchar(apiKey)){
    inspireValidator <- INSPIREMetadataValidator$new(url = Sys.getenv("INSPIRE_API_ENDPOINT"), apiKey = apiKey)
    if(!is.null(inspireValidator)){
      inspireReport <- inspireValidator$getValidationReport(obj = md, raw = TRUE)
      if(inspireValidator$running){
        expect_is(inspireReport, "list")
        expect_equal(names(inspireReport), c("Status","Completeness","Test Run ID","Log", "Ref URI", "HTML Report", "raw"))
      }else{
        expect_null(inspireReport)
      }
    }
  }else{
    inspireValidator <- INSPIREMetadataValidator$new(url = Sys.getenv("INSPIRE_API_ENDPOINT"), apiKey = apiKey)
    if(!is.null(inspireValidator)){
      expect_error(inspireValidator$getValidationReport(obj = md, raw = TRUE))
    }
  }
})

test_that("inspire - metadata validator 'encode' shortcut",{
  testthat::skip_on_cran()
  apiKey <- Sys.getenv("INSPIRE_API_KEY")
  if(nzchar(apiKey)){
    inspireValidator <- INSPIREMetadataValidator$new(url = Sys.getenv("INSPIRE_API_ENDPOINT"), apiKey = apiKey)
    if(!is.null(inspireValidator)) xml <- md$encode(inspire = TRUE, inspireValidator = inspireValidator)
  }
})

test_that("inspire - metadata validator 'save' shortcut",{
  testthat::skip_on_cran()
  apiKey <- Sys.getenv("INSPIRE_API_KEY")
  if(nzchar(apiKey)){
    inspireValidator <- INSPIREMetadataValidator$new(url = Sys.getenv("INSPIRE_API_ENDPOINT"), apiKey = apiKey)
    if(!is.null(inspireValidator)) md$save("my-metadata.xml", inspire = TRUE, inspireValidator = inspireValidator)
  }
})