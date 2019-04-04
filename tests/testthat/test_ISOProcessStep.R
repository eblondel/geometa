# test_ISOProcessStep.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOProcessStep.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOProcessStep")

test_that("encoding",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  md <- ISOProcessStep$new()
  md$setDescription("description")
  md$setRationale("rationale")
  md$setDateTime( ISOdate(2015, 1, 1, 23, 59, 59))
  rp <- ISOResponsibleParty$new()
  rp$setOrganisationName("test")
  rp$setIndividualName("someone")
  rp$setPositionName("position")
  contact <- ISOContact$new()
  phone <- ISOTelephone$new()
  phone$setVoice("myphonenumber")
  phone$setFacsimile("myfacsimile")
  contact$setPhone(phone)
  address <- ISOAddress$new()
  address$setDeliveryPoint("theaddress")
  address$setCity("thecity")
  address$setPostalCode("111")
  address$setCountry("France")
  address$setEmail("someone@theorg.org")
  contact$setAddress(address)
  res <- ISOOnlineResource$new()
  res$setLinkage("http://www.somewhereovertheweb.org")
  res$setName("somename")
  contact$setOnlineResource(res)
  rp$setContactInfo(contact)
  rp$setRole("pointOfContact")
  md$addProcessor(rp)
  xml <- md$encode()
  
  #decoding
  md2 <- ISOProcessStep$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})

test_that("encoding - with empty sources / hrefs",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  md <- ISOProcessStep$new()
  md$setDescription("description")
  md$addFieldAttrs("source", href = "http://somelink")
  xml <- md$encode()
  
  #decoding
  md2 <- ISOProcessStep$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})

test_that("encoding - i18n",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  md <- ISOProcessStep$new()
  md$setDescription(
    "description",
    locales = list(
      EN = "the description",
      FR = "la description",
      ES = "la descripción",
      AR = "الوصف",
      RU = "описание",
      ZH = "描述"
    )
  )
  md$setRationale(
    "rationale",
    locales = list(
      EN = "rationale",
      FR = "raison fondamentale",
      ES = "razón fundamental",
      AR = "المنطق",
      RU = "обоснование",
      ZH = "合理"
    ))
  md$setDateTime( ISOdate(2015, 1, 1, 23, 59, 59))
  rp <- ISOResponsibleParty$new()
  rp$setOrganisationName("test")
  rp$setIndividualName("someone")
  rp$setPositionName("position")
  contact <- ISOContact$new()
  phone <- ISOTelephone$new()
  phone$setVoice("myphonenumber")
  phone$setFacsimile("myfacsimile")
  contact$setPhone(phone)
  address <- ISOAddress$new()
  address$setDeliveryPoint("theaddress")
  address$setCity("thecity")
  address$setPostalCode("111")
  address$setCountry("France")
  address$setEmail("someone@theorg.org")
  contact$setAddress(address)
  res <- ISOOnlineResource$new()
  res$setLinkage("http://www.somewhereovertheweb.org")
  res$setName("somename")
  contact$setOnlineResource(res)
  rp$setContactInfo(contact)
  rp$setRole("pointOfContact")
  md$addProcessor(rp)
  xml <- md$encode()
  
  #decoding
  md2 <- ISOProcessStep$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})