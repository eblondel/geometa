# test_ISOContact.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOContact.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOContact")

test_that("encoding",{
  
  #encoding
  md <- ISOContact$new()
  phone <- ISOTelephone$new()
  phone$setVoice("myphonenumber")
  phone$setFacsimile("myfacsimile")
  md$setPhone(phone)
  address <- ISOAddress$new()
  address$setDeliveryPoint("theaddress")
  address$setCity("thecity")
  address$setPostalCode("111")
  address$setCountry("France")
  address$setEmail("someone@theorg.org")
  md$setAddress(address)
  res <- ISOOnlineResource$new(linkage = "http://www.somewhereovertheweb.org", name = "somename")
  md$setOnlineResource(res)
  
  xml <- md$encode()
  expect_is(xml, "XMLNode")
})