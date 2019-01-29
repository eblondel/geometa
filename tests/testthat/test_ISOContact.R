# test_ISOContact.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOContact.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOContact")

test_that("encoding",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
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
  res <- ISOOnlineResource$new()
  res$setLinkage("http://www.somewhereovertheweb.org")
  res$setName("somename")
  md$setOnlineResource(res)
  
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOContact$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})

test_that("encoding - i18n",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  md <- ISOContact$new()
  phone <- ISOTelephone$new()
  phone$setVoice(
    "myphonenumber",
    locales = list(
      EN = "myphonenumber in UK",
      FR = "mon numéro en France",
      ES = "mi número en España",
      AR = "رقم هاتفي في المملكة العربية السعودية",
      RU = "мой номер телефона в России",
      ZH = "我在中国的电话号码"
    )
  )
  phone$setFacsimile(
    "myfacsimile",
    locales = list(
      EN = "mi facsimile in UK",
      FR = "mon cax en France",
      ES = "mi fax en España",
      AR = "فاكس بلدي في المملكة العربية السعودية",
      RU = "мой факс в россии",
      ZH = "我在中国的传真"
    )
  )
  md$setPhone(phone)
  address <- ISOAddress$new()
  address$setDeliveryPoint(
    "theaddress",
    locales = list(
      EN = "address in UK",
      FR = "adresse en France",
      ES = "dirección en España",
      AR = "العنوان في المملكة العربية السعودية",
      RU = "адрес в россии",
      ZH = "在中国的地址"
    ))
  address$setCity(
    "thecity",
    locales = list(
      EN = "thecity",
      FR="ville",
      ES="Ciudad",
      AR="مدينة",
      RU="город",
      ZH="城市"
    ))
  address$setPostalCode(
    "111",
    locales=list(
      EN="111_UK",FR="111_FR",ES="111_ES",AR="111_AR",RU="111_RU",ZH="111_ZH"
    )
  )
  address$setCountry(
    "United Kingdom",
    locales=list(
      EN="United Kingdom", FR="France", ES="España", AR="网站名称", RU="Россия", ZH = "Китай"
    )
  )
  address$setEmail(
    "someone@theorg.org",
    locales = list(
      EN="someoneinuk@theorg.org",
      FR="someoneinfrance@theorg.org",
      ES="someoneinspain@theorg.org",
      AR="someoneinsaudiarabia@theorg.org",
      RU="someoneinrussia@theorg.org",
      ZH="someoneinchina@theorg.org"
    )
  )
  md$setAddress(address)
  
  res <- ISOOnlineResource$new()
  res$setLinkage("http://somelink")
  res$setName(
    "name",
    locales=list(
      EN="name of the website",
      FR="nom du site internet",
      ES="nombre del sitio web",
      AR="اسم الموقع",
      RU="название сайта",
      ZH="网站名称"
    ))
  res$setDescription(
    "description",
    locales = list(
      EN="description_EN",
      FR="description_FR",
      ES="description_ES",
      AR="description_AR",
      RU="description_RU",
      ZH="description_ZH"
    ))
  res$setProtocol(
    "protocol",
    locales=list(
      EN="protocol_EN",
      FR="protocol_FR",
      ES="protocol_ES",
      AR="protocol_AR",
      RU="protocol_RU",
      ZH="protocol_ZH"
    ))
  md$setOnlineResource(res)
  
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOContact$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})