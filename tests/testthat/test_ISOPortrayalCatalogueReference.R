# test_ISOPortrayalCatalogueReference.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOPortrayalCatalogueReference.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOPortrayalCatalogueReference")

test_that("encoding/decoding",{
  testthat::skip_on_cran()
  #encoding
  md =  ISOPortrayalCatalogueReference$new()
  
  #adding a point of contact to citation
  rp <- ISOResponsibleParty$new()
  rp$setIndividualName("John Who")
  rp$setOrganisationName("somewhere")
  rp$setPositionName("someposition")
  rp$setRole("pointOfContact")
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
  res$setLinkage("http://somelink")
  res$setName("somename")
  contact$setOnlineResource(res)
  rp$setContactInfo(contact)
  
  #citation
  ct <- ISOCitation$new()
  ct$setTitle("sometitle")
  d1 <- ISODate$new()
  d1$setDate(ISOdate(2015, 1, 1, 1))
  d1$setDateType("creation")
  ct$addDate(d1)
  d2 <- ISODate$new()
  d2$setDate(ISOdate(2015, 3, 31, 1))
  d2$setDateType("publication")
  ct$addDate(d2)
  ct$setEdition("1.0")
  ct$setEditionDate(as.Date(ISOdate(2015, 1, 1, 1)))
  ct$addIdentifier(ISOMetaIdentifier$new(code = "identifier"))
  ct$addPresentationForm("mapDigital")
  ct$addCitedResponsibleParty(rp)

  md$addCitation(ct)
  
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOPortrayalCatalogueReference$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})

test_that("encoding/decoding - i18n",{
  testthat::skip_on_cran()
  #encoding
  md = ISOPortrayalCatalogueReference$new()
  
  #adding a point of contact to citation
  rp <- ISOResponsibleParty$new()
  rp$setIndividualName(
    "someone",
    locales = list(
      EN = "name in english",
      FR = "nom en français",
      ES = "Nombre en español",
      AR = "الاسم باللغة العربية",
      RU = "имя на русском",
      ZH = "中文名"
    ))
  rp$setOrganisationName(
    "organization",
    locales = list(
      EN = "organization",
      FR = "organisation",
      ES = "organización",
      AR = "منظمة",
      RU = "организация",
      ZH = "组织"
    ))
  rp$setPositionName(
    "someposition",
    locales = list(
      EN = "my position",
      FR = "mon poste",
      ES = "mi posición",
      AR = "موقعي",
      RU = "моя позиция",
      ZH = "我的位置"
    )
  )
  rp$setRole("pointOfContact")
  contact <- ISOContact$new()
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
  contact$setPhone(phone)
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
      EN="United Kingdom", FR="France", ES="España", AR="العربية السعودية", RU="Россия", ZH = "网站名称"
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
  contact$setAddress(address)
  res <- ISOOnlineResource$new()
  res$setLinkage("http://www.somewhereovertheweb.org")
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
  contact$setOnlineResource(res)
  rp$setContactInfo(contact)
  
  #citation
  ct <- ISOCitation$new()
  ct$setTitle(
    "sometitle",
    locales = list(
      EN = "title",
      FR = "titre",
      ES = "título",
      AR = "لقبان",
      RU = "название",
      ZH = "标题"
    )
  )
  d <- ISODate$new()
  d$setDate(ISOdate(2015, 1, 1, 1))
  d$setDateType("publication")
  ct$addDate(d)
  ct$setEdition("1.0")
  ct$setEditionDate(ISOdate(2015,1,1))
  ct$addIdentifier(ISOMetaIdentifier$new(code = "identifier"))
  ct$addPresentationForm("mapDigital")
  ct$addCitedResponsibleParty(rp)
  
  md$addCitation(ct)
  
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOPortrayalCatalogueReference$new(xml = xml)
  xml2 <- md2$encode()
  #object identity
  expect_true(ISOAbstractObject$compare(md, md2))
  
})