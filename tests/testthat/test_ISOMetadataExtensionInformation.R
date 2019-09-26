# test_ISOMetadataExtensionInformation.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOMetadataExtensionInformation.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOMetadataExtensionInformation")

test_that("encoding - i18n",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  elem <- ISOExtendedElementInformation$new()
  elem$setName(
    "name in English",
    locales = list(
      EN = "name in english",
      FR = "nom en français",
      ES = "Nombre en español",
      AR = "الاسم باللغة العربية",
      RU = "имя на русском",
      ZH = "中文名"
    ))
  elem$setShortName(
    "name in English",
    locales = list(
      EN = "name in english",
      FR = "nom en français",
      ES = "Nombre en español",
      AR = "الاسم باللغة العربية",
      RU = "имя на русском",
      ZH = "中文名"
    ))
  elem$setDomainCode(1L)
  elem$setDefinition(
    "definition",
    locales = list(
      EN = "definition",
      FR = "définition",
      ES = "definición",
      AR = "فريف",
      RU = "определение",
      ZH = "定义"
    ))
  elem$setObligation("mandatory")
  elem$setCondition(
    "condition",
    locales = list(
      EN = "condition",
      FR = "condition",
      ES = "condición",
      AR = "الشرط",
      RU = "состояние",
      ZH = "条件"
    ))
  elem$setDatatype("characterString")
  elem$setMaximumOccurrence("string")
  elem$setDomainValue("value")
  elem$addParentEntity("none")
  elem$setRule(
    "rule",
    locales = list(
      EN = "rule",
      FR = "règle",
      ES = "regla",
      AR = "حكم",
      RU = "правило",
      ZH = "规则"
    ))
  elem$addRationale(
    "rationale",
    locales = list(
      EN = "rationale",
      FR = "raison fondamentale",
      ES = "razón fundamental",
      AR = "المنطق",
      RU = "обоснование",
      ZH = "合理"
    ))
  
  #adding a source
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
  elem$addSource(rp)

  md <- ISOMetadataExtensionInformation$new()
  md$addElement(elem)
  
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOMetadataExtensionInformation$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})

test_that("encoding",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  elem <- ISOExtendedElementInformation$new()
  elem$setName("name")
  elem$setShortName("shortName")
  elem$setDomainCode(1L)
  elem$setDefinition("some definition")
  elem$setObligation("mandatory")
  elem$setCondition("no condition")
  elem$setDatatype("characterString")
  elem$setMaximumOccurrence("string")
  elem$setDomainValue("value")
  elem$addParentEntity("none")
  elem$setRule("rule")
  elem$addRationale("rationale")
  
  #adding a source
  rp <- ISOResponsibleParty$new()
  rp$setIndividualName("someone")
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
  res$setLinkage("http://www.somewhereovertheweb.org")
  res$setName("somename")
  contact$setOnlineResource(res)
  rp$setContactInfo(contact)
  elem$addSource(rp)
  
  md <- ISOMetadataExtensionInformation$new()
  md$addElement(elem)
  
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOMetadataExtensionInformation$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})