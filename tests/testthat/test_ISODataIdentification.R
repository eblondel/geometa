# test_ISODataIdentification.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISODataIdentification.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISODataIdentification")

test_that("encoding",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  md <- ISODataIdentification$new()
  md$setAbstract("abstract")
  md$setPurpose("purpose")
  md$setLanguage("eng")
  md$setCharacterSet("utf8")
  md$addTopicCategory("biota")
  md$addTopicCategory("oceans")
  
  #adding a point of contact
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
  md$addPointOfContact(rp)
  
  #citation
  ct <- ISOCitation$new()
  ct$setTitle("sometitle")
  d <- ISODate$new()
  d$setDate(ISOdate(2015, 1, 1, 1))
  d$setDateType("publication")
  ct$addDate(d)
  ct$setEdition("1.0")
  ct$setEditionDate(ISOdate(2015,1,1))
  ct$setIdentifier(ISOMetaIdentifier$new(code = "identifier"))
  ct$setPresentationForm("mapDigital")
  ct$setCitedResponsibleParty(rp)
  md$setCitation(ct)
  
  #graphic overview
  go <- ISOBrowseGraphic$new(
    fileName = "http://wwww.somefile.org/png",
    fileDescription = "Map Overview",
    fileType = "image/png"
  )
  md$setGraphicOverview(go)
  
  #maintenance information
  mi <- ISOMaintenanceInformation$new()
  mi$setMaintenanceFrequency("daily")
  md$setResourceMaintenance(mi)
  
  #adding legal constraints
  lc <- ISOLegalConstraints$new()
  lc$addUseLimitation("limitation1")
  lc$addUseLimitation("limitation2")
  lc$addUseLimitation("limitation3")
  lc$addAccessConstraint("copyright")
  lc$addAccessConstraint("license")
  lc$addUseConstraint("copyright")
  lc$addUseConstraint("license")
  expect_equal(length(lc$useLimitation), 3L)
  expect_equal(length(lc$accessConstraints), 2L)
  expect_equal(length(lc$useConstraints), 2L)
  md$setResourceConstraints(lc)
  
  #adding extent
  extent <- ISOExtent$new()
  bbox <- ISOGeographicBoundingBox$new(minx = -180, miny = -90, maxx = 180, maxy = 90)
  extent$setGeographicElement(bbox)
  md$setExtent(extent)
  
  #add keywords
  kwds <- ISOKeywords$new()
  kwds$addKeyword("keyword1")
  kwds$addKeyword("keyword2")
  kwds$setKeywordType("theme")
  th <- ISOCitation$new()
  th$setTitle("General")
  th$addDate(d)
  kwds$setThesaurusName(th)
  md$addKeywords(kwds)
  
  #supplementalInformation
  md$setSupplementalInformation("some additional information")
  
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISODataIdentification$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})

test_that("encoding - i18n",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  md <- ISODataIdentification$new()
  md$setAbstract(
    "abstract",
    locales = list(
      EN = "abstract",
      FR = "résumé",
      ES = "resumen",
      AR = "ملخص",
      RU = "резюме",
      ZH = "摘要"
    ))
  md$setPurpose(
    "purpose",
    locales = list(
      EN = "purpose",
      FR = "objectif",
      ES = "objetivo",
      AR = "غرض",
      RU = "цель",
      ZH = "目的"
    ))
  md$setLanguage("eng")
  md$setCharacterSet("utf8")
  md$addTopicCategory("biota")
  md$addTopicCategory("oceans")
  
  #adding a point of contact
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
  md$addPointOfContact(rp)
  
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
  ct$setIdentifier(ISOMetaIdentifier$new(code = "identifier"))
  ct$setPresentationForm("mapDigital")
  ct$setCitedResponsibleParty(rp)
  md$setCitation(ct)
  
  #graphic overview
  go <- ISOBrowseGraphic$new()
  go$setFileName("http://wwww.somefile.org/png")
  go$setFileDescription(
    "Map overview",
    locales = list(
      EN = "Map overview",
      FR = "Aperçu de carte",
      ES = "Vista general del mapa",
      AR = "نظرة عامة على الخريطة",
      RU = "Обзор карты",
      ZH = "地图概述"
    )
  )
  md$setGraphicOverview(go)
  
  #maintenance information
  mi <- ISOMaintenanceInformation$new()
  mi$setMaintenanceFrequency("daily")
  md$setResourceMaintenance(mi)
  
  #adding legal constraints
  lc <- ISOLegalConstraints$new()
  lc$addUseLimitation(
    "use limitation 1", 
    locales= list(
      EN = "use limitation 1",
      FR = "limitation d'utilisation 1",
      ES = "limitación de uso 1",
      AR = "الحد من الاستخدام 1",
      RU = "предел использования 1",
      ZH = "使用限制1"
    ))
  lc$addUseLimitation(
    "use limitation 2", 
    locales= list(
      EN = "use limitation 2",
      FR = "limitation d'utilisation 2",
      ES = "limitación de uso 2",
      AR = "2 الحد من الاستخدام ",
      RU = "предел использования 2",
      ZH = "使用限制2"
    ))
  lc$addAccessConstraint("copyright")
  lc$addAccessConstraint("license")
  lc$addUseConstraint("copyright")
  lc$addUseConstraint("license")
  expect_equal(length(lc$useLimitation), 2L)
  expect_equal(length(lc$accessConstraints), 2L)
  expect_equal(length(lc$useConstraints), 2L)
  md$setResourceConstraints(lc)
  
  #adding extent
  extent <- ISOExtent$new()
  bbox <- ISOGeographicBoundingBox$new(minx = -180, miny = -90, maxx = 180, maxy = 90)
  extent$setGeographicElement(bbox)
  md$setExtent(extent)
  
  #add keywords
  kwds <- ISOKeywords$new()
  kwds$addKeyword(
    "keyword1",
    locales = list(
      EN = "keyword 1", 
      FR = "mot-clé 1", 
      ES = "palabra clave 1",
      AR = "1 الكلمة",
      RU = "ключевое слово 1", 
      ZH = "关键词 1"
    ))
  kwds$addKeyword(
    "keyword1",
    locales = list(
      EN = "keyword 2", 
      FR = "mot-clé 2", 
      ES = "palabra clave 2",
      AR = "2 الكلمة",
      RU = "ключевое слово 2", 
      ZH = "关键词 2"
    ))
  kwds$setKeywordType("theme")
  th <- ISOCitation$new()
  th$setTitle(
    "General",
    locales =list(
      EN = "General",
      FR = "Général",
      ES = "General",
      AR = "جنرال لواء",
      RU = "генеральный",
      ZH = "一般"
    ))
  th$addDate(d)
  kwds$setThesaurusName(th)
  md$addKeywords(kwds)
  
  #supplementalInformation
  md$setSupplementalInformation(
    "additional information",
    locales = list(
      EN = "additional information",
      FR = "information additionnelle",
      ES = "información adicional",
      AR = "معلومة اضافية",
      RU = "Дополнительная информация",
      ZH = "附加信息"
    ))
  
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISODataIdentification$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})