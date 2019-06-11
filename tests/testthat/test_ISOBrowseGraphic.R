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
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOBrowseGraphic$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})

test_that("encoding - i18n",{
  #encoding
  md <- ISOBrowseGraphic$new()
  md$setFileName(
    "file name",
    locales = list(
      EN = "file name",
      FR = "nom du fichier",
      ES = "nombre del fichero",
      AR = "اسم الملف",
      RU = "имя файла",
      ZH = "文件名"
    )
  )
  md$setFileDescription(
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
  md$setFileType(
    "file type",
     locales = list(
       EN = "file type",
       FR = "type de fichier",
       ES = "tipo de fichero",
       AR = "نوع الملف",
       RU = "тип файла",
       ZH = "文件类型"
     ))
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOBrowseGraphic$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})