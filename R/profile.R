.onLoad <- function (libname, pkgname) { # nocov start
  assign(".geometa.iso", new.env(), envir= asNamespace(pkgname))
  
  #schema base URL
  .geometa.iso$languageUrl <- "http://www.loc.gov/standards/iso639-2/"
  .geometa.iso$schemaBaseUrl <- "http://www.isotc211.org/2005/resources"
  
  #fetch ISO codelists
  fetchISOCodelists()
  
  #fetch ISO schemas
  fetchISOSchemas()
  
} # nocov end