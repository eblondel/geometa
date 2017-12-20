.onLoad <- function (libname, pkgname) { # nocov start
  #options
  assign(".geometa.options", new.env(), envir= asNamespace(pkgname))
  .geometa.options$languageUrl = "http://www.loc.gov/standards/iso639-2/"
  .geometa.options$schemaBaseUrl = "http://www.isotc211.org/2005/resources"
  .geometa.options$codelists <- "geometa" 
  
  #hidden objects
  assign(".geometa.iso", new.env(), envir= asNamespace(pkgname))
  
  #set ISO metadata namespaces
  setISOMetadataNamespaces()
  
  #set ISO codelists
  setISOCodelists()
  
  #set ISO schemas
  setISOSchemas()
  
} # nocov end
