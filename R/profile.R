.onLoad <- function (libname, pkgname) { # nocov start
  #options
  assign(".geometa.options", new.env(), envir= asNamespace(pkgname))
  .geometa.options$languageUrl = "http://www.loc.gov/standards/iso639-2/"
  .geometa.options$schemaBaseUrl = "http://www.isotc211.org/2005/resources"
  .geometa.options$codelistUrl <- paste(.geometa.options$schemaBaseUrl, "Codelist", sep="/")
  .geometa.options$internalCodelists <- TRUE
  
  #hidden objects
  assign(".geometa.iso", new.env(), envir= asNamespace(pkgname))
  
  #set ISO metadata namespaces
  setISOMetadataNamespaces()
  
  #set ISO schemas
  setISOMetadataSchemas()
  
  #set ISO codelists
  setISOCodelists()
  
} # nocov end
