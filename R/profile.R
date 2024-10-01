.onLoad <- function (libname, pkgname) { # nocov start
  #options
  assign(".geometa.options", new.env(), envir= asNamespace(pkgname))
  .geometa.options$languageUrl = "http://www.loc.gov/standards/iso639-2/"
  .geometa.options$schemaBaseUrl = "http://www.isotc211.org/2005/resources"
  .geometa.options$codelistUrl <- "http://standards.iso.org/iso/19139/resources"
  .geometa.options$internalCodelists <- TRUE
  .geometa.options$object_comparator <- "print"
  
  #hidden objects
  assign(".geometa.iso", new.env(), envir= asNamespace(pkgname))
  assign(".geometa.iana", new.env(), envir= asNamespace(pkgname))
  
  #metadata schemas
  setISOMetadataSchemas(version = "19139")
  setISOMetadataSchemas(version = "19115-3")
  
  #metadata namespaces
  setISOMetadataNamespaces(version = "19139")
  setISOMetadataNamespaces(version = "19115-3")
  
  #set default metadata standard
  setMetadataStandard(version = "19139")
  
  #set IANA Mime Types
  setIANAMimeTypes()
  
  #Auto serial GML Id variable
  assign(".geometa.gml", new.env(), envir = asNamespace(pkgname))
  .geometa.gml$serialId <- 1L
  
  #mappings
  assign(".geometa.mappings", new.env(), envir = asNamespace(pkgname))
  #mapping rules
  mappings_file <- system.file("extdata/mappings", "geometa_mapping.csv", package = "geometa")
  registerMappings(utils::read.csv(mappings_file, stringsAsFactors = FALSE, na.strings = ""))
  
} # nocov end
