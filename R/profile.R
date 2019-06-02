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
  
  #Auto serial GML Id variable
  assign(".geometa.gml", new.env(), envir = asNamespace(pkgname))
  .geometa.gml$serialId <- 1L
  
  #create a namespace for class constructor caching
  assign(".geometa.classes", new.env(), envir = asNamespace(pkgname))
  #list of cached class constructors
  cacheISOClasses()
  
  #mappings
  assign(".geometa.mappings", new.env(), envir = asNamespace(pkgname))
  #mapping rules
  mappings_file <- system.file("extdata/mappings", "geometa_mapping.csv", package = "geometa")
  registerMappings(utils::read.csv(mappings_file, stringsAsFactors = FALSE, na.strings = ""))
  
} # nocov end
