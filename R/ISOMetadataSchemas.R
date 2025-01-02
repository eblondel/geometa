#' @name registerISOMetadataSchema
#' @aliases registerISOMetadataSchema
#' @title registerISOMetadataSchema
#' @export
#' @description \code{registerISOMetadataSchema} allows to register a new schema
#' in \pkg{geometa}
#' 
#' @usage registerISOMetadataSchema(version, xsdFile)
#' 
#' @param version the schema version
#' @param xsdFile the schema XSD file
#' 
#' @examples             
#'   registerISOMetadataSchema(version = "19139", xsdFile = "http://www.isotc211.org/2005/gmd/gmd.xsd")
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#
registerISOMetadataSchema <- function(version, xsdFile){
  schemas <- tryCatch(
    XML::xmlParse(
      xsdFile, isSchema = TRUE, xinclude = TRUE,
      error = function (msg, code, domain, line, col, level, filename, class = "XMLError"){}
    )
  )
  .geometa.iso$schemas[[version]] <- schemas
}

#' @name getISOMetadataSchemaFile
#' @aliases getISOMetadataSchemaFile
#' @title getISOMetadataSchemaFile
#' @export
#' @description \code{getISOMetadataSchemaFile} allows to get the schema file
#' in \pkg{geometa}
#' 
#' @usage getISOMetadataSchemaFile(version)
#' 
#' @param version the schema version
#' @return the internal path to the schema file
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#
getISOMetadataSchemaFile <- function(version = "19139"){
  available_versions <- c("19139","19115-3")
  if(!version %in% available_versions){
    errMsg <- sprintf("Version '%s' not among available schema versions (%s)",
                      version, paste0(available_versions, collapse = ","))
    stop(errMsg)
  }
  schemaPath <- "extdata/schemas"
  namespace <- switch(version,
                      "19139" = "19115/-1/gmd",
                      "19115-3" = "19115/-3/mdb/2.0" 
  )
  xsdFilename <- switch(version,
                        "19139" = "gmd.xsd",
                        "19115-3" = "mdb.xsd"
  )
  defaultXsdFile <- system.file(paste(schemaPath, namespace, sep="/"), xsdFilename, package = "geometa", mustWork = TRUE)
  return(defaultXsdFile)
}

#' @name setISOMetadataSchemas
#' @aliases setISOMetadataSchemas
#' @title setISOMetadataSchemas
#' @export
#' @description \code{setISOMetadataSchemas} register the schemas in \pkg{geometa}
#' 
#' @usage setISOMetadataSchemas(version)
#' 
#' @param version the schema version
#' 
#' @examples             
#'   setISOMetadataSchemas(version = "19115-3")
#'
#'@export
setISOMetadataSchemas <- function(version = "19139"){
  packageStartupMessage(sprintf("Loading ISO %s XML schemas...", version))
  registerISOMetadataSchema(version = version, getISOMetadataSchemaFile(version = version))
}

#' @name getISOMetadataSchemas
#' @aliases getISOMetadataSchemas
#' @title getISOMetadataSchemas
#' @export
#' @description \code{getISOMetadataSchemas} gets the schemas registered in \pkg{geometa}
#' 
#' @usage getISOMetadataSchemas(version)
#' 
#' @param version the schema version
#' 
#' @examples             
#'   getISOMetadataSchemas(version = "19115-3")
#' 
getISOMetadataSchemas <- function(version = "19139"){
  available_versions <- c("19139","19115-3")
  if(!version %in% available_versions){
    errMsg <- sprintf("Version '%s' not among available schema versions (%s)",
                      version, paste0(available_versions, collapse = ","))
    stop(errMsg)
  }
  return(.geometa.iso$schemas[[version]])
}


#' @name setMetadataStandard
#' @aliases setMetadataStandard
#' @title setMetadataStandard
#' @export
#' @description \code{setMetadataStandard} allows to set the standard to use for encoding/decoding in \pkg{geometa}.
#'  By default the standard "19139" will be used. Possible alternative value "19115-3"
#' 
#' @usage setMetadataStandard(version)
#' 
#' @param version the standard version
#' 
#' @examples             
#'   setMetadataStandard(version = "19115-3")
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#
setMetadataStandard <- function(version = "19139"){
  available_versions <- c("19139","19115-3")
  if(!version %in% available_versions){
    errMsg <- sprintf("Version '%s' not among available schema versions (%s)",
                      version, paste0(available_versions, collapse = ","))
    stop(errMsg)
  }
  .geometa.iso$version <- version
  #metadata namespaces
  setISOMetadataNamespaces(version = version)
  setISOCodelists(version = version)
}

#' @name getMetadataStandard
#' @aliases getMetadataStandard
#' @title getMetadataStandard
#' @export
#' @description \code{getMetadataStandard} allows to set the standard to use for encoding/decoding in \pkg{geometa}.
#' 
#' @usage getMetadataStandard()
#' 
#' @examples             
#'   getMetadataStandard()
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#
getMetadataStandard <- function(){
  return(.geometa.iso$version)
}

