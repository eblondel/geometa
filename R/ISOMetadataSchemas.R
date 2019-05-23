#' @name registerISOMetadataSchema
#' @aliases registerISOMetadataSchema
#' @title registerISOMetadataSchema
#' @export
#' @description \code{registerISOMetadataSchema} allows to register a new schema
#' in \pkg{geometa}
#' 
#' @usage registerISOMetadataSchema(xsdFile)
#' 
#' @param xsdFile the schema XSD file
#' 
#' @examples             
#'   registerISOMetadataSchema(xsdFile = "http://www.isotc211.org/2005/gmd/gmd.xsd")
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#
registerISOMetadataSchema <- function(xsdFile){
  schemas <- tryCatch(
    XML::xmlParse(
      xsdFile, isSchema = TRUE, xinclude = TRUE,
      error = function (msg, code, domain, line, col, level, filename, class = "XMLError"){}
    )
  )
  .geometa.iso$schemas <- schemas
}

#'setISOMetadataSchemas
#'@export
setISOMetadataSchemas <- function(){
  packageStartupMessage("Loading ISO 19139 XML schemas...")
  schemaPath <- "extdata/schemas"
  namespace <- "gmd"
  defaultXsdFile <- system.file(paste(schemaPath, namespace, sep="/"), paste0(namespace,".xsd"),
                                package = "geometa", mustWork = TRUE)
  registerISOMetadataSchema(defaultXsdFile)
}

#' @name getISOMetadataSchemas
#' @aliases getISOMetadataSchemas
#' @title getISOMetadataSchemas
#' @export
#' @description \code{getISOMetadataSchemas} gets the schemas registered in \pkg{geometa}
#' 
#' @usage getISOMetadataSchemas()
#' 
#' @examples             
#'   getISOMetadataSchemas()
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#
getISOMetadataSchemas <- function(){
  return(.geometa.iso$schemas)
}