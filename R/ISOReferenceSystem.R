#' ISOReferenceSystem
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO reference system
#' @return Object of \code{\link{R6Class}} for modelling an ISO ReferenceSystem
#' @format \code{\link{R6Class}} object.
#' 
#' @examples 
#'   md <- ISOReferenceSystem$new()
#'   rsId <- ISOReferenceIdentifier$new(code = "4326", codeSpace = "EPSG")
#'   md$setReferenceSystemIdentifier(rsId)
#'   xml <- md$encode()
#'   
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_MD_ReferenceSystem}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mrs/1.0/mrs/#element_MD_ReferenceSystem}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOReferenceSystem <- R6Class("ISOReferenceSystem",
  inherit = ISOAbstractReferenceSystem,
  private = list(
    xmlElement = "MD_ReferenceSystem",
    xmlNamespacePrefix = list(
      "19139" = "GMD",
      "19115-3" = "MRS"
    )
  ),
  public = list(
    #'@field referenceSystemIdentifier referenceSystemIdentifier
    referenceSystemIdentifier = NULL,
    #'@field referenceSystemType referenceSystemType (=> ISO 19115-3)
    referenceSystemType = NULL,
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #'@description Set reference system identifier
    #'@param identifier object of class \link{ISOReferenceIdentifier} (in 19139) or
    #'\link{ISOMetaIdentifier} (in 19115-3)
    setReferenceSystemIdentifier = function(identifier){
      switch(getMetadataStandard(),
        "19139" = {
          if(!is(identifier, "ISOReferenceIdentifier")){
            stop("The argument should be an object of class 'ISOReferenceIdentifier")
          }
        },
        "19115-3" = {
          if(!is(identifier, "ISOMetaIdentifier")){
            stop("The argument should be an object of class 'ISOMetaIdentifier")
          }
        }
      )
      
      self$referenceSystemIdentifier <- identifier
    },
    
    #'@description Set reference system type
    #'@param referenceSystemType object of class \link{ISOReferenceSystemType} or any \link{character}
    #' among values returned by \code{ISOReferenceSystemType$values()}
    setReferenceSystemType = function(referenceSystemType){
      self$stopIfMetadataStandardIsNot("19115-3")
      if(is(referenceSystemType, "character")){
        referenceSystemType <- ISOReferenceSystemType$new(value = referenceSystemType)
      }
      self$referenceSystemType <- referenceSystemType
    }
  )                        
)