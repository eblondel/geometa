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
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOReferenceSystem <- R6Class("ISOReferenceSystem",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "MD_ReferenceSystem",
    xmlNamespacePrefix = "GMD"
  ),
  public = list(
    #'@field referenceSystemIdentifier referenceSystemIdentifier
    referenceSystemIdentifier = NULL,
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    #'@param prefix prefix
    #'@param code code
    initialize = function(xml = NULL, prefix, code){
      super$initialize(xml = xml)
    },
    
    #'@description Set reference system identifier
    #'@param identifier object of class \link{ISOReferenceIdentifier}
    setReferenceSystemIdentifier = function(identifier){
      if(!is(identifier, "ISOReferenceIdentifier")){
        stop("The argument should be an object of class 'ISOReferenceIdentifier")
      }
      self$referenceSystemIdentifier <- identifier
      
    }
  )                        
)