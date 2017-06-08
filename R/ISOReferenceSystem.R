#' ISOReferenceSystem
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO reference system
#' @return Object of \code{\link{R6Class}} for modelling an ISO ReferenceSystem
#' @format \code{\link{R6Class}} object.
#'
#' @field referenceSystemIdentifier
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, value)}}{
#'    This method is used to instantiate an ISOReferenceSystem
#'  }
#'  \item{\code{setReferenceSystemIdentifier(code, codeSpace)}}{
#'    Sets the reference system identifier
#'  }
#' }
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
  inherit = ISOMetadataElement,
  private = list(
    xmlElement = "MD_ReferenceSystem",
    xmlNamespacePrefix = "GMD"
  ),
  public = list(
    referenceSystemIdentifier = NULL,
    initialize = function(xml = NULL, prefix, code){
      super$initialize(
        xml = xml,
        element = private$xmlElement,
        namespace = getISOMetadataNamespace(private$xmlNamespacePrefix)
      )
    },
    
    #setReferenceSystemIdentifier
    setReferenceSystemIdentifier = function(identifier){
      if(!is(identifier, "ISOReferenceIdentifier")){
        stop("The argument should be an object of class 'ISOReferenceIdentifier")
      }
      self$referenceSystemIdentifier <- identifier
      
    }
  )                        
)