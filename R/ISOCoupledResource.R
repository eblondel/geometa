#' ISOCoupledResource
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO CoupledResource
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOCoupledResource
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples
#'   md <- ISOCoupledResource$new()
#'   md$setOperationName("name")
#'   md$setIdentifier("identifier")
#'   xml <- md$encode()
#' 
#' @references 
#'  - ISO 19139 \url{https://schemas.isotc211.org/19119/-/srv/1.0/srv/#element_SV_CoupledResource}
#'  
#'  - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/srv/2.0/srv/#element_SV_CoupledResource}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOCoupledResource <- R6Class("ISOCoupledResource",
    inherit = ISOAbstractObject,
    private = list(
      xmlElement = "SV_CoupledResource",
      xmlNamespacePrefix = "SRV"
    ),
    public = list(
      
      #'@field operationName operationName [1..1]: character
      operationName = NULL,
      #'@field identifier identifier [1..1]: character
      identifier = NULL,
      
      #'@description Initializes object
      #'@param xml object of class \link{XMLInternalNode-class}
      initialize = function(xml = NULL){
        super$initialize(xml = xml)
      },
      
      #'@description Set operation name
      #'@param operationName operation name
      #'@param locales a list of localized names. Default is \code{NULL}
      setOperationName = function(operationName, locales = NULL){
        self$operationName <- as.character(operationName)
        if(!is.null(locales)){
          self$operationName <- self$createLocalisedProperty(operationName, locales)
        }
      },
      
      #'@description Set identifier
      #'@param identifier identifier
      #'@param locales a list of localized identifiers. Default is \code{NULL}
      setIdentifier = function(identifier, locales = NULL){
        self$identifier <- as.character(identifier)
        if(!is.null(locales)){
          self$identifier <- self$createLocalisedProperty(identifier, locales)
        }
      }
    )                        
)
