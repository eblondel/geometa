#' ISOCoupledResource
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO CoupledResource
#' @return Object of \code{\link{R6Class}} for modelling an ISOCoupledResource
#' @format \code{\link{R6Class}} object.
#'
#' @field operationName [\code{\link{character}}] operation name
#' @field identifier [\code{\link{character}}] identifier
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an \code{\link{ISOCoupledResource}}
#'  }
#'  \item{\code{setOperationName(operationName, locales)}}{
#'    Set the operation name. Locale names can be specified 
#'    as \code{list} with the \code{locales} argument.
#'  }
#'  \item{\code{setIdentifier(identifier, locales)}}{
#'    Set the identifier. Locale names can be specified 
#'    as \code{list} with the \code{locales} argument.
#'  }
#' }
#' 
#' @examples
#'   md <- ISOCoupledResource$new()
#'   md$setOperationName("name")
#'   md$setIdentifier("identifier")
#'   xml <- md$encode()
#' 
#' @references 
#'   ISO 19119:2005 - Geographic information -- Services
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
      
      #+ operationName [1..1]: character
      operationName = NULL,
      #+ identifier [1..1]: character
      identifier = NULL,
      
      initialize = function(xml = NULL){
        super$initialize(xml = xml)
      },
      
      #setOperationName
      setOperationName = function(operationName, locales = NULL){
        self$operationName <- as.character(operationName)
        if(!is.null(locales)){
          self$operationName <- self$createLocalisedProperty(operationName, locales)
        }
      },
      
      #setIdentifier
      setIdentifier = function(identifier, locales = NULL){
        self$identifier <- as.character(identifier)
        if(!is.null(locales)){
          self$identifier <- self$createLocalisedProperty(identifier, locales)
        }
      }
    )                        
)
