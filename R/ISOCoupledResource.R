#' ISOCoupledResource
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO CoupledResource
#' @return Object of \code{\link{R6Class}} for modelling an ISOCoupledResource
#' @format \code{\link{R6Class}} object.
#'
#' @field operationName
#' @field identifier
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOCoupledResource
#'  }
#'  \item{\code{setOperationName(operationName)}}{
#'    Set the operation name
#'  }
#'  \item{\code{setIdentifier(identifier)}}{
#'    Set the identifier
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
      setOperationName = function(operationName){
        self$operationName <- as.character(operationName)
      },
      
      #setIdentifier
      setIdentifier = function(identifier){
        self$identifier <- as.character(identifier)
      }
    )                        
)
