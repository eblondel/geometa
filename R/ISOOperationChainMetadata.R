#' ISOOperationChainMetadata
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO OperationChainMetadata
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOOperationChainMetadata
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples
#'   md <- ISOOperationChainMetadata$new()
#'   xml <- md$encode()
#' 
#' @references 
#'  - ISO 19139 \url{https://schemas.isotc211.org/19119/-/srv/1.0/srv/#element_SV_OperationChainMetadata}
#'  
#'  - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/srv/2.0/srv/#element_SV_OperationChainMetadata}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOOperationChainMetadata <- R6Class("ISOOperationChainMetadata",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "SV_OperationChainMetadata",
    xmlNamespacePrefix = list(
      "19139" = "SRV",
      "19115-3" = "SRV"
    )
  ),
  public = list(
    
    #'@field name name [1..1]: character
    name = NULL,
    #'@field description description [1..1]: character
    description = NULL,
    #'@field operation operation [1..*]: ISOOperationMetadata
    operation = list(),
    
    #'@description Initializes object
    #'@param xml object of class \link[XML]{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #'@description Set name
    #'@param name name
    #'@param locales list of localized texts. Default is \code{NULL}
    setName = function(name, locales = NULL){
      self$name <- as.character(name)
      if(!is.null(locales)){
        self$name <- self$createLocalisedProperty(name, locales)
      }
    },
    
    #'@description Set description
    #'@param description description
    #'@param locales list of localized texts. Default is \code{NULL}
    setDescription = function(description, locales = NULL){
      self$description <- as.character(description)
      if(!is.null(locales)){
        self$description <- self$createLocalisedProperty(description, locales)
      }
    },
    
    #'@description Adds operation metadata
    #'@param operation object of class \link{ISOOperationMetadata}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addOperation = function(operation){
      if(!is(operation, "ISOOperationMetadata")){
        stop("The argument should be an object of class 'ISOOperationMetadata'")
      }
      return(self$addListElement("operation", operation))
    },
    
    #'@description Deletes operation metadata
    #'@param operation object of class \link{ISOOperationMetadata}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delOperation = function(operation){
      if(!is(operation, "ISOOperationMetadata")){
        stop("The argument should be an object of class 'ISOOperationMetadata'")
      }
      return(self$delListElement("operation", operation))
    }
    
    
  )                        
)
