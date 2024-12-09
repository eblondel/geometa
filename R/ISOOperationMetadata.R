#' ISOOperationMetadata
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO OperationMetadata
#' @return Object of \code{\link{R6Class}} for modelling an ISOOperationMetadata
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#'   md <- ISOOperationMetadata$new()
#'   xml <- md$encode()
#' 
#' @references 
#'  - ISO 19139 \link{https://schemas.isotc211.org/19119/-/srv/1.0/srv/#element_SV_OperationMetadata}
#'  
#'  - ISO 19115-3 \link{https://schemas.isotc211.org/19115/-3/srv/2.0/srv/#element_SV_OperationMetadata}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOOperationMetadata <- R6Class("ISOOperationMetadata",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "SV_OperationMetadata",
    xmlNamespacePrefix = "SRV"
  ),
  public = list(
    
    #'@field operationName operationName [1..1]: character
    operationName = NULL,
    #'@field DCP DCP [1..*]: ISODCPList
    DCP = list(),
    #'@field operationDescription operationDescription [0..1]: character
    operationDescription = NULL,
    #'@field invocationName invocationName [0..1]: character
    invocationName = NULL,
    #'@field parameters parameters [0..*]: ISOParameter
    parameters = list(),
    #'@field connectPoint connectPoint [1..*]: ISOOnlineResource
    connectPoint = list(),
    #'@field dependsOn dependsOn [0..*]: ISOOperationMetadata
    dependsOn = list(),
 
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #'@description Set operation name
    #'@param operationName operation name
    #'@param locales list of localized texts. Default is \code{NULL}
    setOperationName = function(operationName, locales = NULL){
      self$operationName <- as.character(operationName)
      if(!is.null(locales)){
        self$operationName <- self$createLocalisedProperty(operationName, locales)
      }
    },
    
    #'@description Adds DCP
    #'@param dcp object of class \link{ISODCPList} or any \link{character}
    #' among values returned by \code{ISODCPList$values()}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addDCP = function(dcp){
      if(!is(dcp, "ISODCPList")){
        dcp <- ISODCPList$new(value = dcp)
      }
      return(self$addListElement("DCP", dcp))
    },
    
    #'@description Deletes DCP
    #'@param dcp object of class \link{ISODCPList} or any \link{character}
    #' among values returned by \code{ISODCPList$values()}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delDCP = function(dcp){
      if(!is(dcp, "ISODCPList")){
        dcp <- ISODCPList$new(value = dcp)
      }
      return(self$delListElement("DCP", dcp))
    },
    
    #'@description Set operation description
    #'@param operationDescription operation description
    #'@param locales list of localized texts. Default is \code{NULL}
    setOperationDescription = function(operationDescription, locales = NULL){
      self$operationDescription <- as.character(operationDescription)
      if(!is.null(locales)){
        self$operationDescription <- self$createLocalisedProperty(operationDescription, locales)
      }
    },
    
    #'@description Set invocation name
    #'@param invocationName invocation name
    #'@param locales list of localized texts. Default is \code{NULL}
    setInvocationName = function(invocationName, locales = NULL){
      self$invocationName <- as.character(invocationName)
      if(!is.null(locales)){
        self$invocationName <- self$createLocalisedProperty(invocationName, locales)
      }
    },
    
    #'@description Adds parameter
    #'@param parameter object of class \link{ISOParameter}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addParameter = function(parameter){
      if(!is(parameter, "ISOParameter")){
        stop("The argument value should be an object of class 'ISOParameter'")
      }
      return(self$addListElement("parameters", parameter))
    },
    
    #'@description Deletes parameter
    #'@param parameter object of class \link{ISOParameter}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delParameter = function(parameter){
      if(!is(parameter, "ISOParameter")){
        stop("The argument value should be an object of class 'ISOParameter'")
      }
      return(self$delListElement("parameters", parameter))
    },
    
    #'@description Adds connection point
    #'@param connectPoint object of class \link{ISOOnlineResource}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addConnectPoint = function(connectPoint){
      if(!is(connectPoint, "ISOOnlineResource")){
        stop("The argument value should be an object of class 'ISOOnlineResource")
      }
      return(self$addListElement("connectPoint", connectPoint))
    },
    
    #'@description Deletes connection point
    #'@param connectPoint object of class \link{ISOOnlineResource}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delConnectPoint = function(connectPoint){
      if(!is(connectPoint, "ISOOnlineResource")){
        stop("The argument value should be an object of class 'ISOOnlineResource")
      }
      return(self$delListElement("connectPoint", connectPoint))
    },
    
    #'@description Adds operation metadata
    #'@param operationMetadata object of class \link{ISOOperationMetadata}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addDependentOperationMetadata = function(operationMetadata){
      if(!is(operationMetadata, "ISOOperationMetadata")){
        stop("The argument value should be an object of class 'ISOOperationMetadata")
      }
      return(self$addListElement("dependsOn", operationMetadata))
    },
    
    #'@description Deletes operation metadata
    #'@param operationMetadata object of class \link{ISOOperationMetadata}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delDependentOperationMetadata = function(operationMetadata){
      if(!is(operationMetadata, "ISOOperationMetadata")){
        stop("The argument value should be an object of class 'ISOOperationMetadata")
      }
      return(self$delListElement("dependsOn", operationMetadata))
    }
    
  )                        
)
