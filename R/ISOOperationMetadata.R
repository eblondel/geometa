#' ISOOperationMetadata
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO OperationMetadata
#' @return Object of \code{\link{R6Class}} for modelling an ISOOperationMetadata
#' @format \code{\link{R6Class}} object.
#'
#' @field operationName [\code{\link{character}}] operation name
#' @field DCP [\code{\link{ISODCPList}}] DCP
#' @field operationDescription [\code{\link{character}}] operation description
#' @field invocationName [\code{\link{character}}] invocation name
#' @field parameters [\code{\link{ISOParameter}}] parameter(s)
#' @field connectPoint [\code{\link{ISOOnlineResource}}] online resources
#' @field dependsOn [\code{\link{ISOOperationMetadata}}] dependent operation metadata
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an \code{\link{ISOOperationMetadata}}
#'  }
#'  \item{\code{setOperationName(operationName, locales)}}{
#'    Set the operation name. Locale names can be specified as 
#'    \code{list} with the \code{locales} argument.
#'  }
#'  \item{\code{addDCP(dcp)}}{
#'    Add a DCP
#'  }
#'  \item{\code{delDCP(dcp)}}{
#'    Deletes a DCP
#'  }
#'  \item{\code{setOperationDescription(operationDescription, locales)}}{
#'    Set the operation description. Locale names can be specified as 
#'    \code{list} with the \code{locales} argument.
#'  }
#'  \item{\code{setInvocationName(invocationName, locales)}}{
#'    Set the invocation name. Locale names can be specified as 
#'    \code{list} with the \code{locales} argument.
#'  }
#'  \item{\code{addParameter(parameter)}}{
#'    Add a parameter, object of class \code{\link{ISOParameter}}
#'  }
#'  \item{\code{delParameter(parameter)}}{
#'    Deletes a parameter, object of class \code{\link{ISOParameter}}
#'  }
#'  \item{\code{addConnectPoint(connectPoint)}}{
#'    Add a connect point, object of class \code{\link{ISOOnlineResource}}
#'  }
#'  \item{\code{delConnectPoint(connectPoint)}}{
#'    Deletes a connect point, object of class \code{\link{ISOOnlineResource}}
#'  }
#'  \item{\code{addDependentOperationMetadata(operationMetadata)}}{
#'    Add dependent operation metadata, object of class \code{\link{ISOOperationMetadata}}
#'  }
#'  \item{\code{delDependentOperationMetadata(operationMetadata)}}{
#'    Deletes dependent operation metadata, object of class \code{\link{ISOOperationMetadata}}
#'  }
#' }
#' 
#' @examples
#'   md <- ISOOperationMetadata$new()
#'   xml <- md$encode()
#' 
#' @references 
#'   ISO 19119:2005 - Geographic information -- Services
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
    
    #+ operationName [1..1]: character
    operationName = NULL,
    #+ DCP [1..*]: ISODCPList
    DCP = list(),
    #+ operationDescription [0..1]: character
    operationDescription = NULL,
    #+ invocationName [0..1]: character
    invocationName = NULL,
    #+ parameters [0..*]: ISOParameter
    parameters = list(),
    #+ connectPoint [1..*]: ISOOnlineResource
    connectPoint = list(),
    #+ dependsOn [0..*]: ISOOperationMetadata
    dependsOn = list(),
 
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
    
    #addDCP
    addDCP = function(dcp){
      if(!is(dcp, "ISODCPList")){
        dcp <- ISODCPList$new(value = dcp)
      }
      return(self$addListElement("DCP", dcp))
    },
    
    #delDCP
    delDCP = function(dcp){
      if(!is(dcp, "ISODCPList")){
        dcp <- ISODCPList$new(value = dcp)
      }
      return(self$delListElement("DCP", dcp))
    },
    
    #setOperationDescription
    setOperationDescription = function(operationDescription, locales = NULL){
      self$operationDescription <- as.character(operationDescription)
      if(!is.null(locales)){
        self$operationDescription <- self$createLocalisedProperty(operationDescription, locales)
      }
    },
    
    #setInvocationName
    setInvocationName = function(invocationName, locales = NULL){
      self$invocationName <- as.character(invocationName)
      if(!is.null(locales)){
        self$invocationName <- self$createLocalisedProperty(invocationName, locales)
      }
    },
    
    #addParameter
    addParameter = function(parameter){
      if(!is(parameter, "ISOParameter")){
        stop("The argument value should be an object of class 'ISOParameter'")
      }
      return(self$addListElement("parameters", parameter))
    },
    
    #delParameter
    delParameter = function(parameter){
      if(!is(parameter, "ISOParameter")){
        stop("The argument value should be an object of class 'ISOParameter'")
      }
      return(self$delListElement("parameters", parameter))
    },
    
    #addConnectPoint
    addConnectPoint = function(connectPoint){
      if(!is(connectPoint, "ISOOnlineResource")){
        stop("The argument value should be an object of class 'ISOOnlineResource")
      }
      return(self$addListElement("connectPoint", connectPoint))
    },
    
    #delConnectPoint
    delConnectPoint = function(connectPoint){
      if(!is(connectPoint, "ISOOnlineResource")){
        stop("The argument value should be an object of class 'ISOOnlineResource")
      }
      return(self$delListElement("connectPoint", connectPoint))
    },
    
    #addDependentOperationMetadata
    addDependentOperationMetadata = function(operationMetadata){
      if(!is(operationMetadata, "ISOOperationMetadata")){
        stop("The argument value should be an object of class 'ISOOperationMetadata")
      }
      return(self$addListElement("dependsOn", operationMetadata))
    },
    
    #delDependentOperationMetadata
    delDependentOperationMetadata = function(operationMetadata){
      if(!is(operationMetadata, "ISOOperationMetadata")){
        stop("The argument value should be an object of class 'ISOOperationMetadata")
      }
      return(self$delListElement("dependsOn", operationMetadata))
    }
    
  )                        
)
