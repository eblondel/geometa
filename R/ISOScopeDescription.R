#' ISOScopeDescription
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO browse graphic
#' @return Object of \code{\link{R6Class}} for modelling an ISO ScopeDescription
#' @format \code{\link{R6Class}} object.
#' 
#' @examples 
#'  md <- ISOScopeDescription$new()
#'  xml <- md$encode()
#' 
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOScopeDescription <- R6Class("ISOScopeDescription",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "MD_ScopeDescription",
    xmlNamespacePrefix = list(
      "19139" = "GMD",
      "19115-3" = "MCC"
    )
  ),
  public = list(
    #'@field attributes attributes [1..*]
    attributes = list(),
    #'@field features features [1..*]
    features = list(),
    #'@field featureInstances featureInstances [1..*]
    featureInstances = list(),
    #'@field attributeInstances attributeInstances [1..*]
    attributeInstances = list(),
    #'@field dataset dataset
    dataset = NULL,
    #'@field other other
    other = NULL,
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #'@description Adds attribute
    #'@param attribute attribute
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addAttribute = function(attribute){
      return(self$addListElement("attributes", attribute))
    },
    
    #'@description Deletes attribute
    #'@param attribute attribute
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delAttribute = function(attribute){
      return(self$delListElement("attributes", attribute))
    },
    
    #'@description Adds attribute instance
    #'@param attributeInstance attribute instance
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addAttributeInstance = function(attributeInstance){
      return(self$addListElement("attributeInstances", attributeInstance))
    },
    
    #'@description Deletes attribute instance
    #'@param attributeInstance attribute instance
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delAttributeInstance = function(attributeInstance){
      return(self$delListElement("attributeInstances", attributeInstance))
    },
    
    #'@description Adds feature instance
    #'@param featureInstance feature instance
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addFeatureInstance = function(featureInstance){
      return(self$addListElement("featureInstances", featureInstance))
    },
    
    #'@description Deletes feature instance
    #'@param featureInstance feature instance
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delFeatureInstance = function(featureInstance){
      return(self$delListElement("featureInstances", featureInstance))
    },
    
    #'@description Set dataset
    #'@param dataset dataset
    setDataset = function(dataset){
      self$dataset <- dataset
    },
    
    #'@description Set other
    #'@param other other
    setOther = function(other){
      self$other <- other
    }
  )                        
)