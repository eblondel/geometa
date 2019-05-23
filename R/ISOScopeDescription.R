#' ISOScopeDescription
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO browse graphic
#' @return Object of \code{\link{R6Class}} for modelling an ISO ScopeDescription
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOScopeDescription
#'  }
#'  \item{\code{addAttribute(attribute)}}{
#'    Adds an attribute
#'  }
#'  \item{\code{delAttribute(attribute)}}{
#'    Deletes an attribute
#'  }
#'  \item{\code{addAttributeInstance(attributeInstance)}}{
#'    Adds an attribute instance
#'  }
#'  \item{\code{delAttributeInstance(attributeInstance)}}{
#'    Deletes an attribute instance
#'  }
#'  \item{\code{addFeature(feature)}}{
#'    Adds an feature
#'  }
#'  \item{\code{delFeature(feature)}}{
#'    Deletes an feature
#'  }
#'  \item{\code{addFeatureInstance(featureInstance)}}{
#'    Adds an feature instance
#'  }
#'  \item{\code{delFeatureInstance(featureInstance)}}{
#'    Deletes an feature instance
#'  }
#'  \item{\code{setDataset(dataset)}}{
#'    Set the dataset
#'  }
#'  \item{\code{setOther(other)}}{
#'    Set other description
#'  }
#' }
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
    xmlNamespacePrefix = "GMD"
  ),
  public = list(
    #attributes [1..*]
    attributes = list(),
    #features [1..*]
    features = list(),
    #featureInstances [1..*]
    featureInstances = list(),
    #attributeInstances [1..*]
    attributeInstances = list(),
    #dataset
    dataset = NULL,
    #other
    other = NULL,
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #addAttribute
    addAttribute = function(attribute){
      return(self$addListElement("attributes", attribute))
    },
    
    #delAttribute
    delAttribute = function(attribute){
      return(self$delListElement("attributes", attribute))
    },
    
    #addAttributeInstance
    addAttributeInstance = function(attributeInstance){
      return(self$addListElement("attributeInstances", attributeInstance))
    },
    
    #delAttributeInstance
    delAttributeInstance = function(attributeInstance){
      return(self$delListElement("attributeInstances", attributeInstance))
    },
    
    #addFeatureInstance
    addFeatureInstance = function(featureInstance){
      return(self$addListElement("featureInstances", featureInstance))
    },
    
    #delFeatureInstance
    delFeatureInstance = function(featureInstance){
      return(self$delListElement("featureInstances", featureInstance))
    },
    
    #setDataset
    setDataset = function(dataset){
      self$dataset <- dataset
    },
    
    #setOther
    setOther = function(other){
      self$other <- other
    }
  )                        
)