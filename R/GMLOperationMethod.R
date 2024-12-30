#' GMLOperationMethod
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML operation method
#' @return Object of \code{\link[R6]{R6Class}} for modelling an GMLOperationMethod
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @references 
#'   ISO 19136:2007 Geographic Information -- Geographic Markup Language.
#'   http://www.iso.org/iso/iso_catalogue/catalogue_tc/catalogue_detail.htm?csnumber=32554 
#'   
#'   OGC Geography Markup Language. http://www.opengeospatial.org/standards/gml
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GMLOperationMethod <- R6Class("GMLOperationMethod",
  inherit = GMLDefinition,
  private = list(
    xmlElement = "OperationMethod",
    xmlNamespacePrefix = "GML"
  ),
  public = list(
   
    #'@field formulaCitation [\code{\link{ISOCitation}}]
    formulaCitation = NA,
    #'@field formula [\code{\link{GMLElement}}]
    formula = NULL,
    #'@field sourceDimensions [\code{\link{GMLElement}}]
    sourceDimensions = NULL,
    #'@field targetDimensions [\code{\link{GMLElement}}]
    targetDimensions = NULL,
    #' @field parameter [\code{list} of [\code{\link{GMLOperationParameter}} or \code{\link{GMLOperationParameterGroup}}]]
    parameter = list(),
    
    #'@description Sets the formula citation
    #'@param citation object of class \code{ISOCitation}
    setFormulaCitation = function(citation){
      if(!is(citation, "ISOCitation")){
        stop("The argument value should be an object of class 'ISOCitation'")
      }
      self$formulaCitation <- citation
    },
    
    #'@description Set formula
    #'@param formula formula, object of class \link{character}
    setFormula = function(formula){
      if(!is(formula, "character")) stop("Input object should be of class 'character'")
      formulaElem <- GMLElement$new(element = "formula")
      formulaElem$setValue(formula)
      self$formula <- formulaElem
    },
    
    #'@description Set source dimensions
    #'@param value value, object of class \link{integer}
    setSourceDimensions = function(value){
      if(!is(value, "integer")){
        value <- as.integer(value)
        if(is.na(value)){
          stop("The argument value should an object of class or coerceable to 'integer'")
        }
      }
      self$sourceDimensions <- GMLElement$create("sourceDimensions", value = value)
    },
    
    #'@description Set target dimensions
    #'@param value value, object of class \link{integer}
    setTargetDimensions = function(value){
      if(!is(value, "integer")){
        value <- as.integer(value)
        if(is.na(value)){
          stop("The argument value should an object of class or coerceable to 'integer'")
        }
      }
      self$targetDimensions <- GMLElement$create("targetDimensions", value = value)
    },
    
    #'@description Adds a parameter
    #'@param param object of class \link{GMLOperationParameter} or \link{GMLOperationParameterGroup}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addParameter = function(param){
      if(!inherits(param, "GMLAbstractGeneralOperationParameter")){
        stop("The argument value should be an object of class 'GMLOperationParameter' or 'GMLOperationParameterGroup'")
      }
      return(self$addListElement("parameter", param))
    },
    
    #'@description Deletes a parameter
    #'@param param object of class \link{GMLOperationParameter} or \link{GMLOperationParameterGroup}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delParameter = function(param){
      if(!inherits(param, "GMLAbstractGeneralOperationParameter")){
        stop("The argument value should be an object of class 'GMLOperationParameter' or 'GMLOperationParameterGroup'")
      }
      return(self$delListElement("parameter", param))
    }
    
  )
)
