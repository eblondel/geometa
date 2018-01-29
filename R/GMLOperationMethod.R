#' GMLOperationMethod
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML operation method
#' @return Object of \code{\link{R6Class}} for modelling an GMLOperationMethod
#' @format \code{\link{R6Class}} object.
#'
#' @field formulaCitation
#' @field formula
#' @field sourceDimensions
#' @field targetDimensions
#' @field parameter
#'
#' @section Inherited methods:
#' \describe{
#'   from \code{GMLDefinition}
#' }
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, defaults, id)}}{
#'    This method is used to instantiate a GML OperationMethod
#'  }
#'  \item{\code{setFormulaCitation(citation)}}{
#'    Sets the formula citation, object of class \code{ISOCitation}
#'  }
#'  \item{\code{setFormula(formula)}}{
#'    Sets a formula, object of class \code{character}
#'  }
#'  \item{\code{setSourceDimensions(value)}}{
#'    Sets the number of source dimensions, object of class \code{integer}
#'  }
#'  \item{\code{setTargetDimensions(value)}}{
#'    Sets the number of target dimensions, object of class \code{integer}
#'  }
#'  \item{\code{addParameter(parameter)}}{
#'    Adds a parameter or parameter group, object of class \code{GMLOperationParameter}
#'    or \code{GMLOperationParameterGroup}
#'  }
#'  \item{\code{delParameter(parameter)}}{
#'    Deletes a parameter or parameter group, object of class \code{GMLOperationParameter}
#'    or \code{GMLOperationParameterGroupo}
#'  }
#' }
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
   
    #+ formulaCitation [1..1]: ISOCitation, or use forula
    formulaCitation = NA,
    #+ formula [1..1]: character
    formula = NULL,
    #+ sourceDimensions [0..1]: integer
    sourceDimensions = NULL,
    #+ targetDimensions [0..1]: integer
    targetDimensions = NULL,
    #+ parameter  [0..*]: GMLOperationParameter or GMLOperationParameterGroup
    parameter = list(),
    
    #setFormulaCitation
    setFormulaCitation = function(citation){
      if(!is(citation, "ISOCitation")){
        stop("The argument value should be an object of class 'ISOCitation'")
      }
      self$formulaCitation <- citation
    },
    
    #setFormula
    setFormula = function(formula){
      if(!is(formula, "character")) stop("Input object should be of class 'character'")
      formulaElem <- GMLElement$new(element = "formula")
      formulaElem$setValue(formula)
      self$formula <- formulaElem
    },
    
    #setSourceDimensions
    setSourceDimensions = function(value){
      if(!is(value, "integer")){
        value <- as.integer(value)
        if(is.na(value)){
          stop("The argument value should an object of class or coerceable to 'integer'")
        }
      }
      self$sourceDimensions <- GMLElement$create("sourceDimensions", value = value)
    },
    
    #setTargetDimensions
    setTargetDimensions = function(value){
      if(!is(value, "integer")){
        value <- as.integer(value)
        if(is.na(value)){
          stop("The argument value should an object of class or coerceable to 'integer'")
        }
      }
      self$targetDimensions <- GMLElement$create("targetDimensions", value = value)
    },
    
    #addParameter
    addParameter = function(param){
      if(!inherits(param, "GMLAbstractGeneralOperationParameter")){
        stop("The argument value should be an object of class 'GMLOperationParameter' or 'GMLOperationParameterGroup'")
      }
      return(self$addListElement("parameter", param))
    },
    
    #delParameter
    delParameter = function(param){
      if(!inherits(param, "GMLAbstractGeneralOperationParameter")){
        stop("The argument value should be an object of class 'GMLOperationParameter' or 'GMLOperationParameterGroup'")
      }
      return(self$delListElement("parameter", param))
    }
    
  )
)